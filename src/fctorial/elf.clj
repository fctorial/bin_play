(ns fctorial.elf
  (:require [parse_struct.core :refer :all]
            [parse_struct.common_types :refer :all]
            [parse_struct.deserialize :refer [_deserialize]]
            [clojure.pprint :refer [pprint]]
            [fctorial.utils :refer :all]
            [fctorial.data :refer :all]
            [fctorial.types :refer :all])
  (:import (clojure.lang ROVec MMap)))

(defn sech->blob [bs sech]
  (ROVec. bs (sech :offset) (+ (sech :offset)
                               (sech :size))))

(defn make_strtab_reader [bs header]
  (if (nil? header)
    (fn [loc])
    (let [blob (sech->blob bs header)]
      (fn [loc]
        (new String (byte-array
                      (take-while #(not= % (byte 0))
                                  (ROVec. blob loc))))))))

(declare header)
(declare sections)
(declare symbols)
(declare segments)

(defmethod print-method clojure.lang.Atom [_ ^java.io.Writer w]
  (.write w "#atom"))

(defn parse-elf [bs]
  (let [magic_t {:type       :struct
                 :definition [[:ident {:type  :string
                                       :bytes 4}]
                              [:class (assoc i8 :adapter {1 :32 2 :64})]
                              [:data (assoc i8 :adapter {1 :LE 2 :BE})]
                              [:version i8]]}
        magic (into MMap/EMPTY (deserialize magic_t bs))
        _ (if (not= (magic :ident) "ELF")
            (throw (new IllegalArgumentException "[DEL]ELF magic not found in the given data")))
        PLATFORM_KEY [(magic :data) (magic :class)]

        HEADER (into magic (deserialize
                             (make_header_t PLATFORM_KEY)
                             (ROVec. bs 16)))

        secheader_t (make_secheader_t PLATFORM_KEY)
        _ (if (not= (type-size secheader_t) (HEADER :shentsize))
            (throw (new IllegalArgumentException (str "section header size in ELF header: " (HEADER :shentsize) "doesn't match header size defined by ELF spec for this architecture: " (type-size secheader_t)))))
        secheaders_raw (deserialize {:type    :array
                                     :len     (HEADER :shnum)
                                     :element (assoc secheader_t
                                                :adapter #(into MMap/EMPTY %))
                                     :adapter vec}
                                    (ROVec. bs (HEADER :shoff)))
        secname_reader (make_strtab_reader bs (get secheaders_raw (HEADER :shstrndx)))
        sec_refs (mapv (fn [_] (atom nil)) secheaders_raw)
        SECTIONS (mapv
                   (fn [[header idx]]
                     (-> header
                         (update :name secname_reader)
                         (update :link (fn [lidx]
                                         (if (zero? lidx)
                                           nil
                                           (fn [] @(sec_refs lidx)))))
                         (assoc :blob (fn []
                                        (if (= (header :type) :NOBITS)
                                          nil
                                          (sech->blob bs header))))
                         (assoc :index idx)))
                   (zip-colls secheaders_raw (range)))
        _ (doseq [[ref sec] (zip-colls sec_refs SECTIONS)]
            (reset! ref sec))

        sym_sec_header (first (filter #(= :SectionType/SYMTAB (% :type)) SECTIONS))
        SYMBOLS (if (nil? sym_sec_header)
                  []
                  (let [symbols_raw (deserialize {:type    :array
                                                  :len     (/ (sym_sec_header :size)
                                                              (sym_sec_header :entsize))
                                                  :element (assoc (make_sym_t PLATFORM_KEY)
                                                             :adapter #(into MMap/EMPTY %))}
                                                 ((sym_sec_header :blob)))
                        symname_reader (make_strtab_reader bs ((sym_sec_header :link)))]
                    (mapv
                      (fn [[sym idx]]
                        (-> sym
                            (update :name symname_reader)
                            (update :shndx (fn [sidx]
                                             (if (zero? sidx)
                                               nil
                                               (fn [] @(sec_refs sidx)))))
                            (assoc :index idx)))
                      (zip-colls symbols_raw (range)))))

        SEGMENTS (if (zero? (HEADER :shoff))
                   []
                   (let [phdr_t (make_phdr_t PLATFORM_KEY)]
                     (deserialize
                       {:type    :array
                        :len     (HEADER :phnum)
                        :element (assoc phdr_t
                                   :adapter #(let [seg (into MMap/EMPTY %)]
                                               (assoc seg :blob (fn [] (ROVec. bs (seg :offset) (+ (seg :offset)
                                                                                                   (seg :filesz)))))))
                        :adapter vec}
                       (ROVec. bs (HEADER :phoff)))))]
    (def header HEADER)
    (def sections SECTIONS)
    (def symbols SYMBOLS)
    (def segments SEGMENTS)
    {:header   HEADER
     :sections SECTIONS
     :symbols  SYMBOLS
     :segments SEGMENTS}))
(def result (parse-elf exec))
;
;(def prog_header {:type       :struct
;                  :definition [[:type ElfWord]
;                               [:off ElfOff]
;                               [:vaddr ElfAddr]
;                               [:paddr ElfAddr]
;                               [:filesz ElfWord]
;                               [:memsz ElfWord]
;                               [:flags (assoc ElfWord :adapter to-flags)]
;                               [:align ElfWord]]})
;
;(def ph (deserialize {:type    :array
;                      :adapter vec
;                      :len     (header :phnum)
;                      :element prog_header}
;                     (clojure.lang.ROVec. data (header :phoff))))
;
;(def sym_entry {:type       :struct
;                :definition [[:name (assoc ElfWord :adapter
;                                                   (fn [loc]
;                                                     (new String (byte-array
;                                                                   (take-while #(not= % (byte 0))
;                                                                               (clojure.lang.ROVec. obj_symnames_blob loc))))))]
;                             [:info i8]
;                             (padding 1)
;                             [:shndx (assoc ElfHalf :adapter
;                                                    (fn [idx]
;                                                      [(get-in sechs [(int idx) :sh_name]) idx]))]
;                             [:value ElfAddr]
;                             [:size ElfXword]]})
;
;(def sech_symtbl (first (filter #(= :SYMTAB (% :sh_type)) sechs)))
;
;(def symtbl (deserialize
;              {:type    :array
;               :len     (/ (sech_symtbl :sh_size)
;                           (sech_symtbl :sh_entsize))
;               :element sym_entry
;               :adapter vec}
;              (sech->blob data sech_symtbl)))
;
;(def exec_header (deserialize elf_header exec))
