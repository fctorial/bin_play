(ns fctorial.elf
  (:require [parse_struct.core :refer :all]
            [parse_struct.common_types :refer :all]
            [parse_struct.deserialize :refer [_deserialize]]
            [clojure.pprint :refer [pprint]]
            [fctorial.utils :refer :all]
            [fctorial.data :refer :all]
            [fctorial.types :refer :all])
  (:import ROVec
           clojure.lang.MMap))

(defmethod _deserialize :struct
  [{definition :definition} data]
  (first (reduce
           (fn [[res data_left] [name spec]]
             (let [size (type-size spec)
                   [curr_chunk next_data_left] (split-n size data_left)
                   val (deserialize spec curr_chunk)]
               [(if (= (spec :type) :padding)
                  res
                  (conj res [name val]))
                next_data_left]))
           [[] data]
           definition)))


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

(defn parse-elf [bs]
  (let [magic_t {:type       :struct
                 :definition [[:ident {:type  :string
                                       :bytes 4}]
                              [:class (assoc i8 :adapter {1 :32 2 :64})]
                              [:data (assoc i8 :adapter {1 :LE 2 :BE})]
                              [:version i8]]}
        magic (into MMap/EMPTY (deserialize magic_t bs))
        _ (if (not= (magic :ident) "ELF")
            (throw (new IllegalArgumentException "Invalid file")))
        PLATFORM_KEY [(magic :data) (magic :class)]
        header_t (make_header_t PLATFORM_KEY)
        HEADER (into magic (deserialize header_t (ROVec. bs 16)))

        secheader_t (make_secheader_t PLATFORM_KEY)
        secheaders_raw (deserialize {:type    :array
                                     :len     (HEADER :shnum)
                                     :element (assoc secheader_t
                                                :adapter #(into MMap/EMPTY %))
                                     :adapter vec}
                                    (ROVec. bs (HEADER :shoff)))
        secname_reader (make_strtab_reader bs (nth secheaders_raw (HEADER :shstrndx)))
        SECTIONS (mapv
                   (fn [header]
                     (-> header
                         (update :name secname_reader)
                         (assoc :get_blob (fn []
                                            (if (= (header :type) :NOBITS)
                                              nil
                                              (sech->blob bs header))))))
                   secheaders_raw)

        symname_reader (make_strtab_reader bs (first (filter #(= (% :name) ".strtab")
                                                             SECTIONS)))
        sym_sec (first (filter #(= :SYMTAB (% :type)) SECTIONS))
        symbols_raw (let [sym_sec (first (filter #(= :SYMTAB (% :type)) SECTIONS))]
                      (deserialize {:type    :array
                                    :len     (/ (sym_sec :size)
                                                (sym_sec :entsize))
                                    :element (assoc (make_sym_t PLATFORM_KEY)
                                               :adapter #(into MMap/EMPTY %))}
                                   ((sym_sec :get_blob))))
        SYMBOLS (mapv
                  (fn [sym]
                    (update sym :name symname_reader))
                  symbols_raw)]
    {:header HEADER
     :sections SECTIONS
     :symbols SYMBOLS}
    SYMBOLS))
(pprint (parse-elf sm))
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
