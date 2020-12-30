(ns fctorial.elf
  (:require [parse_struct.core :refer :all]
            [parse_struct.common_types :refer :all]
            [parse_struct.deserialize :refer [_deserialize]]
            [fctorial.utils :refer :all]
            [fctorial.data :refer :all])
  (:import ROVec))

#_(defmethod _deserialize :struct
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

(def data mb)

(defn sech->blob [all sech]
  (ROVec. all (sech :sh_offset) (+ (sech :sh_offset)
                                   (sech :sh_size))))
(def ElfAddr u32)
(def ElfHalf u16)
(def ElfOff u32)
(def ElfWord u32)
(def ElfXword u32)

(def elf_header {:type       :struct
                 :definition [[:ident {:type  :string
                                       :bytes 4}]
                              [:class i8]
                              [:data i8]
                              [:version i8]
                              (padding 9)
                              [:type ElfHalf]
                              [:machine ElfHalf]
                              [:version ElfWord]
                              [:entry ElfAddr]
                              [:phoff ElfOff]
                              [:shoff ElfOff]
                              [:flags (assoc ElfWord :adapter to-flags)]
                              [:ehsize ElfHalf]
                              [:phentsize ElfHalf]
                              [:phnum ElfHalf]
                              [:shentsize ElfHalf]
                              [:shnum ElfHalf]
                              [:shstrndx ElfHalf]]
                 :adapter    #(into {} %)})

(def header (deserialize elf_header data))

(def prog_header {:type :struct
                  :definition [[:type ElfWord]
                               [:off ElfOff]
                               [:vaddr ElfAddr]
                               [:paddr ElfAddr]
                               [:filesz ElfWord]
                               [:memsz ElfWord]
                               [:flags (assoc ElfWord :adapter to-flags)]
                               [:align ElfWord]]})

(def ph (deserialize {:type :array
                      :adapter vec
                      :len (header :phnum)
                      :element prog_header}
                     (ROVec. data (header :phoff))))

;(def sec_header {:type       :struct
;                 :definition [[:sh_name ElfWord]
;                              [:sh_type ElfWord]
;                              [:sh_flags (assoc ElfXword :adapter to-flags)]
;                              [:sh_addr ElfAddr]
;                              [:sh_offset ElfOff]
;                              [:sh_size ElfXword]
;                              [:sh_link ElfWord]
;                              [:sh_info ElfWord]
;                              [:sh_addralign ElfXword]
;                              [:sh_entsize ElfXword]]
;                 :adapter    #(into {} %)})
;
;(def _sechs (deserialize {:type    :array
;                          :len     (header :e_shnum)
;                          :element sec_header
;
;                          :adapter vec}
;                         (ROVec. data (header :e_shoff))))
;
;
;(let [sec_strtbl (nth _sechs (header :e_shstrndx))
;      bs (sech->blob data sec_strtbl)]
;  (def secnames_blob bs))
;(def sec_types [:NULL :PROGBITS :SYMTAB :STRTAB :RELA :HASH :DYNAMIC :NOTE :NOBITS :REL :SHLIB :DYNSYM])
;(def sechs (mapv
;             (fn [e]
;               (-> e
;                   (dissoc :blob)
;                   (update :sh_name (fn [loc]
;                                      (new String (byte-array
;                                                    (take-while #(not= % (byte 0))
;                                                                (ROVec. secnames_blob loc))))))
;                   (update :sh_type sec_types)))
;             _sechs))
;
;(def name->sech (map-vals first (group-by :sh_name sechs)))
;
;(let [sec_strtbl (nth _sechs (dec (header :e_shstrndx)))
;      bs (sech->blob data sec_strtbl)]
;  (def obj_symnames_blob bs))
;
;(def sym_entry {:type       :struct
;                :definition [[:name (assoc ElfWord :adapter
;                                                   (fn [loc]
;                                                     (new String (byte-array
;                                                                   (take-while #(not= % (byte 0))
;                                                                               (ROVec. obj_symnames_blob loc))))))]
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
