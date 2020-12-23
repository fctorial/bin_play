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

(def ElfAddr u64)
(def ElfHalf u16)
(def ElfOff u64)
(def ElfWord u32)
(def ElfXword u64)

(def elf_header {:type       :struct
                 :definition [[:ident {:type  :string
                                       :bytes 4}]
                              [:ei_class i8]
                              [:ei_data i8]
                              [:ei_version i8]
                              (padding 9)
                              [:e_type ElfHalf]
                              [:e_machine ElfHalf]
                              [:e_version ElfWord]
                              [:e_entry ElfAddr]
                              [:e_phoff ElfOff]
                              [:e_shoff ElfOff]
                              [:e_flags (assoc ElfWord :adapter to-flags)]
                              [:e_ehsize ElfHalf]
                              [:e_phentsize ElfHalf]
                              [:e_phnum ElfHalf]
                              [:e_shentsize ElfHalf]
                              [:e_shnum ElfHalf]
                              [:e_shstrndx ElfHalf]]
                 :adapter    #(into {} %)})
(def obj_header (deserialize elf_header obj))

(def sec_header {:type       :struct
                 :definition [[:sh_name ElfWord]
                              [:sh_type ElfWord]
                              [:sh_flags (assoc ElfXword :adapter to-flags)]
                              [:sh_addr ElfAddr]
                              [:sh_offset ElfOff]
                              [:sh_size ElfXword]
                              [:sh_link ElfWord]
                              [:sh_info ElfWord]
                              [:sh_addralign ElfXword]
                              [:sh_entsize ElfXword]]
                 :adapter    #(into {} %)})

(def _obj_sechs (deserialize {:type    :array
                              :len     (obj_header :e_shnum)
                              :element sec_header

                              :adapter vec}
                             (ROVec. obj (obj_header :e_shoff))))

(let [sec_strtbl (nth _obj_sechs (obj_header :e_shstrndx))
      bs (ROVec. obj (sec_strtbl :sh_offset) (+ (sec_strtbl :sh_offset)
                                                (sec_strtbl :sh_size)))]
  (def obj_names_blob bs))
(def sec_types ["NULL" "PROGBITS" "SYMTAB" "STRTAB" "RELA" "HASH" "DYNAMIC" "NOTE" "NOBITS" "REL" "SHLIB" "DYNSYM"])
(def obj_sechs (mapv
                 (fn [e]
                   (-> e
                       (dissoc :blob)
                       (update :sh_name (fn [loc]
                                          (new String (byte-array
                                                        (take-while #(not= % (byte 0))
                                                                    (ROVec. obj_names_blob loc))))))
                       (update :sh_type sec_types)))
                 _obj_sechs))

(def exec_header (deserialize elf_header exec))
