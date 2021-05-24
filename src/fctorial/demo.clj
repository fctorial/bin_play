(ns fctorial.demo
  (:require [parse_struct.core :refer :all]
            [parse_struct.common_types :refer :all]
            [clojure.pprint :refer [pprint]]
            [fctorial.utils :refer :all]
            [fctorial.data :refer [obj]]
            )
  (:import (clojure.lang ROVec MMap)))

(def ElfAddr u64)
(def ElfHalf u16)
(def ElfOff u64)
(def ElfWord u32)
(def ElfXword u64)

(def magic_t {:type       :struct
              :definition [[:ident {:type  :string
                                    :bytes 4}]
                           [:class (assoc i8 :adapter {1 :32 2 :64})]
                           [:data (assoc i8 :adapter {1 :LE 2 :BE})]
                           [:version i8]]})

(def magic (deserialize magic_t obj))

(assert (= (magic :class) :64))
(assert (= (magic :data) :LE))
(assert (= (magic :ident) "\u007FELF"))

(def elf_header_t {:type       :struct
                   :definition [(padding 24)
                                [:shoff ElfOff]
                                (padding 10)
                                [:shentsize ElfHalf]
                                [:shnum ElfHalf]
                                [:shstrndx (assoc ElfHalf :adapter int)]]})
(def elf_header (deserialize elf_header_t
                             (ROVec. obj 16)))

(assert (= (+ (elf_header :shoff)
              (* (elf_header :shentsize)
                 (elf_header :shnum)))
           (count obj)))

(def sec_header_t {:type       :struct
                   :definition [(padding 4)
                                [:type (assoc ElfWord :adapter #(get [:SectionType/NULL
                                                                      :SectionType/PROGBITS
                                                                      :SectionType/SYMTAB
                                                                      :SectionType/STRTAB
                                                                      :SectionType/RELA
                                                                      :SectionType/HASH
                                                                      :SectionType/DYNAMIC
                                                                      :SectionType/NOTE
                                                                      :SectionType/NOBITS
                                                                      :SectionType/REL
                                                                      :SectionType/SHLIB
                                                                      :SectionType/DYNSYM]
                                                                     %))]
                                (padding 16)
                                [:offset ElfOff]
                                [:size ElfXword]
                                [:link ElfWord]
                                (padding 20)]})

(def sec_1 (deserialize sec_header_t (ROVec. obj (elf_header :shoff))))

(def secs (deserialize {:type    :array
                        :len     (elf_header :shnum)
                        :element sec_header_t
                        :adapter vec}
                       (ROVec. obj (elf_header :shoff))))
(def symtab_header (first (filter #(= (% :type) :SectionType/SYMTAB) secs)))
(def symnames_header (secs (symtab_header :link)))
(def symnames (deserialize {:type  :string
                            :bytes (symnames_header :size)}
                           (ROVec. obj (symnames_header :offset))))

(def sym_t {:type       :struct
            :definition [[:name (assoc ElfWord :adapter (fn [idx]
                                                          (.substring symnames
                                                                      idx
                                                                      (.indexOf symnames 0 idx))))]
                         (padding 2)
                         [:shndx ElfHalf]
                         [:value ElfAddr]
                         [:size ElfXword]]})

(def symbols (deserialize {:type    :array
                           :len     (/ (symtab_header :size)
                                       (type-size sym_t))
                           :element sym_t}
                          (ROVec. obj (symtab_header :offset))))

(def spec {:type    :array
           :len     20
           :element i32be})

(def data1 (range 20))

(def bs (serialize spec data1))

(def data2 (deserialize spec bs))

(assert (= data1 data2))
