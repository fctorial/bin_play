(ns fctorial.types
  (:require [parse_struct.common_types :refer [i8 u8 u16 u32 u64 u16be u32be u64be padding]]
            [fctorial.utils :refer [to-flags]]))

(def ^:dynamic ElfAddr nil)
(def ^:dynamic ElfHalf nil)
(def ^:dynamic ElfOff nil)
(def ^:dynamic ElfWord nil)
(def ^:dynamic ElfXword nil)

(def arch->prims {:LE {:32 {#'ElfAddr  u32
                            #'ElfHalf  u16
                            #'ElfOff   u32
                            #'ElfWord  u32
                            #'ElfXword u32}
                       :64 {#'ElfAddr  u64
                            #'ElfHalf  u16
                            #'ElfOff   u64
                            #'ElfWord  u32
                            #'ElfXword u64}}
                  :BE {:32 {#'ElfAddr  u32be
                            #'ElfHalf  u16be
                            #'ElfOff   u32be
                            #'ElfWord  u32be
                            #'ElfXword u32be}
                       :64 {#'ElfAddr  u64be
                            #'ElfHalf  u16be
                            #'ElfOff   u64be
                            #'ElfWord  u32be
                            #'ElfXword u64be}}})

(defmacro subs-prims [prims df]
  `(with-bindings (vec ~prims)
     ~df))

(defn opt-map [m val]
  (get m val val))

(defn make_header_t [plat_key]
  {:type       :struct
   :definition (subs-prims (get-in arch->prims plat_key)
                           [[:type (assoc ElfHalf :adapter #(opt-map [:ElfType/NONE :ElfType/REL :ElfType/EXEC :ElfType/DYN :ElfType/CORE] (int %)))]
                            [:machine (assoc ElfHalf :adapter #(opt-map {0  :MachineType/NONE
                                                                         1  :MachineType/M32
                                                                         2  :MachineType/SPARC
                                                                         3  :MachineType/_386
                                                                         4  :MachineType/_68K
                                                                         5  :MachineType/_88K
                                                                         6  :MachineType/_860
                                                                         7  :MachineType/MIPS
                                                                         8  :MachineType/MIPS_RS4
                                                                         62 :MachineType/X86_64} (int %)))]
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
                            [:shstrndx (assoc ElfHalf :adapter int)]])})

(defn make_secheader_t [plat_key]
  {:type       :struct
   :definition (subs-prims (get-in arch->prims plat_key)
                           [[:name ElfWord]
                            [:type (assoc ElfWord :adapter (partial opt-map [:SectionType/NULL
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
                                                                             :SectionType/DYNSYM]))]
                            [:flags (assoc ElfXword :adapter #(map
                                                                (partial opt-map {0 :WRITE
                                                                                  1 :ALLOC
                                                                                  2 :EXEC})
                                                                (to-flags %)))]
                            [:addr ElfAddr]
                            [:offset ElfOff]
                            [:size ElfXword]
                            [:link ElfWord]
                            [:info ElfWord]
                            [:addralign ElfXword]
                            [:entsize ElfXword]])})

(defn make_sym_t [plat_key]
  {:type       :struct
   :definition (subs-prims (get-in arch->prims plat_key)
                           ({:64 [[:name ElfWord]
                                  [:info (assoc u8 :adapter (fn [n]
                                                              {:binding (opt-map [:LOCAL :GLOBAL :WEAK] (bit-shift-right n 4))
                                                               :type    (opt-map [:NOTYPE :OBJECT :FUNC :SECTION :FILE] (bit-and n 0xf))}))]
                                  (padding 1)
                                  [:shndx (assoc ElfHalf :adapter int)]
                                  [:value ElfAddr]
                                  [:size ElfXword]]
                             :32 [[:name ElfWord]
                                  [:value ElfAddr]
                                  [:size ElfXword]
                                  [:info (assoc u8 :adapter (fn [n]
                                                              {:binding (opt-map [:LOCAL :GLOBAL :WEAK] (bit-shift-right n 4))
                                                               :type    (opt-map [:NOTYPE :OBJECT :FUNC :SECTION :FILE] (bit-and n 0xf))}))]
                                  (padding 1)
                                  [:shndx ElfHalf]]}
                            (plat_key 1)))})

(defn make_phdr_t [plat_key]
  {:type       :struct
   :definition (subs-prims (get-in arch->prims plat_key)
                           ({:64 [[:type (assoc ElfWord :adapter (partial opt-map [:NULL :LOAD :DYNAMIC :INTERP :NOTE :SHLIB :PHDR]))]
                                  [:flags (assoc ElfWord :adapter #(map
                                                                     [:X :W :R]
                                                                     (to-flags %)))]
                                  [:offset ElfOff]
                                  [:vaddr ElfAddr]
                                  [:paddr ElfAddr]
                                  [:filesz ElfXword]
                                  [:memsz ElfXword]
                                  [:align ElfXword]]
                             :32 [[:type (assoc ElfWord :adapter (partial opt-map [:NULL :LOAD :DYNAMIC :INTERP :NOTE :SHLIB :PHDR]))]
                                  [:offset ElfOff]
                                  [:vaddr ElfAddr]
                                  [:paddr ElfAddr]
                                  [:filesz ElfXword]
                                  [:memsz ElfXword]
                                  [:flags (assoc ElfWord :adapter #(map
                                                                     [:X :W :R]
                                                                     (to-flags %)))]
                                  [:align ElfXword]]}
                            (plat_key 1)))})
