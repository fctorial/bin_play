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
                           [[:type (assoc ElfHalf :adapter #(opt-map [:NONE :REL :EXEC :DYN :CORE] (int %)))]
                            [:machine (assoc ElfHalf :adapter #(opt-map [:NONE :M32 :SPARC :386 :68K :88K :860 :MIPS :MIPS_RS4] (int %)))]
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
                            [:shstrndx ElfHalf]])})

(defn make_secheader_t [plat_key]
  {:type       :struct
   :definition (subs-prims (get-in arch->prims plat_key)
                           [[:name ElfWord]
                            [:type (assoc ElfWord :adapter #(opt-map [:NULL :PROGBITS :SYMTAB :STRTAB :RELA :HASH :DYNAMIC :NOTE :NOBITS :REL :SHLIB :DYNSYM] %))]
                            [:flags (assoc ElfXword :adapter to-flags)]
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
                                  [:shndx ElfHalf]
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
