(ns fctorial.main
  (:require [parse_struct.core :refer :all]
            [parse_struct.common_types :refer :all]
            [fctorial.utils :refer :all])
  (:import ROVec)
  )

(def bs (ROVec. (.readAllBytes (new java.io.FileInputStream "data/multiboot.bin"))))

(def MULTIBOOT_MAGIC (mapv to-byte [0x1B 0xAD 0xB0 0x02]))
(def BOOTLOADER_MAGIC (mapv to-byte [0x2B 0xAD 0xB0 0x02]))

(def off1 (->> (range (- (count bs) 4))
               (filter (fn [i]
                         (every? (partial apply =) (zip-colls MULTIBOOT_MAGIC (reverse (ROVec. bs i (+ i 4)))))))
               first))
(def mb_header (ROVec. bs off1 (+ off1 48)))

(def fields (deserialize {:type       :struct
                          :definition [[:magic u32]
                                       [:flags (assoc u32 :adapter to-flags)]
                                       [:checksum u32]
                                       [:header_addr u32]
                                       [:load_addr u32]
                                       [:load_end_addr u32]
                                       [:bss_end_addr u32]
                                       [:entry_addr u32]
                                       [:mode_type u32]
                                       [:width u32]
                                       [:height u32]
                                       [:depth u32]]}
                         mb_header))

(def off2 (->> (range (- (count bs) 4))
               (filter (fn [i]
                         (every? (partial apply =)
                                 (zip-colls BOOTLOADER_MAGIC (reverse (ROVec. bs i (+ i 4)))))))
               first))
