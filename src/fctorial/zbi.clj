(ns fctorial.zbi
  (:require [parse_struct.core :refer :all]
            [parse_struct.common_types :refer :all]
            [parse_struct.deserialize :refer [_deserialize]]
            [fctorial.utils :refer :all]
            [fctorial.data :refer :all])
  (:import ROVec))

(def ZBI_TYPES {0x544f4f42 :ZBI_TYPE_CONTAINER
                0x004e524b :ZBI_TYPE_KERNEL_PREFIX
                0x00FFFFFF :ZBI_TYPE_KERNEL_MASK
                0x4c4e524b :ZBI_TYPE_KERNEL_X64
                0x384e524b :ZBI_TYPE_KERNEL_ARM64
                0x4b534452 :ZBI_TYPE_STORAGE_RAMDISK
                0x42534642 :ZBI_TYPE_STORAGE_BOOTFS
                0x46534642 :ZBI_TYPE_STORAGE_BOOTFS_FACTORY
                0x4c444d43 :ZBI_TYPE_CMDLINE
                0x4d4f4f42 :ZBI_TYPE_CRASHLOG
                0x4c4c564e :ZBI_TYPE_NVRAM
                0x4c4c5643 :ZBI_TYPE_NVRAM_DEPRECATED
                0x44494C50 :ZBI_TYPE_PLATFORM_ID
                0x4953426D :ZBI_TYPE_DRV_BOARD_INFO
                0x43555043 :ZBI_TYPE_CPU_CONFIG
                0x544F504F :ZBI_TYPE_CPU_TOPOLOGY
                0x434D454D :ZBI_TYPE_MEM_CONFIG
                0x5652444B :ZBI_TYPE_KERNEL_DRIVER
                0x50445352 :ZBI_TYPE_ACPI_RSDP
                0x49424d53 :ZBI_TYPE_SMBIOS
                0x4d494645 :ZBI_TYPE_EFI_MEMORY_MAP
                0x53494645 :ZBI_TYPE_EFI_SYSTEM_TABLE
                0x30323845 :ZBI_TYPE_E820_TABLE
                0x42465753 :ZBI_TYPE_FRAMEBUFFER
                0x47524149 :ZBI_TYPE_IMAGE_ARGS
                0x53525642 :ZBI_TYPE_BOOT_VERSION
                0x43414D6D :ZBI_TYPE_DRV_MAC_ADDRESS
                0x5452506D :ZBI_TYPE_DRV_PARTITION_MAP
                0x524F426D :ZBI_TYPE_DRV_BOARD_PRIVATE
                0x42525748 :ZBI_TYPE_HW_REBOOT_REASON
                0x4e4c5253 :ZBI_TYPE_SERIAL_NUMBER
                0x4C465442 :ZBI_TYPE_BOOTLOADER_FILE
                0xd00dfeed :ZBI_TYPE_DEVICETREE})

(defn zbi_align [n]
  (if (zero? (mod n 8))
    n
    (+ n (- 8 (mod n 8)))))

(def uint32_t u32)

(def zbi_header_t {:type       :struct
                   :definition [[:type (assoc uint32_t :adapter ZBI_TYPES)]
                                [:length uint32_t]
                                [:extra (assoc uint32_t :adapter #(format "0x%08x" (long %)))]
                                [:flags (assoc uint32_t :adapter to-flags)]
                                (padding 8)
                                ;[:reserved0 uint32_t]
                                ;[:reserved1 uint32_t]
                                [:magic (assoc uint32_t :adapter #(format "0x%08x" (long %)))]
                                [:crc32 uint32_t]]})

(def zbi_coll_header (deserialize zbi_header_t zbi))

(def headers (loop [res []
                    off (type-size zbi_header_t)]
               (if (>= off (count zbi))
                 (->> res
                      (group-by :type)
                      (map-vals first))
                 (let [header (deserialize zbi_header_t (ROVec. zbi off))
                       from (+ off (type-size zbi_header_t))
                       to (+ from (header :length))]
                   (recur (conj res
                                (assoc header :from from
                                              :to to))
                          (zbi_align to))))))

(def cmdline (deserialize {:type    :string
                           :bytes   (get-in headers [:ZBI_TYPE_CMDLINE :length])
                           :adapter trim-nulls-end}
                          (ROVec. zbi (get-in headers
                                              [:ZBI_TYPE_CMDLINE :from]
                                              [:ZBI_TYPE_CMDLINE :to]))))

(def kernel_header (deserialize {:type       :struct
                                 :definition [[:entry u64]
                                              [:reserve u64]]}
                                (ROVec. zbi (->> headers
                                                 :ZBI_TYPE_KERNEL_X64
                                                 :from))))
