(ns fctorial.data
  (:import clojure.lang.ROVec))

;(def mb (ROVec. (.readAllBytes (new java.io.FileInputStream "/home/fctorial/src/fuchsia/fuchsia/out/core.qemu-x64/multiboot.bin"))))

(def exec (ROVec. (.readAllBytes (new java.io.FileInputStream "data/t"))))

(def obj (ROVec. (.readAllBytes (new java.io.FileInputStream "data/t.o"))))

;(def zbi (ROVec. (.readAllBytes (new java.io.FileInputStream "/home/fctorial/src/fuchsia/fuchsia/out/core.qemu-x64/fuchsia.zbi"))))

(def sm (ROVec. (.readAllBytes (new java.io.FileInputStream "data/sm"))))

(def dm (ROVec. (.readAllBytes (new java.io.FileInputStream "data/dm"))))

(def syscall (ROVec. (.readAllBytes (new java.io.FileInputStream "data/syscall"))))

;(def ze (ROVec. (.readAllBytes (new java.io.FileInputStream "/home/fctorial/src/fuchsia/fuchsia/out/core.qemu-x64.zircon/kernel-x64-clang/obj/kernel/zircon.elf"))))
