(ns fctorial.data
  (:import ROVec))

(def mb (ROVec. (.readAllBytes (new java.io.FileInputStream "/home/fctorial/src/fuchsia/fuchsia/out/workstation.qemu-x64/multiboot.bin"))))

(def exec (ROVec. (.readAllBytes (new java.io.FileInputStream "data/t"))))

(def obj (ROVec. (.readAllBytes (new java.io.FileInputStream "data/t.o"))))

(def zbi (ROVec. (.readAllBytes (new java.io.FileInputStream "/home/fctorial/src/fuchsia/fuchsia/out/workstation.qemu-x64/fuchsia.zbi"))))
