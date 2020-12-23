(ns fctorial.data
  (:import ROVec))

(def mb (ROVec. (.readAllBytes (new java.io.FileInputStream "data/multiboot.bin"))))

(def exec (ROVec. (.readAllBytes (new java.io.FileInputStream "data/t"))))

(def obj (ROVec. (.readAllBytes (new java.io.FileInputStream "data/t.o"))))
