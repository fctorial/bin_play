(defproject app "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [fctorial/parse_struct "0.10.1"]
                 [org.clojure/core.async "1.3.610"]
                 [nrepl "0.8.3"]]
  :profiles {:reveal {:dependencies [[vlaaad/reveal "1.2.186"]]
                      :repl-options {:nrepl-middleware [vlaaad.reveal.nrepl/middleware]}}}
  :java-source-paths ["src"]
  :repl-options {:init-ns fctorial.main})
