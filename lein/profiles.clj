{:user {:plugins [[cider/cider-nrepl "0.13.0"]
                  [lein-kibit "0.1.2"]
                  ;; A Leiningen plugin to check your project for outdated dependencies and plugins.
                  ;; https://github.com/xsc/lein-ancient
                  [lein-ancient "0.6.10"]
                  [lein-nodisassemble "0.1.3"]
                  [jonase/eastwood "0.2.3"]]
        :dependencies [[criterium "0.4.3"]]
        :injections [(require '[criterium.core :refer [bench quick-bench]])
                     (require '[no.disassemble :refer [disassemble]])]}}
