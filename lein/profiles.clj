{:user {:plugins [[cider/cider-nrepl "0.13.0"]
                  [lein-ancient "0.6.8"]
                  [lein-kibit "0.1.2"]
                  [lein-nodisassemble "0.1.3"]
                  [jonase/eastwood "0.2.3"]]
        :dependencies [[criterium "0.4.3"]]
        :injections [(require '[criterium.core :refer [bench quick-bench]])
                     (require '[no.disassemble :refer [disassemble]])]}}
