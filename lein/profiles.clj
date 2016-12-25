{:user {:plugins [[cider/cider-nrepl "0.13.0"]
                  [lein-ancient "0.6.8"]
                  [lein-clojars "0.9.1"]
                  [lein-kibit "0.1.2"]
                  [lein-nodisassemble "0.1.3"]
                  [jonase/eastwood "0.2.3"]]
        :dependencies [[criterium "0.4.3"]
                       [spyscope "0.1.5"]]
        :injections [(require 'spyscope.core) ; for the reader macro #spy/d, available in all namespaces
                     (require '[criterium.core :refer [bench quick-bench]])
                     (require '[no.disassemble :refer [disassemble]])]}}
