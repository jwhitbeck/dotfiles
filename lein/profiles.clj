{:repl {:dependencies [[org.clojure/tools.nrepl "0.2.12"]]}
 :user {:plugins [[cider/cider-nrepl "0.13.0"]
                  [lein-ancient "0.6.8"]
                  [lein-clojars "0.9.1"]
                  [lein-kibit "0.1.2"]
                  [lein-nodisassemble "0.1.3"]
                  [lein-try "0.4.3"]
                  [jonase/eastwood "0.2.3"]]
        :dependencies [[criterium "0.4.3"]
                       [org.clojure/tools.trace "0.7.9"]
                       [org.clojure/tools.namespace "0.2.11"]
                       [spyscope "0.1.5"]
                       [slamhound "1.5.5"]]
        :injections [(require 'spyscope.core) ; for the reader macro #spy/d, available in all namespaces
                     (require '[criterium.core :refer [bench quick-bench]])
                     (require '[no.disassemble :refer [disassemble]])
                     (require '[clojure.tools.namespace.repl :refer [refresh]])]
        :aliases {"slamhound" ["run" "-m" "slam.hound"]}}}
