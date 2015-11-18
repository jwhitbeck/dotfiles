;;; Resources
;;;  - https://github.com/zcaudate/vinyasa
;;;  - http://z.caudate.me/give-your-clojure-workflow-more-flow/

{:user {:plugins [[cider/cider-nrepl "0.9.1"]
                  [lein-ancient "0.5.5"]
                  [lein-bikeshed "0.1.8"]
                  [lein-clojars "0.9.1"]
                  [lein-cloverage "1.0.2"]
                  [lein-exec "0.3.1"]
                  [lein-kibit "0.1.2"]
                  [lein-nodisassemble "0.1.3"]
                  [lein-try "0.4.1"]
                  [jonase/eastwood "0.2.2"]]
        :dependencies [[criterium "0.4.3"]
                       [org.clojure/tools.trace "0.7.6"]
                       [org.clojure/tools.namespace "0.2.4"]
                       [spyscope "0.1.4"]
                       [slamhound "1.5.1"]
                       [io.aviso/pretty "0.1.8"]]
        :injections [(require 'spyscope.core) ; for the reader macro #spy/d, available in all namespaces
                     (require '[criterium.core :refer [bench quick-bench]])
                     (require '[no.disassemble :refer [disassemble]])
                     ;; Pretty printing of stackstraces
                     (require 'io.aviso.repl 'clojure.repl)
                     (alter-var-root #'clojure.repl/pst
                                     (constantly @#'io.aviso.repl/pretty-pst))]
        :aliases {"slamhound" ["run" "-m" "slam.hound"]}}}
