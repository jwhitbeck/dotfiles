;;; Resources
;;;  - https://github.com/zcaudate/vinyasa
;;;  - http://z.caudate.me/give-your-clojure-workflow-more-flow/

{:user {:plugins [[cider/cider-nrepl "0.7.0-SNAPSHOT"]
                  [lein-ancient "0.5.5"]
                  [lein-clojars "0.9.1"]
                  [lein-exec "0.3.1"]
                  [lein-kibit "0.0.8"]
                  [lein-try "0.4.1"]
                  [jonase/eastwood "0.1.0"]]
        :dependencies [[criterium "0.4.3"]
                       [org.clojure/tools.trace "0.7.6"]
                       [org.clojure/tools.namespace "0.2.4"]
                       [leiningen "2.3.4"]
                       [im.chit/vinyasa "0.1.8"]
                       [spyscope "0.1.4"]
                       [slamhound "1.5.1"]
                       [io.aviso/pretty "0.1.8"]]
        :injections [(require '[vinyasa.inject :as inj])
                     (inj/inject 'clojure.core
                                 '[[vinyasa.inject inject]
                                   [vinyasa.pull pull]
                                   [vinyasa.lein lein]
                                   [vinyasa.reimport reimport]
                                   [clojure.stacktrace e print-stack-trace]
                                   [clojure.tools.trace trace-ns trace-vars untrace-ns untrace-vars]
                                   [clojure.tools.namespace.repl refresh]
                                   [criterium.core bench quick-bench]])
                     (require 'spyscope.core) ; for the reader macro #spy/d, available in all namespaces
                     ;; Pretty printing of stackstraces
                     (require 'io.aviso.repl
                              'clojure.repl
                              'clojure.main)
                     (alter-var-root #'clojure.repl/pst
                                     (constantly @#'io.aviso.repl/pretty-pst))]
        :aliases {"slamhound" ["run" "-m" "slam.hound"]}}}
