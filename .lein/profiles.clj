;;; Resources
;;;  - https://github.com/zcaudate/vinyasa
;;;  - http://z.caudate.me/give-your-clojure-workflow-more-flow/

{:user {:plugins [[cider/cider-nrepl "0.8.1"]
                  [lein-ancient "0.5.5"]
                  [lein-bikeshed "0.1.8"]
                  [lein-clojars "0.9.1"]
                  [lein-cloverage "1.0.2"]
                  [lein-exec "0.3.1"]
                  [lein-kibit "0.0.8"]
                  [lein-try "0.4.1"]
                  [jonase/eastwood "0.1.4"]
                  [org.bodil/lein-noderepl "0.1.10"]]
        :dependencies [[criterium "0.4.3"]
                       [org.clojure/tools.trace "0.7.6"]
                       [org.clojure/tools.namespace "0.2.4"]
                       [leiningen #=(leiningen.core.main/leiningen-version)]
                       [im.chit/vinyasa "0.3.0"]
                       [spyscope "0.1.4"]
                       [slamhound "1.5.1"]
                       [io.aviso/pretty "0.1.8"]]
        :injections [(require 'spyscope.core) ; for the reader macro #spy/d, available in all namespaces
                     (require '[vinyasa.inject :as inj])
                     (require 'io.aviso.repl
                              'clojure.repl
                              'clojure.main)
                     (inj/in clojure.core
                             [vinyasa.inject :refer [inject]]
                             [vinyasa.pull :all]
                             [vinyasa.reimport :all]
                             [clojure.stacktrace :refer [e print-stack-trace]]
                             [clojure.tools.trace :refer [trace-ns trace-vars untrace-ns untrace-vars]]
                             [clojure.tools.namespace.repl :refer [refresh]]
                             [criterium.core :refer [bench quick-bench]])
                     ;; Pretty printing of stackstraces
                     (alter-var-root #'clojure.repl/pst
                                     (constantly @#'io.aviso.repl/pretty-pst))]
        :aliases {"slamhound" ["run" "-m" "slam.hound"]}}}
