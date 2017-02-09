{:user {:plugins [[cider/cider-nrepl "0.14.0"]
                  ;; A Leiningen plugin to check your project for outdated dependencies and plugins.
                  ;; https://github.com/xsc/lein-ancient
                  [lein-ancient "0.6.10"]
                  ;; kibit is a static code analyzer for Clojure. Use with kibit-helper emacs package.
                  ;; https://github.com/jonase/kibit
                  [lein-kibit "0.1.3" :exclusions [org.clojure/clojure]]
                  ;; A Clojure library designed to let you inspect bytecode of functions and things.
                  ;; https://github.com/gtrak/no.disassemble
                  ;[lein-nodisassemble "0.1.3"]
                  ;; Eastwood is a Clojure lint tool that uses the tools.analyzer and tools.analyzer.jvm
                  ;; libraries to inspect namespaces and report possible problems.
                  ;; https://github.com/jonase/eastwood
                  ;[jonase/eastwood "0.2.3" :exclusions [org.clojure/clojure]]
                  ]
        :dependencies [[criterium "0.4.3"]]}}
