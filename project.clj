(defproject latte-kernel "1.0b1-SNAPSHOT"
  :description "The (very) small kernel of the LaTTe proof assistant"
  :url "https://github.com/latte-central/latte-kernel.git"
  :license {:name "MIT Licence"
            :url "http://opensource.org/licenses/MIT"}
  ;; :dependencies []
  :codox {:output-path "docs"
          :metadata {:doc/format :markdown}
          ;;:namespaces []
          }
  :plugins [[lein-cljsbuild "1.1.7"]]
  :cljsbuild {
              :builds [{:source-paths ["src-cljs"]
                        :compiler {:output-to "resources/public/js/main.js"
                                   :optimizations :whitespace
                                   :pretty-print true}}]}
  :profiles {:dev {:dependencies
                   [[org.clojure/clojure "1.9.0"]
                    [org.clojure/clojurescript "1.9.946"]
                    [com.cemerick/piggieback "0.2.2"]
                    ]}
             :test {:dependencies [[org.clojure/clojure "1.9.0"]]}})

