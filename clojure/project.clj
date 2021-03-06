(defproject mal "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [jline "2.11"]
                 [me.raynes/conch "0.8.0"]]
  :repl-options {:init-ns user}
  :main mal.step0-repl
  :profiles {:step0 {:main mal.step0-repl}
             :step1 {:main mal.step1-read-print}
             :step2 {:main mal.step2-eval}
             :step3 {:main mal.step3-env}
             :step4 {:main mal.step4-if-fn-do}
             :step5 {:main mal.step5-tco}
             :step6 {:main mal.step6-file}
             :step7 {:main mal.step7-quote}
             :step8 {:main mal.step8-macros}
             :step9 {:main mal.step9-try}
             :stepA {:main mal.stepA-mal}

             :dev {:dependencies [[org.clojure/tools.namespace "0.2.7"]
                                  [org.clojure/test.check "0.7.0"]]
                   :source-paths ["dev"]}})
