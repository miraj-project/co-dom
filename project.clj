(defproject miraj/markup "0.1.0-SNAPSHOT"
  :description "core.data.xml fork"
  :url "https://github.com/mobileink/data.xml"
  :source-paths ["src/main/clojure"]
  ;; :test-paths ["src/test/clojure"]
  :dependencies [[org.clojure/clojure "1.8.0-RC3"]
                 [org.clojure/tools.logging "0.3.1"]
                 [org.slf4j/slf4j-log4j12 "1.7.1"]
                 [log4j/log4j "1.2.17" :exclusions [javax.mail/mail
                                                    javax.jms/jms
                                                    com.sun.jmdk/jmxtools
                                                    com.sun.jmx/jmxri]]
                 [ clj-logging-config "1.9.7"]]
  :profiles {:dev {:prep-tasks ^:replace ["clean" "compile"]
                   :source-paths ["src/main/clojure" "src/test/clojure" "dev"]}}
  :repl-options {:port 4001})
