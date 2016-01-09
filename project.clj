(defproject miraj/markup "0.1.0-SNAPSHOT"
  :description "HTML5/Polymer in Clojure"
  :url "https://github.com/mobileink/miraj.markup"
  :source-paths ["src/main/clj"]
  :dependencies [[org.clojure/clojure "1.8.0-RC4"]
                 [polymer/behaviors "1.2.3-SNAPSHOT"]]
  :profiles {:dev {:prep-tasks ^:replace ["clean" "compile"]
                   :source-paths ["src/main/clj"] ;; "src/test/clj" "dev"]
                   :dependencies [[polymer/paper "1.2.3-SNAPSHOT"]
                                  [polymer/iron "1.2.3-SNAPSHOT"]
                                  [polymer/behaviors "1.2.3-SNAPSHOT"]
                                  [miraj/html "5.1.0-SNAPSHOT"]
                                  [org.clojure/tools.namespace "0.2.1"]
                                  [org.clojure/tools.logging "0.3.1"]
                                  [org.slf4j/slf4j-log4j12 "1.7.1"]
                                  [log4j/log4j "1.2.17" :exclusions [javax.mail/mail
                                                                     javax.jms/jms
                                                                     com.sun.jmdk/jmxtools
                                                                     com.sun.jmx/jmxri]]
                                  [ clj-logging-config "1.9.7"]]}
             :test {:resource-paths ["test/resources"]
                    :dependencies [[miraj "1.1.4-SNAPSHOT"]
                                   [polymer/paper "1.2.3-SNAPSHOT"]
                                   [miraj/html "5.1.0-SNAPSHOT"]]}}
  :repl-options {:port 4001}
  :test-selectors {:attrs :attrs
                   :docs :docs
                   :elts :elts
                   :html :html
                   :sugar :sugar})
