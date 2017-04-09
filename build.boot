(def +project+ 'miraj/co-dom)
(def +version+ "1.0.0-SNAPSHOT")

(set-env!
 :resource-paths #{"src/main/clj"}
 :source-paths #{"src/test/clj"}

 :repositories #(conj % ["clojars" {:url "https://clojars.org/repo/"}])

 :dependencies   '[[org.clojure/clojure RELEASE]
                   [org.clojure/data.json "0.2.6"]
                   [clj-time "0.11.0"]

                   [org.clojure/tools.logging "0.3.1" :scope "compile"]
                   [org.slf4j/slf4j-log4j12 "1.7.1"]
                   [log4j/log4j "1.2.17" :exclusions [javax.mail/mail
                                                      javax.jms/jms
                                                      com.sun.jmdk/jmxtools
                                                      com.sun.jmx/jmxri]]

                   ;; testing only
                   ;; [miraj/core "0.1.0-SNAPSHOT" :scope "test"]
;                   [miraj/html "5.1.0-SNAPSHOT" :scope "test"]
                   ;; [miraj.polymer/paper "1.2.3-SNAPSHOT" :scope "test"]
                   ;; [miraj.polymer/iron "1.2.3-SNAPSHOT" :scope "test"]

                   [samestep/boot-refresh "0.1.0" :scope "test"]
                   [adzerk/boot-test "1.2.0" :scope "test"]])

(require '[adzerk.boot-test :refer :all]
         '[samestep.boot-refresh :refer [refresh]])

(task-options!
 pom  {:project     +project+
       :version     +version+
       :description "Base library supporting functional HTML - see also miraj/html"
       :url         "https://github.com/miraj-project/co-dom"
       :scm         {:url "https://github.com/miraj-project/co-dom.git"}
       :license     {"EPL" "http://www.eclipse.org/legal/epl-v10.html"}})

(deftask build
  "build"
  []
  (comp (pom)
        (jar)
        (install)
        (target)))

(deftask check
  "watch etc. for dev using checkout"
  []
  (comp (watch)
        (notify :audible true)
        #_(refresh)
        (pom)
        (jar)
        (target)
        (install)))

(deftask systest
  "serve and repl for integration testing"
  []
  (set-env! :resource-paths #(conj % "test/system/clj"))
  (comp
   (build)
   (serve :dir "target")
   (cider)
   (repl)
   (watch)
   (notify :audible true)
   (target)))

(deftask utest
  "test:"
  [n namespaces  NS  #{sym}  "test ns"]
  (set-env! :source-paths #(conj % "test/unit/clj"))
  (test :namespaces namespaces
        :exclude #"data.xml.*"))
        ;; :filters #(= (-> % meta :name) "binding-1way")))
