(def +project+ 'miraj/co-dom)
(def +version+ "0.1.0-SNAPSHOT")

(set-env!
 :resource-paths #{"src/main/clj"}
 :source-paths #{"src/test/clj"}

 :dependencies   '[[org.clojure/clojure RELEASE :scope "provided"]
                   ;; [org.clojure/clojurescript "1.7.228"]
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

                   [samestep/boot-refresh "0.1.0"]
                   [adzerk/boot-test "1.2.0" :scope "test"]])

(require '[adzerk.boot-test :refer :all]
         '[samestep.boot-refresh :refer [refresh]])

;; ;; [boot/core "2.5.2" :scope "provided"]
;; ;; [adzerk/boot-test "1.0.7" :scope "test"]
;; ;; Webjars-locator uses logging
;; [org.slf4j/slf4j-nop "1.7.12" :scope "test"]
;; [org.webjars/webjars-locator "0.29"]
;; ;; For testing the webjars asset locator implementation
;; [org.webjars/bootstrap "3.3.6" :scope "test"]])

;; (require '[adzerk.boot-test :refer [test]])

(task-options!
 aot {:namespace #{'miraj.NSException}}
 pom  {:project     +project+
       :version     +version+
       :description "miraj co-dom"
       :url         "https://github.com/miraj-project/miraj.co-dom.git"
       :scm         {:url "https://github.com/miraj-project/miraj.co-dom.git"}
       :license     {"EPL" "http://www.eclipse.org/legal/epl-v10.html"}})

(deftask build
  "build"
  []
  (comp ;; (aot)
        (pom)
        (jar)
        (install)
        (target)))

(deftask dev
  "watch etc."
  []
  (comp (repl)
        (watch)
        (notify :audible true)
        #_(refresh)
        (pom)
        (jar)
        (target)
        (install)))

(deftask check
  "test:"
  [n namespaces  NS  #{sym}  "Compile all miraj vars in namespace NS."]
  (set-env! :source-paths #{"test"})
  ;;(set-env! :dependencies #(conj % '[miraj.polymer.paper "1.2.3-SNAPSHOT" :scope "test"]))
  (test :namespaces namespaces :exclude #"data.xml.*"))
