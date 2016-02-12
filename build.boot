(def +version+ "0.1.0-SNAPSHOT")

(set-env!
  :resource-paths #{"src/main/clj"}
  :source-paths #{"src/main/clj"}
  :dependencies   '[[org.clojure/clojure "1.8.0" :scope "provided"]
                    [org.clojure/data.json "0.2.6"]
                    [clj-time "0.11.0"]
                    [org.clojure/clojurescript "1.7.228"]])

                    ;; ;; [boot/core "2.5.2" :scope "provided"]
                    ;; ;; [adzerk/boot-test "1.0.7" :scope "test"]
                    ;; ;; Webjars-locator uses logging
                    ;; [org.slf4j/slf4j-nop "1.7.12" :scope "test"]
                    ;; [org.webjars/webjars-locator "0.29"]
                    ;; ;; For testing the webjars asset locator implementation
                    ;; [org.webjars/bootstrap "3.3.6" :scope "test"]])

;; (require '[adzerk.boot-test :refer [test]])

(task-options!
  pom  {:project     'miraj/markup
        :version     +version+
        :description "miraj markup"
        :url         "https://github.com/miraj-project/miraj.markup.git"
        :scm         {:url "https://github.com/miraj-project/miraj.markup.git"}
        :license     {"EPL" "http://www.eclipse.org/legal/epl-v10.html"}})
