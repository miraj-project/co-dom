(ns styles.shared)

(alter-meta!
 *ns* (fn [m] (assoc m
                     :co-ns true
                     :resource-type :polymer-style-module)))

(def uri "styles/shared/foo.html")

(def foo "foo-style") ;; foo-style is in foo.html

(def bar "bar-style") ;; ditto
