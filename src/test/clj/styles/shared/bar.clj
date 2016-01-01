(ns styles.shared.bar)

(alter-meta! *ns*
             (fn [m] (assoc m
                            :co-ns true
                            :resource-type :polymer-style-module)))

(def uri "shared-styles/bar.html")

(def bara "baz-style") ;; baz-style is in bar.html

(def barb "buz-style") ;; ditto
