(ns scripts)

(alter-meta! *ns*
             (fn [m] (assoc m :co-ns true :resource-type :js)))

(def jquery {:uri "https://code.jquery.com/jquery-2.1.1.min.js"})

(def foo {:uri "js/foo.js"})
