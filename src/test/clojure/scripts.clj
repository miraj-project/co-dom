(ns scripts)

(alter-meta! *ns*
             (fn [m] (assoc m :co-ns true :resource-type :js)))

(def jquery "https://code.jquery.com/jquery-2.1.1.min.js")

(def materialize "js/materialize.min.js")
