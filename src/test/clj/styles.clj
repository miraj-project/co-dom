(ns styles)

(alter-meta! *ns*
             (fn [m] (assoc m :co-ns true :resource-type :css)))

;;https//github.com/necolas/normalize.css
;;(def normalize "bower_components/normalize-css/normalize.css")

(def foo {:uri "styles/foo.css"
          :media "(min-width 700px)"})

(def bar {:uri "styles/bar.css"
          :media "(min-width: 700px) and (orientation: landscape)"})
