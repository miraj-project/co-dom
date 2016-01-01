(ns styles)

(alter-meta! *ns*
             (fn [m] (assoc m :co-ns true :resource-type :css)))

;;https//github.com/necolas/normalize.css
(def normalize
  {:uri "bower_components/normalize-css/normalize.css"})

(def materialize
  {:uri "bower_components/materialize-css/materialize.css"})
 ;; <!-- Compiled and minified CSS -->
 ;;  <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/materialize/0.97.5/css/materialize.min.css">

 ;;  <!-- Compiled and minified JavaScript -->
 ;;  <script src="https://cdnjs.cloudflare.com/ajax/libs/materialize/0.97.5/js/materialize.min.js"></script>


(def foo {:uri "styles/foo.css"
          :media "(min-width 700px)"})

(def bar {:uri "styles/bar.css"
          :media "(min-width: 700px) and (orientation: landscape)"})
