(ns styles.materialize)

(alter-meta! *ns*
             (fn [m] (assoc m :co-ns true :resource-type :css+js)))

(def materialize
  {:uri "bower_components/materialize-css/materialize.css"})
 ;; <!-- Compiled and minified CSS -->
 ;;  <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/materialize/0.97.5/css/materialize.min.css">

 ;;  <!-- Compiled and minified JavaScript -->
 ;;  <script src="https://cdnjs.cloudflare.com/ajax/libs/materialize/0.97.5/js/materialize.min.js"></script>


  ;; <!DOCTYPE html>
  ;; <html>
  ;;   <head>
  ;;     <!--Import Google Icon Font-->
  ;;     <link href="http://fonts.googleapis.com/icon?family=Material+Icons" rel="stylesheet">
  ;;     <!--Import materialize.css-->
  ;;     <link type="text/css" rel="stylesheet" href="css/materialize.min.css"  media="screen,projection"/>

  ;;     <!--Let browser know website is optimized for mobile-->
  ;;     <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
  ;;   </head>

  ;;   <body>
  ;;     <!--Import jQuery before materialize.js-->
  ;;     <script type="text/javascript" src="https://code.jquery.com/jquery-2.1.1.min.js"></script>
  ;;     <script type="text/javascript" src="js/materialize.min.js"></script>
  ;;   </body>
  ;; </html>
