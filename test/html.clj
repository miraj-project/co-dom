(ns test.html
  (:require [miraj.html :as h]
            [miraj.markup :as xml]
            [clojure.tools.logging :as log :only [trace debug error info]]
            [clojure.pprint :as pp]
            [clojure.string :as string]
            [clojure.test]))
            ;; [miraj.markup.test-utils :refer [test-stream lazy-parse*]]))

(xml/pprint
; (xml/serialize
  :html (h/html (h/body "foo")))

;; HTML boolean attribs: empty string or matching string, e.g. {:foo ""} or {:foo "foo"}
(xml/pprint :html (h/div {:foo "foo"} "Hello, world"))
(println (xml/serialize :html (h/div {:foo "foo"} "Hello, world")))
(xml/pprint :html (h/div {:foo ""} "Hello, world"))
(println (xml/serialize :html (h/div {:foo ""} "Hello, world")))

;; Polymer annotations: :foo => {{foo}} (two-way); 'foo => [[foo]] (one-way)
(xml/pprint :html (h/div {:items 'employees}))
(xml/pprint :html (h/div {:items :employees}))
(xml/pprint :html (h/span :index ", " :items))
(xml/pprint :html (h/span 'index ", " :items))
(xml/pprint :html (h/span :index ", " 'items))
(xml/pprint :html (h/span 'index ", " 'items))



;; (xml/pprint
;;  :html
;;  (h/html
;;   (h/link {:href "foo"})
;;   (h/body
;;    (h/div
;;     (iron/list "foo")
;;     (paper/button "hello")))))
