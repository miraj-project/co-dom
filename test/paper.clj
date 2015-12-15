(ns test.paper
  (:require [polymer.paper :as paper]
            [miraj.markup :as xml]
            [clojure.tools.logging :as log :only [trace debug error info]]
            [clojure.pprint :as pp]
            [clojure.string :as string]
            [clojure.test]))
            ;; [miraj.markup.test-utils :refer [test-stream lazy-parse*]]))

(xml/pprint
; (xml/serialize
  :html (paper/button "foo"))

;; Polymer annotations: :foo => {{foo}} (two-way); 'foo => [[foo]] (one-way)
(xml/pprint :html (paper/dialog {:id "foo" :x 'employees}))


(meta (paper/dialog {:id "foo" :x 'employees}))
