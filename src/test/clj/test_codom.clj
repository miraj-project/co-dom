(ns test_codom
  (:refer-clojure :exclude [import])
  (:require [miraj.co-dom :refer :all]
            [miraj.html :as h]))

;; (def doc (element :html
;;                   (element :head
;;                            (element :meta {:name "description"
;;                                            :content "miraj.co-dom test"})
;;                            (element :script {:src "/scripts/foo.js"}))
;;                   (element :body
;;                            (element :h1 "Hello World"))))

;; (println doc)

;; (serialize doc)

(pprint (element :div :#foo.bar!centered
                    {:$background-color "red"
                     :bar 99}))

(serialize
 (h/div :#mydiv.centered {:$color "green" :foo 99}))

;; (pprint doc)

;; (println (optimize :js doc))

;; (pprint (optimize :js doc))

;; (co-compile "resources/footest.html"
;;             (optimize :js doc)
;;             :pprint)
