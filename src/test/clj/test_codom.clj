(ns test_codom
  (:refer-clojure :exclude [import])
  (:require [miraj.co-dom :refer :all]))

;; (def doc (element :html
;;                   (element :head
;;                            (element :meta {:name "description"
;;                                            :content "miraj.co-dom test"})
;;                            (element :script {:src "/scripts/foo.js"}))
;;                   (element :body
;;                            (element :h1 "Hello World"))))

;; (println doc)

;; (serialize doc)

(serialize (element :div {:$background-color "red"
                          :$color "green"
                          :id "foo"
                          :bar 99}))


;; (pprint doc)

;; (println (optimize :js doc))

;; (pprint (optimize :js doc))

;; (co-compile "resources/footest.html"
;;             (optimize :js doc)
;;             :pprint)
