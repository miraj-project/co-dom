(ns test
  (:require [miraj.markup :refer :all]))

(def doc (element :html
                  (element :head
                           (element :meta {:name "description"
                                           :content "co-compile test"})
                           (element :script {:src "/scripts/foo.js"}))
                  (element :body
                           (element :h1 "Hello World"))))

(println doc)

(println (serialize doc))

(println (optimize :js doc))

(co-compile "resources/footest.html"
            (optimize :js doc)
            :pprint)
