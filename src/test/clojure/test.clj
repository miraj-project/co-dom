(ns test
  (:require [miraj.markup :refer :all]
            [miraj.html :as h]
            #_[clojure.data.xml :as x]))

(def doc (element :html
                  (element :head
                           (element :meta {:name "description"
                                           :content "co-compile test"})
                           (element :link {:rel "stylesheet" :href "/scripts/foo.css"} "BUG!")
                           (element :script {:src "/scripts/foo.js"}))
                  (element :body
                           (element :h1 "Hello World"))))

doc

(println doc)

(pprint doc)

(println (serialize doc))

(println (optimize :js doc))

(pprint (optimize :js doc))

(co-compile "resources/footest.html"
            (optimize :js doc)
            :pprint)

(def x "foo")

(println (serialize (element :link {:rel "stylesheet" :href="foo.css"})))

(println (element :foo {:bar ""}))

(x/emit-str (x/element :foo {:bar (* 2 3)}))
