(ns test
  (:require [miraj.markup :refer :all :exclude [normalize]]
            [miraj.html :as h]
            [clojure.tools.namespace.repl :refer [refresh]]
            ;; [polymer.paper]
            #_[clojure.data.xml :as x]))

(pprint
 (miraj.markup/normalize
  (with-meta
    (h/html
     (h/script {:src "foo.js"})
     (require '[scripts :as js :refer [jquery materialize]]
              '[styles :as css :refer [foo]])
     (h/body (h/h1 "hello")))
    {:title "hello" :description "foo" :base "http://foo"})))

(use 'miraj.markup :reload)
(use 'scripts :reload)
(use 'styles :reload)

(refresh)

(meta (with-meta (element :foo) {:a :b}))


scripts/jquery


(remove-ns 'polymer.paper)
(ns-unalias *ns* 'paper)

(paper/button)

(require '[polymer.paper :as paper :refer [button card]])

(paper/button)

(remove-ns 'polymer.iron)


(ns-unalias *ns* 'iron)
(create-ns 'polymer.iron)
(require '[polymer.iron :as iron :refer [icon list]])

(iron/label)

iron

(meta (find-ns 'polymer.iron))

polymer.iron/icon

(println (meta (find-var 'polymer.paper/button)))

(println (meta (find-var 'polymer.iron/icon)))

(println (meta (find-var 'clojure.core/list)))

;;(refresh)

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


;;(x/emit-str (x/element :foo {:bar (* 2 3)}))
