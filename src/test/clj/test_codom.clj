(ns test_codom
  (:refer-clojure :exclude [import])
  (:require [miraj.core :as miraj]
            [miraj.co-dom :as x]
            [miraj.html :as h]
            :reload))

;; (def doc (element :html
;;                   (element :head
;;                            (element :meta {:name "description"
;;                                            :content "miraj.co-dom test"})
;;                            (element :script {:src "/scripts/foo.js"}))
;;                   (element :body
;;                            (element :h1 "Hello World"))))

;; (println doc)

;; (x/serialize doc)

;; #_(x/pprint (element :div :#foo.bar!centered
;;                     {:$background-color "red"
;;                      :bar 99}))

;;(remove-ns 'miraj.co-dom)
;;(remove-ns 'test_codom)
;;(require '[miraj.co-dom :as codom] :reload :verbose)

(x/pprint
 (h/html
 (h/body
  (h/div :#myid.foo {:$hover {:content "afdeagd"}}
         (h/button :!foobar))
  (h/div
   (h/span  :#myspan.foo.bar {:lname "Jones"
                              :$hover {:background-color "green"}}
            :!centered "hello"))))
)

;;(ns-unalias *ns* 'codom)

;; (x/pprint
;;  (h/div
;;   (h/span :.centered {:$color "green"
;;                       :foo 99
;;                       :$hover {:background "red" :color "blue"}
;;                             })
;;   )
;;  )

;; (pprint doc)

;; (println (optimize :js doc))

;; (pprint (optimize :js doc))

;; (co-compile "resources/footest.html"
;;             (optimize :js doc)
;;             :pprint)
