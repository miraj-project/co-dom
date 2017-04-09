(ns test_codom
  ;; (:refer-clojure :exclude [import])
  (:require [miraj.co-dom :refer :all]
            :reload))

(element :div {'@lname :name})
(element :div {:lname :$name})

(element :meta {:name "description"
                :content "miraj.co-dom test"})


;;;;;;;;;;;;;;;; special keyword attribs:  :#foo  :.foo  :!foo  :$foo  :@foo

(element :div :#foo)
(element :div :.bar.baz)

(element :div :!centered)
(serialize (element :div :!centered))

(element :div {:$color "blue"})
(pprint (element :div :#foo {:$link {:background "#ff0"}
                                :$visited {:background "#fff"}
                                :$hover {:outline "thin red solid"}
                                :$active {:background "#00f"}
                                :$color "green"
                                :$background-color "red"}))

(element :div {:@lname "Smith"})


(element :div :#foo.bar.baz!centered)

(element :div :#foo :.bar :.baz :!centered)

(element :div :#foo :.bar :.baz :!centered {:$color "blue"})

(element :div :#foo.bar.baz {:$hover {:background "blue"}})

(element :div :#foo :.bar :.baz :!centered {:$hover {:background "blue"}})


;;;;;;;;;;;;;;;;  binding annotations

;; property bindings
(serialize (element :div {:lname :name.last}))
(serialize (element :div {:lname 'name.last}))

(element :div {:lname '@name})
(element :div {'@lname :name})

;;  attribute bindings
(serialize (element :div {'@lname :name.last}))
(serialize (element :div {:lname 'name.last}))

;;  content bindings
(serialize (element :div :lname))
(serialize (element :div :name.last ", " :name.first))

;; void elements
(element :link {:rel "stylesheet" :href "foo/bar.css"})

;; nesting

(element :chapter (element :section))


;; loops etc.

(pprint (element :head
         (for [i (range 0 3)]
           (element :link {:rel "import" :href (str "foo" i ".html")}))))


;;;;;;;;;;;;;;;; templating

(defn frag [lname fname]
  (element :div
           (element :span "Hello there, ")
           (element :span fname " ")
           (element :span lname "!")))

(pprint (frag "Smith", "John"))

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

