(ns test_codom
  ;; (:refer-clojure :exclude [import])
  (:require [miraj.co-dom :refer :all]
            [miraj.style :as s]
            :reload))

(element :div {:lname :$name})

(element :meta {:name "description"
                :content "miraj.co-dom test"})


;;;;;;;;;;;;;;;; special keyword attribs:  :#foo  :.foo  :!foo  :$foo  :@foo

(element :div :#foo)
(element :div :.bar.baz)

(element :div :!centered)
(serialize (element :div :!centered))

(element :div {:$color "blue"})

(element :div {::s/color "blue"
               :foo 99})

(def x
 (element :div :#foo {::s/color "blue"
                      ::s/hover {:color "red"}
                      :foo 99})
)

(pprint x)
(pprint (-> x meta :miraj/pseudo))


(pprint (element :div :#foo {::s/color "green"
                             ::s/background-color "red"}))

(def x
  (element :div :#foo {::s/color "green"
                       ::s/background-color "red"
                       ::s/active {:background "#00f"}
                       ::s/hover {:outline "thin red solid"}}))

(serialize x)

(serialize (-> x meta :miraj/pseudo))

(def x
  (element :div :#foo {::s/link {:background "#ff0"}
                       ::s/visited {:background "#fff"}
                       ::s/hover {:outline "thin red solid"}
                       ::s/active {:background "#00f"}
                       ::s/color "green"
                       ::s/background-color "red"})
)

(serialize x)

(serialize (-> x meta :miraj/pseudo))

(element :div :#foo.bar.baz!centered)

(element :div :#foo :.bar :.baz :!centered)

(element :div :#foo :.bar :.baz :!centered {::s/color "blue"})

(element :div :#foo.bar.baz {::s/hover {:background "blue"}})

(element :div :#foo :.bar :.baz :!centered {::s/hover {:background "blue"}})

;;;;;;;;;;;;;;;;  binding annotations DEPRECATED - polymer only!
;; use polymer.dom/lambda and miraj.core/defcomponent binding forms instead

;; property bindings
(serialize (element :div {:lname :name.last}))
(serialize (element :div {:lname 'name.last}))


;;  attribute bindings
(def x (element :div {:lname  :name.last
                      :miraj.polymer/fname "name.first"
                      :foo.bar/baz "buz"}
                (element :span "hello")))

(binding [*pprint* true]
  (println (serialize x)))

(serialize-raw x)

(println (normalize-html-str (pprint x)))

(def x (element :div :#foo {:lname :name.last
                            :fname$ :name.first
                            :miraj.style/color "blue"
                            :miraj.style/hover {:background "red"}}))
x

(binding [*pprint* true]
  (println (serialize x)))

(pprint x)

;;(normalize-html-str (serialize-raw x))

(serialize (-> x meta :miraj/pseudo))


(serialize (element :div {:$lname 'name.last}))

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
;;                     {::s/background-color "red"
;;                      :bar 99}))

