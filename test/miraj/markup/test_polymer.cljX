(ns ^{:doc "Tests for polymer html functions"
      :author "Gregg Reynolds"}
  data.xml.test-polymer
  (:refer-clojure :exclude [map meta time])
  (:require ;;[miraj.core :refer :all]
            [miraj.html :as h :refer :all]
            [miraj.ml.polymer :as p :refer :all]
            ;; [hiccup.page :refer [html5]]
            [miraj.markup :as xml]
            [clojure.tools.logging :as log :only [trace debug error info]]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.edn :as edn]
            [clojure.tools.reader.reader-types :as readers]
            [clojure.test :refer :all]
            [miraj.markup]
            [data.xml.test-utils :refer [test-stream lazy-parse*]]))

(deftest ^:polymer test-1
  (testing "test 1"
    (is (= (h/div)
           #miraj.markup.Element{:tag :div, :attrs {}, :content ()}))
    (is (= (h/div {})
           #miraj.markup.Element{:tag :div, :attrs {}, :content ()}))
    (is (= (h/div {:class "test"})
           #miraj.markup.Element{:tag :div, :attrs {:class "test"}, :content ()}))
    (is (= (h/div "content")
           #miraj.markup.Element{:tag :div, :attrs {}, :content ("content")}))
    (is (= (h/div {} "content")
           #miraj.markup.Element{:tag :div, :attrs {}, :content ("content")}))
    (is (= (h/div {:class "test"} "content")
           #miraj.markup.Element{:tag :div, :attrs {:class "test"}, :content ("content")}))
    (is (= (h/div (h/span))
           #miraj.markup.Element{:tag :div, :attrs {}, :content (#miraj.markup.Element{:tag :span, :attrs {}, :content ()})}))
    (is (= (h/div {} (h/span))
           #miraj.markup.Element{:tag :div, :attrs {}, :content (#miraj.markup.Element{:tag :span, :attrs {}, :content ()})}))
    (is (= (h/div {} (h/span {}))
           #miraj.markup.Element{:tag :div, :attrs {}, :content (#miraj.markup.Element{:tag :span, :attrs {}, :content ()})}))
    (is (= (h/div "hello" (h/span))
           #miraj.markup.Element{:tag :div, :attrs {}, :content ("hello" #miraj.markup.Element{:tag :span, :attrs {}, :content ()})}))
    (is (= (h/div "hello" (h/span "world"))
           #miraj.markup.Element{:tag :div, :attrs {}, :content ("hello" #miraj.markup.Element{:tag :span, :attrs {}, :content ("world")})}))
    (is (= (h/div "hello" (h/span "world") "goodbye")
           #miraj.markup.Element{:tag :div, :attrs {}, :content ("hello" #miraj.markup.Element{:tag :span, :attrs {}, :content ("world")} "goodbye")}))
    ))

;; VOID and EMPTY elements
;; void != empty
;; html void elements  = xml elements with "nothing" content model
;; html voids:  http://www.w3.org/html/wg/drafts/html/master/syntax.html#void-elements
;; xml nothings: http://www.w3.org/html/wg/drafts/html/master/dom.html#concept-content-nothing
;; HTML serialization does not close void elts like link, br, etc.
;; "Void elements only have a start tag; end tags must not be specified for void elements."
;; the start tag of a void element *may* self-close
;; start tags: http://www.w3.org/html/wg/drafts/html/master/syntax.html#start-tags
;; "if the element is one of the void elements, or if the element is a
;; foreign element, then there may be a single U+002F SOLIDUS
;; character (/). This character has no effect on void elements, but
;; on foreign elements it marks the start tag as self-closing."

;; The critical point: voids with "/>" are NOT deemed self-closing;
;; the "/" is devoid of meaning, so to speak.  So "<link/>" is not a
;; self-closing tag, it's a start tag with an optional "/".  But
;; foreign tags may self-close, and we can infer that "normal" tags
;; must not self-close.

;; SERIALIZATION NOTES
;; Serialization to XML does not normalize, e.g. empties are not
;; collapsed: <p></p> does not become <h/>.

;; HTML mode does not add the optional "/" to void start tags,
;; e.g. <link> not <link />.

;; (def html5-void-elt-tags
;;   #{"area" "base" "br" "col"
;;    "embed" "hr" "img" "input"
;;    "keygen" "link" "meta" "param"
;;    "source" "track" "wbr"})

(deftest ^:polymer serialize-voids
  (testing "serialize html void elts"
    (doseq [tag html5-void-elt-tags]
      (is (= (xml/serialize ((resolve (symbol (str "miraj.html.polymer/" tag)))))
             (str "<" tag "></" tag ">")))
      (is (= (xml/serialize :xml ((resolve (symbol (str "miraj.html.polymer/" tag)))))
             (str "<" tag "></" tag ">")))
      (is (= (xml/serialize :html ((resolve (symbol (str "miraj.html.polymer/" tag)))))
             (str "<" tag ">"))))))

(deftest ^:polymer serialize-empties
  (testing "serialize html empty elts"
    (is (= (xml/serialize (h/p)) "<p></p>"))
    (is (= (xml/serialize :xml (h/p)) "<p></p>"))
    (is (= (xml/serialize :html (h/p)) "<p></p>"))))

;; void elts
(deftest ^:polymer pprint-voids
  (testing "pprint html void elts"
    (is (= (with-out-str (xml/pprint (h/link))) "<link/>\n\n"))
    (is (= (with-out-str (xml/pprint :xml (h/link)) "<link/>\n\n")))
    ;; note void elt not close with "/"
    (is (= (with-out-str (xml/pprint :html (h/link)) "<link>\n\n")))))

(deftest ^:polymer pprint-empties
  (testing "pprint html empty elts"
    (is (= (with-out-str (xml/pprint (h/body)) "<body/>\n\n")))
    (is (= (with-out-str (xml/pprint :xml (h/body)) "<body/>\n\n")))
    (is (= (with-out-str (xml/pprint :html (h/body)) "<body/>\n\n")))))

;; POLYMER elts
(xml/serialize (p/dom-module))
(xml/serialize (p/dom-repeat))
(xml/serialize (h/template))

(print (p/list-polymer-elts))


;; STREAMS
;; args to serialize and pprint need not be a tree, may be a forest
(print (xml/serialize (h/div) (h/div)))
(xml/serialize :xml (h/div) (h/div))
(xml/serialize :html (h/div) (h/div))

(xml/serialize (h/link) (h/link))
(xml/serialize :xml (h/link) (h/link))
(xml/serialize :html (h/link) (h/link))

;; (xml/pprint (h/div) (h/div))
;; (xml/pprint :xml (h/div) (h/div))
;; (xml/pprint :html (h/div) (h/div))

;; (xml/pprint (h/link) (h/link))

;; (xml/pprint :html
;;  (xml/serialize
;;   (h/html
;;    (h/link {:rel "import" :href "/polymer/polymer/polymer.html"})
;;    (h/link {:rel "import" :href "/polymer/polymer/foo.html"})
;;   (h/div
;;    (h/ul
;;     (h/li (h/span "bar") (h/span "baz"))
;;     (h/li (h/span "bar2") (h/span "baz2"))
;;     )))))


