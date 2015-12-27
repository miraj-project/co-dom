;   Copyright (c) Gregg Reynolds. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Test element handling"
      :author "Gregg Reynolds"}
  miraj.markup.elts-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [miraj.markup :refer :all]))

(deftest ^:elts defelt
  (let [e (element :foo)]
    (is (= e #miraj.markup.Element{:tag :foo, :attrs {}, :content ()}))))

(deftest ^:elts defeltmeta
  (let [e (with-meta (element :foo) {:bar :baz})]
    (is (= e #miraj.markup.Element{:tag :foo, :attrs {}, :content ()}))
    (is (= (meta e) {:bar :baz}))))

(deftest ^:elts as-fragment
  (let [input (list [:tag1 "stuff"]
                    [:tag2 "other"])]
    (is (= (sexps-as-fragment input)
           (map sexp-as-element input)))
    (is (thrown? Exception (sexp-as-element input)))))

(deftest ^:elts elts-1
  (testing "ctor permutations"
    (is (= (element :foo)
           #miraj.markup.Element{:tag :foo, :attrs {}, :content ()}))
    (is (= (element :foo {})
           #miraj.markup.Element{:tag :foo, :attrs {}, :content ()}))
    (is (= (element :foo {:class "bar"})
           #miraj.markup.Element{:tag :foo, :attrs {:class "bar"}, :content ()}))
    (is (= (element :foo "baz")
           #miraj.markup.Element{:tag :foo, :attrs {}, :content ("baz")}))
    (is (= (element :foo {} "baz")
           #miraj.markup.Element{:tag :foo, :attrs {}, :content ("baz")}))
    (is (= (element :foo {:class "bar"} "baz")
           #miraj.markup.Element{:tag :foo, :attrs {:class "bar"}, :content ("baz")}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTML element tests

;; (def html5-void-elt-tags
;;   #{"area" "base" "br" "col"
;;    "embed" "hr" "img" "input"
;;    "keygen" "link" "meta" "param"
;;    "source" "track" "wbr"})

(deftest ^:html void-1
    (is (= html5-void-elts
           #{"area" "base" "br" "col" "embed" "hr" "img" "input"
             "keygen" "link" "meta" "param" "source" "track" "wbr"})))

;; HTML5 void elements look just like standard elements in the node tree.
(deftest ^:html void-2
  (let [e (element :link)]
    (is (= e #miraj.markup.Element{:tag :link, :attrs {}, :content ()}))))

;; examples from https://developer.mozilla.org/en-US/docs/Web/HTML/Element/link
(deftest ^:html void-3
  (let [link0 (element :link {:href "style.css" :rel="stylesheet"})
        link1 (element :link {:href "default.css" :rel="stylesheet" :title "Default Style"})
        link2 (element :link {:href "fancy.css" :rel="alternate stylesheet" :title "Fancy"})
        link3 (element :link {:href "basic.css" :rel="alternate stylesheet" :title "Basic"})]
    (is (= link0 #miraj.markup.Element{:tag :link,
                                   :attrs {:href "style.css" :rel="stylesheet"},
                                   :content ()}))
    (is (= link1 #miraj.markup.Element{:tag :link,
                                   :attrs {:href "default.css" :rel="stylesheet" :title "Default Style"},
                                   :content ()}))
    (is (= link2 #miraj.markup.Element{:tag :link,
                                   :attrs {:href "fancy.css" :rel="alternate stylesheet"
                                           :title "Fancy"},
                                   :content ()}))
    (is (= link3 #miraj.markup.Element{:tag :link,
                                   :attrs {:href "basic.css" :rel="alternate stylesheet"
                                           :title "Basic"},
                                   :content ()}))))

;; HTML5 void elements serialization
(deftest ^:html void-4
  (testing "XML serialization of void element to empty, non-self-closing XML element"
    (let [link0 (element :link)
          link1 (element :link {:href "style.css" :rel="stylesheet"})]
      (is (= (serialize :xml link0)
             "<link></link>")))))

(deftest ^:html void-5
  (testing "HTML serialization of void element to unclosed start tag"
    (let [link0 (element :link)
          link1 (element :link {:href "style.css" :rel="stylesheet"})]
      (is (= (serialize :html link0)
             "<link>")))))

(deftest ^:html void-6
  (testing "Default serialization is :html"
    (let [link0 (element :link)
          link1 (element :link {:href "style.css" :rel="stylesheet"})]
      (is (= (serialize link0)
             "<link>")))))

(deftest ^:html void-7
  (testing "Serialization never generates self-closing tags."
    (let [link (element :link)
          div (element :div)]
      (is (= (serialize link) "<link>"))
      (is (= (serialize :html link) "<link>"))
      (is (= (serialize :xml link) "<link></link>"))
      (is (= (serialize div) "<div></div>"))
      (is (= (serialize :html div) "<div></div>"))
      (is (= (serialize :xml div) "<div></div>")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ATTRIBUTE tests

(deftest ^:attrs attr-1
  (let [e (element :div {:class "secret"})]
    (is (= e #miraj.markup.Element{:tag :div, :attrs {:class "secret"}, :content ()}))))

(deftest ^:attrs attr-2
  (let [e (element :div {:class :secret})]
    (is (= e #miraj.markup.Element{:tag :div, :attrs {:class :secret}, :content ()}))))

(deftest ^:attrs attr-3
  (let [e (element :div {:class 'secret})]
    (is (= e #miraj.markup.Element{:tag :div, :attrs {:class secret}, :content ()}))
    (is (= (type (:class (:attrs e) symbol))))
    (is (= (:class (:attrs e) 'secret)))))

;; Clojurization.  All attribute names are expressed as keywords;
;; HTML5-defined attributes are available in clojure-case;
;; e.g. :cross-origin instead of :crossorigin.  Exceptions: ordinary
;; compound words are not split,
;; e.g. :bookmark, :prefetch, :stylesheet, :sidebar.

(deftest ^:attrs namecase-0
  (testing "HTML attr names are case-insensitive.  Currently we only allow camel-case (lower-case with dashes); this will be relaxed in a later version."
    (let [e (element :div {:fooBar "baz"})]
      (is (= e #miraj.markup.Element{:tag :div, :attrs {:fooBar "baz"}, :content ()}))
      (is (= (serialize :xml e) "<div fooBar=\"baz\"></div>"))
      (is (thrown-with-msg? Exception #"HTML attribute names are case-insensitive; currently, only lower-case is allowed."
                            (serialize e)))
      (is (thrown? Exception (serialize (element :div {:aB "foo"})))))))

(deftest ^:attrs namecase-1
  (testing "HTML serialization converts clojure-case attr names."
    (let [e (element :div {:context-menu "foo"})]
      (is (= e #miraj.markup.Element{:tag :div, :attrs {:context-menu "foo"}, :content ()}))
      (is (= (serialize :xml e) "<div context-menu=\"foo\"></div>"))
      (is (= (serialize e) "<div contextmenu=\"foo\"></div>")))))

;; Boolean attributes.  An HTML boolean attribute is an attribute
;; without a value.  Use nil as attrib value to express this.
(deftest ^:attrs bool-1
  (let [e (element :body {:unresolved nil})]
    (is (= e #miraj.markup.Element{:tag :body, :attrs {:unresolved nil}, :content ()}))
    (is (= (serialize e) "<body unresolved></body>"))))

(deftest ^:attrs bool-2
  (testing "XML serialization barfs on boolean attribs."
    (is (thrown-with-msg? Exception #"Clojure nil attribute val disallowed"
                          (serialize :xml  (element :body {:unresolved nil}))))))

;; Enumerated attribute values.  HTML5 defines restrictions on some
;; attributes; for example, the "rel" attribute values must come from
;; a list of link types.  Serialization validates (some of) these.
(deftest ^:attrs attr-4
  (let [e (element :link {:rel "author"})]
    (is (= e #miraj.markup.Element{:tag :link, :attrs {:rel "author"}, :content ()}))
    (is (= (serialize e) "<link rel=\"author\">"))))

(deftest ^:attrs attr-5
  (testing "Strings and keywords allowed for vals for :rel attrib.
  NOTE: the assumption is that we do not want to use Polymer two-way
  binding for this attribute, e.g. rel='{{foo}}'.  If that turns out
  to be a bad assumption support for keyword vals will be removed."
    (is (= (serialize (element :link {:rel :stylesheet}) "<link rel=\"stylesheet\">")))
    (is (= (serialize (element :link {:rel "stylesheet"}) "<link rel=\"stylesheet\">")))))

(deftest ^:attrs attr-6
  (let [e (element :link {:rel "foo"})]
    (is (thrown-with-msg? Exception #"Invalid link type value for rel attribute:"
                          (serialize e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Polymer annotations.  Symbol attribute values serialize as [[one-way]]
;; annotations; keyword attribute values serialize as {{two-way}}
;; annotations.
(deftest ^:attrs annot-1
  (testing "Polymer one-way annotation"
    (let [e (element :div {:class 'secret})]
      (is (= (serialize e))
          "<div class=\"[[secret]]\"></div>"))))

(deftest ^:attrs annot-2
  (testing "Polymer two-way annotation"
    (let [e (element :div {:class :secret})]
      (is (= (serialize e))
          "<div class=\"{{secret}}\"></div>"))))


