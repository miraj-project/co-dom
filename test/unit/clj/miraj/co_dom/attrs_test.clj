;   Copyright (c) Gregg Reynolds. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Test attribute handing"
      :author "Gregg Reynolds"}
  miraj.co-dom.attrs-test
  (:refer-clojure :exclude [import require])
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
;;            [miraj.html :as h]
            [miraj.style :as s]
            [miraj.co-dom :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ATTRIBUTE tests

(deftest ^:attrs attr-1
  (let [e (element :div {:foo "secret"})]
    (is (= e #miraj.co_dom.Element{:tag :div, :attrs {:foo "secret"}, :content ()}))))

(deftest ^:attrs attr-2
  (let [e (element :div {:foo "secret" :bar 99})]
    (is (= e #miraj.co_dom.Element{:tag :div, :attrs {:foo "secret" :bar 99}, :content ()}))
    (is (= (serialize e) "<div bar=\"99\" foo=\"secret\"></div>"))))

(deftest ^:attrs attr-3
  (let [e (element :div {:foo "secret" :bar 99 :baz true})]
    (is (= e #miraj.co_dom.Element{:tag :div, :attrs {:foo "secret" :bar 99 :baz true}, :content ()}))
    (is (= (serialize e) "<div baz=\"true\" bar=\"99\" foo=\"secret\"></div>"))))

;; Clojurization.  All attribute names are expressed as keywords;
;; HTML5-defined attributes are available in clojure-case;
;; e.g. :cross-origin instead of :crossorigin.  Exceptions: ordinary
;; compound words are not split,
;; e.g. :bookmark, :prefetch, :stylesheet, :sidebar.

(deftest ^:attrs namecase-0
  (testing "OBSOLETE: HTML attr names are case-insensitive.  Currently we only allow camel-case (lower-case with dashes); this will be relaxed in a later version."
    (let [e (element :div {:fooBar "baz"})]
      (is (= e #miraj.co_dom.Element{:tag :div, :attrs {:fooBar "baz"}, :content ()}))
      (is (= (serialize e) "<div fooBar=\"baz\"></div>"))
      #_(is (thrown-with-msg? Exception #"HTML attribute names are case-insensitive; currently, only lower-case is allowed."
                            (serialize e)))
      #_(is (thrown? Exception (serialize (element :div {:aB "foo"})))))))

(deftest ^:attrs namecase-1
  (testing "OBSOLETE: HTML serialization converts clojure-case attr names."
    (let [e (element :div {:context-menu "foo"})]
      (is (= e #miraj.co_dom.Element{:tag :div, :attrs {:context-menu "foo"}, :content ()}))
      #_(is (= (serialize :xml e) "<div context-menu=\"foo\"></div>"))
      #_(is (= (serialize e) "<div contextmenu=\"foo\"></div>")))))

;; (deftest ^:attrs bool-2
;;   (testing "XML serialization barfs on boolean attribs."
;;     (is (thrown-with-msg? Exception #"Clojure nil attribute val disallowed"
;;                           (serialize :xml  (element :body {:unresolved nil}))))))

;; Enumerated attribute values.  HTML5 defines restrictions on some
;; attributes; for example, the "rel" attribute values must come from
;; a list of link types.  Serialization validates (some of) these.
(deftest ^:attrs val-4
  (let [e (element :link {:rel "author"})]
    (is (= e #miraj.co_dom.Element{:tag :link, :attrs {:rel "author"}, :content ()}))
    (is (= (serialize e) "<link rel=\"author\">"))))

(deftest ^:attrs val-5
  (testing "Strings and keywords allowed for vals for :rel attrib.
  NOTE: the assumption is that we do not want to use Polymer two-way
  binding for this attribute, e.g. rel='{{foo}}'.  If that turns out
  to be a bad assumption support for keyword vals will be removed.
NB: NOT YET IMPLEMENTED"
    #_(is (= (serialize (element :link {:rel :stylesheet}))
           "<link rel=\"stylesheet\">"))
    (is (= (serialize (element :link {:rel "stylesheet"}))
           "<link rel=\"stylesheet\">"))))

;; (deftest ^:attrs val-6
;;   (let [e (element :link {:rel "foo"})]
;;     (is (thrown-with-msg? Exception #"Invalid link type value for rel attribute:"
;;                           (serialize e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shortcuts
(deftest ^:attrs ^:sugar id-sugar-1
  (testing "id shortcut"
    (is (= (serialize (element :span :#foo)) "<span id=\"foo\"></span>"))))

(deftest ^:attrs ^:sugar id-sugar-2
  (testing "id shortcut"
    (is (= (serialize (element :span :#foo "bar")) "<span id=\"foo\">bar</span>"))))

(deftest ^:attrs ^:sugar id-sugar-3
  (testing "id shortcut"
    (is (= "<span bar=\"99\" id=\"foo\">baz</span>"
           (serialize (element :span :#foo {:bar 99} "baz"))))))

(deftest ^:attrs ^:sugar class-sugar-1
  (testing "id shortcut"
    (is (= (serialize (element :span :.foo)) "<span class=\"foo\"></span>"))))

(deftest ^:attrs ^:sugar class-sugar-2
  (testing "id shortcut"
    (is (= (serialize (element :span :.foo "bar")) "<span class=\"foo\">bar</span>"))))

(deftest ^:attrs ^:sugar id-class-sugar-1
  (testing "id shortcut"
    (is (= (serialize (element :span :#foo.bar)) "<span id=\"foo\" class=\"bar\"></span>"))))

(deftest ^:attrs ^:sugar id-class-sugar-2
  (testing "id shortcut"
    (is (= (serialize (element :span :#foo.bar "baz")) "<span id=\"foo\" class=\"bar\">baz</span>"))))

;; Boolean attributes.  An HTML boolean attribute is an attribute
;; without a value.  Use :!
(deftest ^:attrs bool-sugar-1
  (let [e (element :body :!unresolved)]
    (is (= "<body unresolved></body>" (serialize e)))))

(deftest ^:attrs bool-sugar-2
  (let [e (element :div :!centered {:foo 99} "bar")]
    (is (= "<div foo=\"99\" centered>bar</div>" (serialize e)))))

(deftest ^:attrs bool-sugar-3
  (let [e (element :div :!centered!horizontal)]
    (is (= "<div centered horizontal></div>"
           (serialize e)))))

(deftest ^:attrs style-sugar-1
  (let [e (element :div {::s/color "red"} "hello")]
    (is (= "<div style=\"color:red;\">hello</div>"
           (serialize e)))))

;; pseudo elts also supported
(deftest ^:attrs style-sugar-2
  (let [e (element :div :#foo {::s/hover {:background "blue"}
                               ::s/background "green"} "hello")]
    (is (= "<div style=\"background:green;\" id=\"foo\">hello</div>"
           (serialize e)))))

;; special kws concatenate
(deftest ^:attrs sugars-1
  (let [e (element :div :#foo.bar.baz!centered)]
    (is (= "<div id=\"foo\" class=\"bar baz\" centered></div>"
           (serialize e)))))

;; order is insignificant
(deftest ^:attrs sugars-2
  (let [e (element :div :.bar#foo!centered.baz)]
    (is (= "<div id=\"foo\" class=\"bar baz\" centered></div>"
           (serialize e)))))

;; special kws need not be concatenated
(deftest ^:attrs sugars-2
  (let [e (element :div :#foo :.bar :.baz :!centered)]
    (is (= "<div id=\"foo\" class=\"bar baz\" centered></div>"
           (serialize e)))))

(deftest ^:attrs sugars-3
  (let [e (element :div :#foo :.bar :.baz :!centered {::s/color "blue"})]
    (is (= "<div style=\"color:blue;\" id=\"foo\" class=\"bar baz\" centered></div>"
           (serialize e)))))

#_(deftest ^:attrs sugars-4
  (let [e (element :div :#foo.bar.baz {::s/hover {:background "blue"}})]
    (is (= x
           (serialize e)))))

#_(deftest ^:attrs sugars-5
  (let [e (element :div :#foo :.bar :.baz :!centered {::s/hover {:background "blue"}})]
    (is (= x
           (serialize e)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Polymer binding annotations.  Symbol attribute values
;; serialize as [[one-way]] annotations; keyword attribute
;; values serialize as {{two-way}} annotations.

(deftest ^:attrs binding-1way-1
  (testing "Polymer one-way annotation"
    (let [e (element :div {:foo 'secret})]
      (is (= (serialize e)
          "<div foo=\"[[secret]]\"></div>")))))

(deftest ^:attrs binding-1way-2
  (testing "Polymer one-way annotation"
    (let [e (element :div {:foo 'secret} 'bar)]
      (is (= (serialize e)
          "<div foo=\"[[secret]]\">[[bar]]</div>")))))

(deftest ^:attrs binding-1way-3
  (let [e (element :div {:foo 'secret})]
    (is (= e #miraj.co_dom.Element{:tag :div, :attrs {:foo secret}, :content ()}))
    (is (= "<div foo=\"[[secret]]\"></div>" (serialize e)))
    (is (= clojure.lang.Symbol (type (:foo (:attrs e)))))
    (is (= (:foo (:attrs e)) 'secret))))

(deftest ^:attrs binding-2way-1
  (testing "Polymer two-way annotation"
    (let [e (element :div {:foo :secret})]
      (is (= (serialize e)
          "<div foo=\"{{secret}}\"></div>")))))

(deftest ^:attrs binding-2way-2
  (testing "Polymer two-way annotation"
    (let [e (element :div {:foo :secret} :bar)]
      (is (= (serialize e)
          "<div foo=\"{{secret}}\">{{bar}}</div>")))))

(deftest ^:attrs binding-2way-3
  (let [e (element :div {:foo :secret})]
    (is (= e #miraj.co_dom.Element{:tag :div, :attrs {:foo :secret}, :content ()}))
    (is (= "<div foo=\"{{secret}}\"></div>" (serialize e)))))


