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
  (:require ;;[org.clojure/clojure "1.8.0-RC4"]
            [clojure.java.io :as io]
            [clojure.test :refer :all]
;;            [miraj.html :as h]
            [miraj.co-dom :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ATTRIBUTE tests

(deftest ^:attrs attr-1
  (let [e (element :div {:class "secret"})]
    (is (= e #miraj.co_dom.Element{:tag :div, :attrs {:class "secret"}, :content ()}))))

(deftest ^:attrs attr-2
  (let [e (element :div {:class :secret})]
    (is (= e #miraj.co_dom.Element{:tag :div, :attrs {:class :secret}, :content ()}))))

(deftest ^:attrs attr-3
  (let [e (element :div {:class 'secret})]
    (is (= e #miraj.co_dom.Element{:tag :div, :attrs {:class secret}, :content ()}))
    (is (= (type (:class (:attrs e) symbol))))
    (is (= (:class (:attrs e) 'secret)))))

;; Clojurization.  All attribute names are expressed as keywords;
;; HTML5-defined attributes are available in clojure-case;
;; e.g. :cross-origin instead of :crossorigin.  Exceptions: ordinary
;; compound words are not split,
;; e.g. :bookmark, :prefetch, :stylesheet, :sidebar.

(deftest ^:attrs namecase-0
  (testing "OBSOLETE: HTML attr names are case-insensitive.  Currently we only allow camel-case (lower-case with dashes); this will be relaxed in a later version."
    (let [e (element :div {:fooBar "baz"})]
      (is (= e #miraj.co_dom.Element{:tag :div, :attrs {:fooBar "baz"}, :content ()}))
      (is (= (serialize :xml e) "<div fooBar=\"baz\"></div>"))
      #_(is (thrown-with-msg? Exception #"HTML attribute names are case-insensitive; currently, only lower-case is allowed."
                            (serialize e)))
      #_(is (thrown? Exception (serialize (element :div {:aB "foo"})))))))

(deftest ^:attrs namecase-1
  (testing "OBSOLETE: HTML serialization converts clojure-case attr names."
    (let [e (element :div {:context-menu "foo"})]
      (is (= e #miraj.co_dom.Element{:tag :div, :attrs {:context-menu "foo"}, :content ()}))
      (is (= (serialize :xml e) "<div context-menu=\"foo\"></div>"))
      #_(is (= (serialize e) "<div contextmenu=\"foo\"></div>")))))

;; Boolean attributes.  An HTML boolean attribute is an attribute
;; without a value.  Use nil as attrib value to express this.
(deftest ^:attrs bool-1
  (let [e (element :body {:unresolved nil})]
    (is (= e #miraj.co_dom.Element{:tag :body, :attrs {:unresolved nil}, :content ()}))
    (is (= (serialize e) "<body unresolved></body>"))))

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
(deftest ^:attrs ^:sugar sugar-1
  (testing "id shortcut"
    (is (= (serialize (element :span ::foo)) "<span id=\"foo\"></span>"))))

(deftest ^:attrs ^:sugar sugar-2
  (testing "id shortcut"
    (is (= (serialize (element :span ::.foo)) "<span class=\"foo\"></span>"))))

(deftest ^:attrs ^:sugar sugar-3
  (testing "id shortcut"
    (is (= (serialize (element :span ::foo.bar)) "<span id=\"foo\" class=\"bar\"></span>"))))

(deftest ^:attrs ^:sugar sugar-4
  (testing "id shortcut"
    (is (= (serialize (element :span ::foo "bar")) "<span id=\"foo\">bar</span>"))))

(deftest ^:attrs ^:sugar sugar-5
  (testing "id shortcut"
    (is (= (serialize (element :span ::.foo "bar")) "<span class=\"foo\">bar</span>"))))

(deftest ^:attrs ^:sugar sugar-6
  (testing "id shortcut"
    (is (= (serialize (element :span ::foo.bar "baz")) "<span id=\"foo\" class=\"bar\">baz</span>"))))

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
  (testing "Polymer one-way annotation"
    (let [e (element :div {:class 'secret} 'foo)]
      (is (= (serialize e))
          "<div class=\"[[secret]]\">[[foo]]</div>"))))

(deftest ^:attrs annot-3
  (testing "Polymer two-way annotation"
    (let [e (element :div {:class :secret})]
      (is (= (serialize e))
          "<div class=\"{{secret}}\"></div>"))))

(deftest ^:attrs annot-4
  (testing "Polymer two-way annotation"
    (let [e (element :div {:class :secret} :foo)]
      (is (= (serialize e))
          "<div class=\"{{secret}}\">{{foo}}</div>"))))


