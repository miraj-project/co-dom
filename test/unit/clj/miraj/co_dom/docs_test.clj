;   Copyright (c) Gregg Reynolds. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Test html doc handling"
      :author "Gregg Reynolds"}
  miraj.co-dom.docs-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [miraj.co-dom :refer :all :exclude [import require]]))

(def doc (element :html
                  (element :head
                           (element :meta {:application-name "co-dom test"})
                           (element :script {:src "foo/bar.js"}))
                  (element :body
                           (element :h1 "Hello world")
                           (element :script {:src "foo/baz.js"}))))

(deftest ^:docs doc-1
  (let [doc (element :html
                   (element :head
                            (element :meta {:name "description"
                                            :content "Search the world's information..."})
                   (element :body
                            (element :input))))]
    (is (= (serialize doc)
           "<!doctype html>\n<html><head><meta content=\"Search the world's information...\" name=\"description\"><body><input></body></head></html>"))))

(deftest ^:docs serialize-1
  (testing "serialize doc"
    (is (= (serialize doc)
           "<!doctype html>\n<html><head><meta application-name=\"co-dom test\"><script src=\"foo/bar.js\"></script></head><body><h1>Hello world</h1><script src=\"foo/baz.js\"></script></body></html>"))))

(deftest ^:docs optimize-1
  (testing "JS optimizer adds charset, moves <script> elements from <head> to bottom of <body>."
    (is (= (xsl-xform js-optimizer doc) ;; (normalize doc))
           #miraj.co_dom.Element{:tag :html, :attrs {}, :content (#miraj.co_dom.Element{:tag :head, :attrs {}, :content (#miraj.co_dom.Element{:tag :meta, :attrs {:application-name "co-dom test"}, :content ()})} #miraj.co_dom.Element{:tag :body, :attrs {}, :content (#miraj.co_dom.Element{:tag :h1, :attrs {}, :content ("Hello world")} #miraj.co_dom.Element{:tag :script, :attrs {:src "foo/baz.js"}, :content ()} #miraj.co_dom.Element{:tag :script, :attrs {:src "foo/bar.js"}, :content ()})})}))))

(deftest ^:docs optimize-2
  (testing "JS optimizer moves <script> elements from <head> to bottom of <body>."
    (is (= "<!doctype html>\n<html><head><meta application-name=\"co-dom test\"></head><body><h1>Hello world</h1><script src=\"foo/baz.js\"></script><script src=\"foo/bar.js\"></script></body></html>"
           (serialize (xsl-xform js-optimizer doc))))))
    ;; #_(pprint (optimize :js (normalize doc))))

