;   Copyright (c) Gregg Reynolds. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Test html doc handling"
      :author "Gregg Reynolds"}
  miraj.markup.docs-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [miraj.markup :refer :all]))

(def doc (element :html
                  (element :head
                           (element :meta {:application-name "markup test"})
                           (element :script {:src "foo/bar.css"}))
                  (element :body
                           (element :h1 "Hello world")
                           (element :script {:src "foo/baz.css"}))))

(deftest ^:docs doc-1
  (let [doc (element :html
                   (element :head
                            (element :meta {:name "description"
                                            :content "Search the world's information..."})
                   (element :body
                            (element :input))))]
    (is (= (serialize doc)
           "<!doctype html><html><head><meta name=\"description\" content=\"Search the world's information...\"><body><input></body></head></html>"))))

(deftest ^:docs optimize-1
  (testing "JS optimizer moves <script> elements from <head> to bottom of <body>."
    (is (= (optimize :js doc)
           #miraj.markup.Element{:tag :html, :attrs {}, :content (#miraj.markup.Element{:tag :head, :attrs {}, :content (#miraj.markup.Element{:tag :meta, :attrs {:application-name "markup test"}, :content ()})} #miraj.markup.Element{:tag :body, :attrs {}, :content (#miraj.markup.Element{:tag :h1, :attrs {}, :content ("Hello world")} #miraj.markup.Element{:tag :script, :attrs {:src "foo/bar.css"}, :content ()} #miraj.markup.Element{:tag :script, :attrs {:src "foo/baz.css"}, :content ()})})}))
    (is (= (serialize (optimize :js doc))
           "<!doctype html><html><head><meta applicationname=\"markup test\"></head><body><h1>Hello world</h1><script src=\"foo/bar.css\"></script><script src=\"foo/baz.css\"></script></body></html>"))
    (pprint (optimize :js doc))))

