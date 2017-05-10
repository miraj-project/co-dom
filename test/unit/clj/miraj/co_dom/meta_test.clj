;   Copyright (c) Gregg Reynolds. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Test html doc handling"
      :author "Gregg Reynolds"}
  miraj.co-dom.meta-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [miraj.co-dom :refer :all :exclude [import require]]))

(def doc (element :html
                  (element :head
                           (element :meta {:application-name "co-dom test"})
                           (element :script {:src "foo/bar.css"}))
                  (element :body
                           (element :h1 "Hello world")
                           (element :script {:src "foo/baz.css"}))))

(deftest ^:docs meta-1
  (let [frag (element :meta {:name "description"
                             :content "Search the world's information..."})]
    (is (= frag
           #miraj.co_dom.Element{:tag :meta
                                 :attrs {:content "Search the world's information..."
                                         :name "description"}
                                 :content ()}))

    (is (= (serialize frag)
           "<meta content=\"Search the world's information...\" name=\"description\">"))))

