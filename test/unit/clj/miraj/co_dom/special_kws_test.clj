;   Copyright (c) Gregg Reynolds. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Test Special Keyword handling"
      :author "Gregg Reynolds"}
  miraj.co-dom.special-kws-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [miraj.co-dom :refer :all :exclude [import require]]))

;; special keywords:
;;    :#foo         =>  {:id "foo"}
;;    :.foo.bar     =>  {:class "foo bar"}
;;    :!foo         =>  {:foo __BOOLEAN}
;;    :{$foo "bar"} =>  {:style "foo:bar;"}

(deftest ^:docs specials-id
  (let [frag1 (element :foo :#bar)
        frag2 (element :foo :#bar {:x 99})
        frag3 (element :foo :#bar {:x 99} "Hello")]
    (is (= frag1
           #miraj.co_dom.Element{:tag :foo :attrs {:id "bar"} :content ()}))
    (is (= frag2
           #miraj.co_dom.Element{:tag :foo :attrs {:x 99 :id "bar"} :content ()}))
    (is (= frag3
           #miraj.co_dom.Element{:tag :foo :attrs {:id "bar" :x 99} :content ("Hello")}))))
