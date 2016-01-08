;   Copyright (c) Gregg Reynolds. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Test resource handling"
      :author "Gregg Reynolds"}
  miraj.markup.resources-test
  (:refer-clojure :exclude [import require])
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [miraj.markup :refer :all]))

(pprint (require '[polymer.paper :as paper :refer [button]]))

(deftest ^:resources res-1
  (let [e (serialize (require '[polymer.paper :as paper :refer [button]]))]
    (is (= e
           "<link rel=\"import\" href=\"bower_components/paper-button/paper-button.html\">"))))

(deftest ^:resources res-2
  (let [e (serialize (require '[polymer.paper :as paper :refer [button]]))]
    (is (= e
           "<link rel=\"import\" href=\"bower_components/paper-button/paper-button.html\">"))
    (is (= (serialize (paper/button)) "<paper-button></paper-button>"))))

