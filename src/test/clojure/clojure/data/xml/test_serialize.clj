(ns ^{:doc "Tests for serialization"
      :author "Gregg Reynolds"}
  clojure.data.xml.test-serialize
  (:require [miraj.core :refer :all]
            [miraj.html.polymer :as h :refer :all]
            [hiccup.page :refer [html5]]
            [clojure.data.xml :as xml]
            [clojure.tools.logging :as log :only [trace debug error info]]
            [clojure.pprint :as pp]
            [clojure.string :as string]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.edn :as edn]
            [clojure.tools.reader.reader-types :as readers]
            [clojure.test]
            [clojure.data.xml]
            [clojure.data.xml.test-utils :refer [test-stream lazy-parse*]]))

(xml/pprint
 (xml/serialize :with-xml-declaration
  (h/html
   (h/link {:rel "import" :href "/polymer/polymer/polymer.html"})
   (h/link {:rel "import" :href "/polymer/polymer/foo.html"})
  (h/div
   (h/ul
    (h/li (h/span "bar") (h/span "baz"))
    (h/li (h/span "bar2") (h/span "baz2"))
    )))))

(xml/pprint :html
 (xml/serialize :html
  ;; <link rel="import" href="../../polymer/polymer/polymer.html">
 (let [forms (co-type my-widget
                         "docstring"
                         (:require [into]  ;; "polymer/polymer/polymer.html"
                                   ;; => <link rel="import" href="polymer/polymer/polymer.html">
                                   ;; [styles.shared.style-modules] ;; "styles/shared/style-modules.html"]
                                   ;; [polymer.iron :as iron :refer [ajax flex-layout icons list pages selector]]
                                   ;; ;; (h/style {:include "shared-styles"}) - goes inside template elt
                                   ;; [css.foo :css ["foo.css"]]
                                   ;; [js.foo :js ["foo.js"]]
                                   )
                         (ctor
                          ;; parameters/properties
                          [^{:type String, :default "Jones", :notify false, :read-only true, :attrsync true}
                           author
                           list-items ;; make arg to dom-repeat explicit
                           ^{:type Content, :default nil}
                           content]
                          (h/div
                           (h/link {:rel "import" :href "/polymer/polymer/polymer.html"})
                           (h/ul
                            (h/li "foo: " (h/span {:class "paper-font-body1"} :author))
                            (h/li (h/span {:class "paper-font-body1"} :author%))))
          ))]
   forms)))


