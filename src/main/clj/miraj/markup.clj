;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "derived from clojure.data.xml"
      :author "Gregg Reynolds, Chris Houser"}
  miraj.markup
  (:refer-clojure :exclude [import require])
  (:require [clojure.string :as str]
            [clojure.data.json :as json]
            [clj-time.core :as t]
            [clojure.pprint :as pp]
            [clojure.java.io :as io])
            ;; [clojure.tools.reader :as reader]
            ;; [clojure.tools.reader.reader-types :as readers]
            ;; [cljs.analyzer :as ana]
            ;; [cljs.compiler :as c]
            ;; [cljs.closure :as cc]
            ;; [cljs.env :as env])
            ;; [clojure.tools.logging :as log :only [trace debug error info]])
  (:import [java.io ByteArrayInputStream StringReader StringWriter]
           [javax.xml.stream XMLInputFactory
                             XMLStreamReader
                             XMLStreamConstants]
           [javax.xml.parsers DocumentBuilder DocumentBuilderFactory]
           [javax.xml.transform.dom DOMSource]
           [javax.xml.transform OutputKeys TransformerFactory]
           [javax.xml.transform.stream StreamSource StreamResult]
           [java.nio.charset Charset]
           [java.io Reader]))
;;           [java.util Date]))

;; (println "loading miraj/markup.clj")
;;FIXME:  support comment nodes

(defonce mode (atom nil))
(defonce verify? (atom false))
(defonce miraj-boolean-tag "__MIRAJ_BOOLEAN_955196")

(defn pprint-str [m]
  (let [w (StringWriter.)] (pp/pprint m w)(.toString w)))

(defn ns->uri [n] (str/replace n #"\." "/"))

(defn var->path [v]
  ;; (println "var->path: " v)
  (let [nm (:name (meta v))
        namesp (:ns (meta v))
        ;; _ (println "namesp: " namesp)
        ns-name (ns-name namesp)
        ;; _ (println "ns-name: " ns-name)
        path (str/replace ns-name #"-|\." {"-" "_" "." "/"})
        ;; _ (println "path: " path)
        fn (str/replace nm #"-|\." {"-" "_" "." "/"})
        ;; _ (println "fn: " fn)
        ]
    (str path "/" fn)))

(defn sym->cljs-ns
  [sym]
  ;; (println "sym->cljs-ns: " sym)
  (let [v (resolve sym)
        nm (:name (meta v))
        ;; _ (println "nm: " nm)
        namesp (:ns (meta v))
        ;; _ (println "namesp: " namesp)
        ns-name (ns-name namesp)
        ]
    (symbol (str ns-name "." nm))))

(defn var->cljs-ns
  [v]
  ;; (println "var->cljs-ns: " v)
  (let [nm (:name (meta v))
        ;; _ (println "nm: " nm)
        namesp (:ns (meta v))
        ;; _ (println "namesp: " namesp)
        ns-name (ns-name namesp)
        ]
    (symbol (str ns-name "." nm))))

(defn var->sym
  [v]
  ;; (println "var->sym: " v)
  (let [nm (:name (meta v))
        ;; _ (println "nm: " nm)
        namesp (:ns (meta v))
        ;; _ (println "namesp: " namesp)
        ns-name (ns-name namesp)
        ]
    (symbol (str ns-name "." nm))))

(defmacro interface-sym->protocol-sym
  "Protocol names cannot contain '.'"
  [sym]
  ;; (println "interface-sym->protocol-sym: " sym)
  `(if (var? (resolve ~sym))
     (symbol (str (->> (resolve ~sym) meta :ns))
            (str (->> (resolve ~sym) meta :name)))
     (let [nodes# (str/split (str ~sym) #"\.")
           ns# (str/join "." (drop-last nodes#))
           ;; _# (println "ns: " ns#)
           nm# (last nodes#)
           newsym# (symbol (str ns#) (str nm#))]
       ;; (println "interface-sym->protocol-sym: " ~sym " => " newsym#)
       newsym#)))

(def cljkey->jskey
  {:notify :notify
   :reflect :reflectToAttribute
   :read-only :readOnly})

(def cljtype->jstype
  {'Boolean 'js/Boolean
   'Date 'js/Date
   'Number 'js/Number
   'String 'js/String
   'Vector 'js/Array
   'Map 'js/Object})

(def html5-void-elts
  #{"area" "base" "br" "col"
   "embed" "hr" "img" "input"
   "keygen" "link" "meta" "param"
   "source" "track" "wbr"})

(def html5-global-attrs
  "https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes"
  ;;FIXME - handle data-*
  {:access-key :_
   :class :_
   :content-editable :_
   :context-menu :_
   :dir :_
   :draggable :_
   :drop-zone :_
   :hidden :_
   :id :_
   :item-id :_
   :item-prop :_
   :item-ref :_
   :item-scope :_
   :item-type :_
   :lang :_
   :spellcheck :_
   :style :_
   :tab-index :_
   :title :_
   :translate :_})

(def html5-link-types
  #{:alternate :archives :author :bookmark :dns-prefetch :external :first
    :help :icon :index :last :license :next :no-follow :no-referrer :ping-back
    :import ;; non-standard but needed for polymer TODO: log a warning
    :preconnect :prefetch :preload :prerender :prev
    :search :stylesheet :sidebar :tag :up})

(def html5-link-attrs
  {:crossorigin #{:anonymous :use-credentials}
   :disabled :deprecated
   :href :uri
   :hreflang :bcp47  ;; http://www.ietf.org/rfc/bcp/bcp47.txt
   :integrity :_
   :media :_
   :methods :_
   :rel :link-type ;; https://developer.mozilla.org/en-US/docs/Web/HTML/Link_types
   :rev :obsolete
   :sizes :sizes
   :target :_
   :type :mime})

(defn kw->nm [kw] (subs (str kw) 1))

; Represents a parse event.
; type is one of :start-element, :end-element, or :characters
(defrecord Event [type name attrs str])

(defn event [type name & [attrs str]]
  (Event. type name attrs str))

(defn qualified-name [event-name]
  (if (instance? clojure.lang.Named event-name)
   [(namespace event-name) (name event-name)]
   (let [name-parts (str/split event-name #"/" 2)]
     (if (= 2 (count name-parts))
       name-parts
       [nil (first name-parts)]))))

;; (defn validate-html-rel-attval
;;   [val]
;;   (println "validate-html-rel-attval " val)
;;   (contains? html5-link-types val))

(def camel-case-regex #"[a-z]*[A-Z][a-z]*") ;; FIXME: do we need a proper cc regex?

(defn validate-html5-attr-name
  [nm val]
  ;;FIXME: allow for HTML attrnames that do use "-", e.g. data-*, dns-prefetch, etc.
  ;; What about user-defined attrs?  Tough luck?
  ;; (if (re-matches #".*[A-Z].*" (str nm))
  ;;   (throw (Exception. (str "HTML attribute names are case-insensitive; currently, only lower-case is allowed.  (This restriction will be relaxed in a later version.)  Please use clojure-case (lower-case with dashes) for {" (keyword nm) " " val "}."))))
  #_(str/replace nm #"-" "")
;;  (if (.endsWith nm "$")
  (str nm))

(defn get-two-way-token
  [v]
  (let [s (subs (str v) 1)
        parts (str/split s #"->")]
    (cond
      (> (count parts) 2) (throw (Exception. (str "too many -> parts in expr " v)))
      (= (count parts) 2) (str (last parts) "::" (first parts))
      :else s)))

(defn write-attributes [attrs ^javax.xml.stream.XMLStreamWriter writer]
  (doseq [[k v] attrs]
    ;;(println "ATTR: " k " = " v " " (type v) (keyword? v))
    (let [[attr-ns nm] (qualified-name k)
          attr-name (if (= :html @mode)
                      (validate-html5-attr-name nm v)
                      (str nm))

          ;; FIXME: only do polymer annotations in HTML mode
          attr-val ;;(if (= :html @mode)
                     (cond
                       (= :rel k) (if (contains? html5-link-types
                                                 (if (string? v) (keyword v) v))
                                    (if (keyword? v) (subs (str v) 1) v)
                                    (throw (Exception.
                                            (str "Invalid link type value for rel attribute: {"
                                                 k " " v "}; valid values are: "
                                                 html5-link-types))))
                       (keyword? v)
                       (do ;;(println "KEYWORD")
                         (if (nil? (namespace v))
                           (str "{{" (get-two-way-token v) "}}")
                           ;;FIXME
                           (str "{{" (subs (str v) 1) "}}")))

                       (symbol? v) (str "[[" (str v) "]]")
                       ;; (= (subs (str k) 1) (str v)) miraj-boolean-tag
                       ;; (empty? v) miraj-boolean-tag
                       (nil? v) miraj-boolean-tag
                       :else (str v))
                     ;;(str v))
                     ]
      (if attr-ns
        (.writeAttribute writer attr-ns attr-name attr-val)
        (.writeAttribute writer attr-name attr-val)))))

(declare serialize serialize-impl)

; Represents a node of an XML tree
(defrecord Element [tag attrs content]
  java.lang.Object
  (toString [x]
    (do ;;(print "Element toString: " x "\n")
        (let [sw (java.io.StringWriter.)]
          (serialize x)))))

(defrecord CData [content])
(defrecord Comment [content])

;;FIXME: support fragments

;; HTML ELEMENTS

;; void != empty
;; html void elements  = xml elements with "nothing" content model
;; html voids:  http://www.w3.org/html/wg/drafts/html/master/syntax.html#void-elements
;; xml nothings: http://www.w3.org/html/wg/drafts/html/master/dom.html#concept-content-nothing

;; "Void elements only have a start tag; end tags must not be specified for void elements."
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

;; In other words, HTML5 syntax and semantics are not uniform across
;; all elements.

(def xsl-identity-transform-html
   ;; see http://news.oreilly.com/2008/07/simple-pretty-printing-with-xs.html
  (str
   "<xsl:stylesheet version='1.0' xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>"
   "<xsl:strip-space elements='*' />"
   "<xsl:output method='xml' encoding='UTF-8' indent='yes' cdata-section-elements='script style'/>"

   "<xsl:template match='node()'>"
     "<xsl:copy>"
       "<xsl:apply-templates select='@*|node()'/>"
     "</xsl:copy>"
   "</xsl:template>"

   "<xsl:template match='html'>"
     "<xsl:text disable-output-escaping='yes'>&lt;!doctype html&gt;</xsl:text>"
     "<xsl:text>&#x0A;</xsl:text>"
     "<xsl:copy>"
       "<xsl:apply-templates select='@*|node()'/>"
     "</xsl:copy>"
   "</xsl:template>"

   "<xsl:template priority=\"99\" match=\"" (str/join "|" html5-void-elts) "\">"
     "<xsl:copy>"
       "<xsl:apply-templates select='@*|node()'/>"
       "VOID_333109"
     "</xsl:copy>"
   "</xsl:template>"

   ;; remove self-closing tags
   "<xsl:template match='*[not(node()) and not(string(.))]'>"
   ;; "<xsl:message>EMPTY TAG</xsl:message>"
     "<xsl:copy>"
       "<xsl:apply-templates select='@*|node()'/>"
       "_EMPTY_333109"
     "</xsl:copy>"
   "</xsl:template>"

   "<xsl:template match='script' priority='999'>"
     "<xsl:copy>"
       "<xsl:apply-templates select='@*|node()'/>"
       "<xsl:if test='not(node()) and not(string(.))'>"
         "_EMPTY_333109"
       "</xsl:if>"
     "</xsl:copy>"
   "</xsl:template>"

   "<xsl:template match='script/text()' priority='999'>"
     "<xsl:text disable-output-escaping='yes'>"
       "<xsl:value-of select='.'/>"
     ;; "<xsl:copy>"
       ;; "<xsl:apply-templates select='@*|node()'/>"
     ;; "</xsl:copy>"
     "</xsl:text>"
   "</xsl:template>"

   "<xsl:template match=\"@*\">"
   ;; Handle HTML boolean attributes
     "<xsl:choose>"
       "<xsl:when test='name() = .'>"
         "<xsl:attribute name='{name()}'>"
           miraj-boolean-tag
         "</xsl:attribute>"
       "</xsl:when>"
       "<xsl:when test='. = concat(\":\", name())'>"
         "<xsl:attribute name='{name()}'>"
           miraj-boolean-tag
         "</xsl:attribute>"
       "</xsl:when>"
       "<xsl:when test='. = \"\"'>"
         "<xsl:attribute name='{name()}'>"
           miraj-boolean-tag
         "</xsl:attribute>"
       "</xsl:when>"
       "<xsl:otherwise>"
         "<xsl:copy/>"
       "</xsl:otherwise>"
     "</xsl:choose>"
   "</xsl:template>"
   "</xsl:stylesheet>"))

(def xsl-identity-transform-xml
   ;; see http://news.oreilly.com/2008/07/simple-pretty-printing-with-xs.html
  (str
   "<xsl:stylesheet version='1.0' xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>"
   "<xsl:strip-space elements='*' />"
   "<xsl:output method='xml' encoding='UTF-8' indent='yes'/>"

   "<xsl:template match='@*|node()'>"
     "<xsl:copy>"
       "<xsl:apply-templates select='@*|node()'/>"
     "</xsl:copy>"
   "</xsl:template>"
   "</xsl:stylesheet>"))

(def xsl-normalize
  (str
   "<xsl:stylesheet version='1.0' xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>"
   "<xsl:strip-space elements='*' />"
   "<xsl:output method='xml' encoding='UTF-8' indent='yes'/>"

   "<xsl:template match='html' priority='99'>"
     "<xsl:copy>"
       "<head>"
         "<xsl:choose>"
           "<xsl:when test='meta[@name=\"charset\"]'>"
             "<xsl:apply-templates select='meta[@name=\"charset\"]' mode='charset'/>"
           "</xsl:when>"
           "<xsl:otherwise>"
             "<meta name='charset' content='utf-8'/>"
           "</xsl:otherwise>"
         "</xsl:choose>"
         "<xsl:apply-templates select='link|meta|style' mode='head'/>"
         "<xsl:apply-templates select='head/link|head/meta|head/style' mode='head'/>"
         "<xsl:apply-templates select='script|head/script' mode='head'/>"
       "</head>"
       "<xsl:apply-templates select='@*|node()'/>"
     "</xsl:copy>"
   "</xsl:template>"

   "<xsl:template match='head'/>"
   "<xsl:template match='head' mode='head'>"
     "<xsl:apply-templates select='@*|node()' mode='head'/>"
   "</xsl:template>"

   "<xsl:template match='meta[@name=\"charset\"]' priority='99'/>"
   "<xsl:template match='meta[@name=\"charset\"]' mode='head'/>"
   "<xsl:template match='meta[@name=\"charset\"]' mode='charset'>"
     "<xsl:copy>"
       "<xsl:apply-templates select='@*|node()'/>"
     "</xsl:copy>"
   "</xsl:template>"

   "<xsl:template match='@*|node()'>"
     "<xsl:copy>"
       "<xsl:apply-templates select='@*|node()'/>"
     "</xsl:copy>"
   "</xsl:template>"

   "<xsl:template match='link|meta|script|style'/>"
   "<xsl:template match='link|meta|script|style' mode='head'>"
     "<xsl:copy>"
       "<xsl:apply-templates select='@*|node()'/>"
     "</xsl:copy>"
   "</xsl:template>"

   "<xsl:template match='body//style' priority='99'>"
     "<xsl:copy>"
       "<xsl:apply-templates select='@*|node()'/>"
     "</xsl:copy>"
   "</xsl:template>"

   "<xsl:template match='body//script' priority='99'>"
     "<xsl:copy>"
       "<xsl:apply-templates select='@*|node()'/>"
     "</xsl:copy>"
   "</xsl:template>"

   ;; ;; "<xsl:template match='script'/>"
   ;; "<xsl:template match='script/text()'>"
   ;;   "<xsl:text disable-output-escaping='yes'>"
   ;;   "FOO &amp; BAR"
   ;;   "<xsl:copy>"
   ;;     "<xsl:apply-templates select='@*|node()'/>"
   ;;   "</xsl:copy>"
   ;;   "</xsl:text>"
   ;; "</xsl:template>"

   ;; "<xsl:template match='style/text()'>"
   ;;   "<xsl:text disable-output-escaping='yes'>"
   ;;   "FOO &lt; BAR"
   ;;     ;; "<xsl:copy>"
   ;;     ;;   "<xsl:apply-templates select='@*|node()'/>"
   ;;     ;; "</xsl:copy>"
   ;;   "</xsl:text>"
   ;; "</xsl:template>"

   "<xsl:template match='body//link' priority='99' mode='head'/>"
   "</xsl:stylesheet>"))

(def xsl-normalize-codom
  (str
   "<xsl:stylesheet version='1.0' xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>"
   "<xsl:strip-space elements='*' />"
   "<xsl:output method='xml' encoding='UTF-8' indent='yes'/>"

   "<xsl:template match='/' priority='99'>"
     "<xsl:apply-templates select='@*|node()'/>"
   "</xsl:template>"

   "<xsl:template match='CODOM_56477342333109' priority='99'>"
     "<xsl:copy>"
       "<xsl:element name='link'>"
         "<xsl:attribute name='rel'>import</xsl:attribute>"
         "<xsl:attribute name='href'>"
           "<xsl:text>/bower_components/polymer/polymer.html</xsl:text>"
         "</xsl:attribute>"
       "</xsl:element>"
       "<xsl:apply-templates select='//link' mode='head'/>"
       "<xsl:element name='dom-module'>"
         "<xsl:attribute name='id'>"
           "<xsl:value-of select='@id'/>"
         "</xsl:attribute>"
         "<template>"
           "<xsl:apply-templates/>"
         "</template>"
       "</xsl:element>"
     "</xsl:copy>"
   "</xsl:template>"

   "<xsl:template match='@*|node()'>"
     "<xsl:copy>"
       "<xsl:apply-templates select='@*|node()'/>"
     "</xsl:copy>"
   "</xsl:template>"

   "<xsl:template match='link'/>"
   "<xsl:template match='link' mode='head'>"
     "<xsl:copy>"
       "<xsl:apply-templates select='@*|node()'/>"
     "</xsl:copy>"
   "</xsl:template>"

   "</xsl:stylesheet>"))

;; from http://webcomponents.org/polyfills/ :
;; Note: Due to the nature of some of the polyfills, to maximize
;; compatibility with other libraries, make sure that webcomponents.js is
;; the first script tag in your document's <head>.
(def xsl-optimize-js
  (str
   "<xsl:stylesheet version='1.0' xmlns:xsl='http://www.w3.org/1999/XSL/Transform' >"
   "<xsl:strip-space elements='*' />"
   "<xsl:output method='xml' encoding='UTF-8' indent='yes'/>"

   "<xsl:template match='html'>"
     "<xsl:if test='not(head)'>"
       "<xsl:message terminate='yes'>OPTIMIZE-JS ERROR: &lt;head> not found; did you forget to run normalize first?</xsl:message>"
     "</xsl:if>"
     "<xsl:copy>"
       "<xsl:apply-templates select='@*|node()'/>"
     "</xsl:copy>"
   "</xsl:template>"

   "<xsl:template match='@*|node()'>"
     "<xsl:copy>"
       "<xsl:apply-templates select='@*|node()'/>"
     "</xsl:copy>"
   "</xsl:template>"

   "<xsl:template match='head'>"
     "<xsl:copy>"
       "<xsl:apply-templates select='meta[@name=\"charset\"]' mode='optimize'/>"
       "<xsl:apply-templates select='//script' mode='polyfill'/>"
       "<xsl:apply-templates select='@*|node()'/>"
     "</xsl:copy>"
   "</xsl:template>"

   "<xsl:template match='meta[@name=\"charset\"]'/>"
   "<xsl:template match='meta[@name=\"charset\"]' mode='optimize'>"
     "<xsl:copy>"
       "<xsl:apply-templates select='@*|node()'/>"
     "</xsl:copy>"
   "</xsl:template>"

   "<xsl:template match='script'/>"
   "<xsl:template match='script' mode='optimize' priority='99'>"
     "<xsl:copy>"
       "<xsl:apply-templates select='@*|node()'/>"
     "</xsl:copy>"
   "</xsl:template>"

   ;;FIXME - put webcomponentsjs after all <meta> elts?
   ;; (h/script {:src "bower_components/webcomponentsjs/webcomponents-lite.js"})
   "<xsl:template match='script' mode='polyfill'/>"
   ;; "<xsl:template match='script[contains(@src, \"webcomponentsjs\")]'/>"
   "<xsl:template match='script[contains(@src, \"webcomponentsjs\")]' mode='optimize' priority='99'/>"
   "<xsl:template match='script[contains(@src, \"webcomponentsjs\")]' mode='polyfill' priority='99'>"
     "<xsl:copy>"
       "<xsl:apply-templates select='@*|node()'/>"
     "</xsl:copy>"
   "</xsl:template>"

   "<xsl:template match='body' priority='99'>"
     "<xsl:copy>"
       "<xsl:apply-templates select='@*|node()'/>"
       "<xsl:apply-templates select='//script' mode='optimize'/>"
     "</xsl:copy>"
   "</xsl:template>"
   "</xsl:stylesheet>"))

(declare element parse-str)

(defn xsl-xform
  [ss elts]
  ;; (println "xsl-xform ss: " ss)
  ;; (println "xsl-xform doc: " elts)
  (let [ml (do
             (if (not (instance? miraj.markup.Element elts))
               (do ;;(println (type elts))
                   (throw (Exception. "xsl-xform only works on clojure.data.xml.Element"))))
             (serialize :xml elts))
        ;; _ (println "XF SOURCE: " ml)
        xmlSource (StreamSource.  (StringReader. ml))
        xmlOutput (StreamResult. (StringWriter.))
        factory (TransformerFactory/newInstance)
        transformer (.newTransformer factory (StreamSource. (StringReader. ss)))]
    ;; (.setOutputProperty transformer OutputKeys/INDENT "yes")
    ;; (.setOutputProperty transformer "{http://xml.apache.org/xslt}indent-amount", "4")
    (if (.startsWith ml "<?xml")
      (.setOutputProperty transformer OutputKeys/OMIT_XML_DECLARATION "no")
      (.setOutputProperty transformer OutputKeys/OMIT_XML_DECLARATION "yes"))
    (.transform transformer xmlSource xmlOutput)
    (parse-str (.toString (.getWriter xmlOutput)))))

;;FIXME: support non-tree input
;;FIXME: support :xhtml option
(defn pprint-impl
  [& elts]
  ;; (println "PPRINT-IMPL: " elts)
  (let [s (if (or (= :html (first elts))
                  (= :xml (first elts)))
            (do ;(log/trace "FIRST ELT: " (first elts) " " (keyword? (first elts)))
                (rest elts))
            (if (keyword? (first elts))
              (throw (Exception. "only :html and :xml supported"))
              elts))
        fmt (if (keyword? (first elts)) (first elts) :html)
        void (reset! mode fmt)
        ;; log (log/trace "mode: " @mode)
        ;; always serialize to xml, deal with html issues in the transform
        ml (if (string? s)
             (throw (Exception. "xml pprint only works on clojure.data.xml.Element"))
             (if (> (count s) 3)
               (do ;;(println "pprint-impl FOREST")
                   (let [s (serialize :xml (element :CODOM_56477342333109 s))]
                     (reset! mode fmt)
                     s))
               (let [s (serialize :xml s)]
                 (reset! mode fmt)
                 s)))
        ;; _ (println "xml serialized: " ml)
        xmlSource (StreamSource.  (StringReader. ml))
        xmlOutput (StreamResult.
                   (let [sw (StringWriter.)]
                     (if (.startsWith ml "<!doctype")
                       (.write sw "<!doctype html>\n"))
                     sw))
        factory (TransformerFactory/newInstance)
        transformer (if (= :html @mode)
                      (do
                        ;;(log/trace "transforming with xsl-identity-transform-html: " xsl-identity-transform-html)
                      (.newTransformer factory (StreamSource. (StringReader. xsl-identity-transform-html))))
                      (do
                        ;;(log/trace "transforming with xsl-identity-transform-xml")
                      (.newTransformer factory (StreamSource. (StringReader. xsl-identity-transform-xml)))))]
    ;;                      (.newTransformer factory))]
    (.setOutputProperty transformer OutputKeys/INDENT "yes")
    (.setOutputProperty transformer "{http://xml.apache.org/xslt}indent-amount", "4")
    (if (.startsWith ml "<?xml")
      (.setOutputProperty transformer OutputKeys/OMIT_XML_DECLARATION "no")
      (.setOutputProperty transformer OutputKeys/OMIT_XML_DECLARATION "yes"))

    (.transform transformer xmlSource xmlOutput)
    (println (if (= :html fmt)
               ;(str/replace (.toString (.getWriter xmlOutput)) #"VOID_333109<[^>]+>" "")
               (let [string-writer (.getWriter xmlOutput)
                     s (.toString string-writer)
                     void (.flush string-writer)
                     s (str/replace s #"<CODOM_56477342333109>\n" "")
                     s (str/replace s #"</CODOM_56477342333109>\n" "")
                     s (str/replace s #"VOID_333109<[^>]+>" "")
                     s (str/replace s #"_EMPTY_333109" "")
                     s (str/replace s #"^_([^=]*)=" "$1\\$=")
                     s (str/replace s #"<!\[CDATA\[" "")
                     s (str/replace s #"]]>" "")
                     regx (re-pattern (str "=\"" miraj-boolean-tag "\""))]
                 ;; boolean attribs: value must be "" or must match attrib name
                 ;;FIXME: make this more robust
                 (str/replace s regx ""))
               (.toString (.getWriter xmlOutput))))))

(defn pprint
  [& elts]
  ;; (println "PPRINT elts: " elts)
  (if (keyword? (first elts))
    (do ;;(println "fnext elts: " (fnext elts))
        (if (nil? (fnext elts))
          nil
          (apply pprint-impl elts)))
    (do ;;(println "first elts: " (first elts))
        (if (nil? (first elts))
          nil
          (pprint-impl (first elts))))))

(defn serialize-impl
  [& elts]
  ;; (println "serialize-impl: " elts)
  (let [s (if (or (= :html (first elts))
                  (= :xml (first elts)))
            (do ;(log/trace "FIRST ELT: " (first elts) " " (keyword? (first elts)))
                (rest elts))
            (if (keyword? (first elts))
              (throw (Exception. "only :html and :xml supported"))
              elts))
        fmt (if (keyword? (first elts)) (first elts) :html)
        void (reset! mode fmt)
        ;; _ (println "mode: " @mode)
        ;; always serialize to xml, deal with html issues in the transform
        ml (if (string? s)
             (throw (Exception. "xml pprint only works on clojure.data.xml.Element"))
             (if (> (count s) 1)
               (throw (Exception. "forest input not yet supported for serialize"))
               (let [s (serialize :xml s)]
                 (reset! mode fmt)
                 s)))
        xmlSource (StreamSource.  (StringReader. ml))
        xmlOutput (StreamResult.
                   (let [sw (StringWriter.)]
                     (if (.startsWith ml "<!doctype")
                       (.write sw "<!doctype html>"))
                     sw))
        factory (TransformerFactory/newInstance)
        transformer (if (= :html @mode)
                      (do
                        ;;(println "transforming with xsl-identity-transform-html")
                      (.newTransformer factory (StreamSource. (StringReader. xsl-identity-transform-html))))
                      (do
                        ;;(log/trace "transforming with xsl-identity-transform-xml")
                      (.newTransformer factory (StreamSource. (StringReader. xsl-identity-transform-xml)))))]
    ;;                      (.newTransformer factory))]
    (.setOutputProperty transformer OutputKeys/INDENT "no")
    (.setOutputProperty transformer "{http://xml.apache.org/xslt}indent-amount", "0")
    (if (.startsWith ml "<?xml")
      (.setOutputProperty transformer OutputKeys/OMIT_XML_DECLARATION "no")
      (.setOutputProperty transformer OutputKeys/OMIT_XML_DECLARATION "yes"))

    (.transform transformer xmlSource xmlOutput)
    (let[result (if (= :html fmt)
                  (let [string-writer (.getWriter xmlOutput)
                        s (.toString string-writer)
                        ;; _ (prn "XML OUTPUT: " s)
                        void (.flush string-writer)
                        s (str/replace s #"<CODOM_56477342333109>" "")
                        s (str/replace s #"</CODOM_56477342333109>" "")
                        s (str/replace s #"VOID_333109<[^>]+>" "")
                        s (str/replace s #"_EMPTY_333109" "")
                        s (str/replace s #"<!\[CDATA\[" "")
                        s (str/replace s #"]]>" "")
                        regx (re-pattern (str "=\"" miraj-boolean-tag "\""))]
                    (str/replace s regx ""))
                  (do (println "XML FOOBAR")
                      (.toString (.getWriter xmlOutput))))]
      ;; (prn "OUTPUT: " result)
      result)))

(declare emit)

(defn serialize
  "Serializes the Element to String and returns it.
   Options:
    mode:  :html (default) or :xml
    :encoding <str>          Character encoding to use
    :with-xml-declaration <bool>, default false"
  ;; [& args]
  [& elts]
  ;; (println "serialize: " elts)
  (let [args (if (or (= :html (first elts)) (= :xml (first elts)))
               (rest elts)
               (if (keyword? (first elts))
                 (throw (Exception. "only :html and :xml supported"))
                 elts))
        fmt (if (keyword? (first elts)) (first elts) :html)
        ^java.io.StringWriter
        string-writer (java.io.StringWriter.)]
    (reset! mode fmt)
    ;; (println "serializing to" @mode ": " args)
    (let [doc-str (cond
                    (= @mode :html)
                    (do ;(log/trace "emitting HTML: " args)
                      ;; (if (= :html (:tag (first args)))
                      ;;   (.write string-writer "<!DOCTYPE html>"))
                      (apply serialize-impl elts))
                    ;; (emit args string-writer :html true :with-xml-declaration false))

                    (= @mode :xml)
                    (do ;;(println "emiting XML")
                      ;; (apply serialize-impl elts))
                      (.toString
                       (if (= :with-xml-declaration (first args))
                         (do ;(log/trace "emitting with xml decl: " args)
                           (emit (rest args) string-writer :with-xml-declaration true))
                         (do ;(log/trace "emitting w/o xml decl: " args)
                           (emit args string-writer :with-xml-declaration false)))))
                    :else
                    (throw (Exception. "invalid mode: " @mode)))]
      doc-str)))
    ;; (str (if (= @mode :html)
    ;;        (let [s (str/replace (.toString string-writer) #"VOID_333109<[^>]+>" "")
    ;;              regx (re-pattern (str "=\"" miraj-boolean-tag "\""))]
    ;;              (str/replace s regx ""))
    ;;        (.toString string-writer)))))

(defn emit-start-tag [event ^javax.xml.stream.XMLStreamWriter writer]
  ;;(println "emit-start-tag: " (:name event))
  (let [[nspace qname] (qualified-name (:name event))]
    (.writeStartElement writer "" qname (or nspace ""))
    (write-attributes (:attrs event) writer)))

(defn emit-void-tag [event ^javax.xml.stream.XMLStreamWriter writer]
  (let [[nspace qname] (qualified-name (:name event))]
    (.writeStartElement writer "" qname (or nspace ""))
    (write-attributes (:attrs event) writer)
    (.writeCharacters writer "") ;; forces close of start tag
    ))

(defn emit-end-tag [event
                    ^javax.xml.stream.XMLStreamWriter stream-writer
                    ^java.io.Writer writer]
  (let [t (name (:name event))]
    ;; (println "EMIT-END-TAG: " t (type t))
    ;;(.writeEndElement writer)
    (.write writer (str
                    (if (= @mode :html)
                      (if (contains? html5-void-elts t)
                        "VOID_333109"))
                    "</" t ">"))))

(defn str-empty? [s]
  (or (nil? s)
      (= s "")))

(defn emit-cdata [^String cdata-str ^javax.xml.stream.XMLStreamWriter writer]
  ;; (println "EMIT-CDATA " cdata-str)
  (when-not (str-empty? cdata-str)
    (let [idx (.indexOf cdata-str "]]>")]
      (if (= idx -1)
        (.writeCData writer cdata-str )
        (do
          (.writeCData writer (subs cdata-str 0 (+ idx 2)))
          (recur (subs cdata-str (+ idx 2)) writer))))))

(defn emit-event [event
                  ^javax.xml.stream.XMLStreamWriter stream-writer
                  ^java.io.Writer writer]
  ;; (println "EMIT-EVENT: " event)
  (case (:type event)
    :start-element (emit-start-tag event stream-writer)
    :end-element (do
                   #_(println "END ELT")
                   (emit-end-tag event stream-writer writer))
    ;; :void-element (do
    ;;                 #_(println "VOID ELT")
    ;;                 (emit-start-tag event stream-writer))
    :chars #_(if (:disable-escaping opts)
             (do ;; to prevent escaping of elts embedded in (str ...) constructs:
               (.writeCharacters stream-writer "") ; switches mode?
               (.write writer (:str event)))       ; writes without escaping < & etc.
             )
    (.writeCharacters stream-writer (:str event))
;;    (.write writer (str ">" (:str event)))       ; writes without escaping < & etc.

    :kw (.writeCharacters stream-writer (:str event))
    :sym (.writeCharacters stream-writer (:str event))
    :cdata (emit-cdata (:str event) stream-writer)
    :comment (.writeComment stream-writer (:str event))
    (throw (Exception. (str "no matching clause: ") (:type event)))))

(defprotocol EventGeneration
  "Protocol for generating new events based on element type"
  (gen-event [item]
    "Function to generate an event for e.")
  (next-events [item next-items]
    "Returns the next set of events that should occur after e.  next-events are the
     events that should be generated after this one is complete."))

(extend-protocol EventGeneration
  Element
  (gen-event [element]
    ;; (if (= (:tag element) :link)
    ;;   (Event. :void-element (:tag element) (:attrs element) nil)
      (Event. :start-element (:tag element) (:attrs element) nil)) ;)
  (next-events [element next-items]
    (do #_(println "NEXT evt: " (:tag element))
        ;(if (= (:tag element) :link)
         ; next-items
          (cons (:content element)
                (cons (Event. :end-element (:tag element) nil nil) next-items))))

  Event
  (gen-event [event] event)
  (next-events [_ next-items]
    next-items)

  clojure.lang.PersistentArrayMap
  (gen-event [coll]
    (println "GEN-EVENT PAM: " coll))
  (next-events [coll next-items])

  clojure.lang.Sequential
  (gen-event [coll]
    (gen-event (first coll)))
  (next-events [coll next-items]
    (if-let [r (seq (rest coll))]
      (cons (next-events (first coll) r) next-items)
      (next-events (first coll) next-items)))

  ;; clojure.lang.PersistentArrayMap
  ;; (gen-event [coll]
  ;;   (println (str "GEN-EVENT: " coll)))
  ;; (next-events [coll next-items]
  ;;   (println (str "NEXT-EVENTS: " coll next-items)))

  clojure.lang.Keyword
  (gen-event [kw]
    (let [nm (name kw)
          ns (namespace kw)]
      (Event. :kw nil nil
                (str "{{" (namespace kw) (if (namespace kw) ".") (name kw) "}}"))))
              ;; FIXME this should not be necessary if the tag fns are correct:
              ;; (if (nil? (namespace kw))
              ;;   (str "class=\"" (str/replace (name kw) "." " ") "\"")))))

  (next-events [_ next-items]
    next-items)

  clojure.lang.Symbol
  (gen-event [sym]
    (let [nm (name sym)
          ns (namespace sym)]
      ;; (log/trace "gen-event Symbol: " sym)
      (Event. :sym nil nil
              (str "[[" ns (if ns ".") nm "]]"))))
      ;;         (str "[[" (namespace kw) (if (namespace kw) ".") (name kw) "]]")))))
  (next-events [_ next-items]
    next-items)

  String
  (gen-event [s]
    (Event. :chars nil nil s))
  (next-events [_ next-items]
    next-items)

  Boolean
  (gen-event [b]
    (Event. :chars nil nil (str b)))
  (next-events [_ next-items]
    next-items)

  Number
  (gen-event [b]
    (Event. :chars nil nil (str b)))
  (next-events [_ next-items]
    next-items)

  Long
  (gen-event [b]
    (Event. :chars nil nil (str b)))
  (next-events [_ next-items]
    next-items)

  CData
  (gen-event [cdata]
    (Event. :cdata nil nil (:content cdata)))
  (next-events [_ next-items]
    next-items)

  Comment
  (gen-event [comment]
    (Event. :comment nil nil (:content comment)))
  (next-events [_ next-items]
    next-items)

  nil
  (gen-event [_]
    (Event. :chars nil nil ""))
  (next-events [_ next-items]
    next-items))

(defn flatten-elements [elements]
  ;; (prn "flatten-elements:")
  ;; (prn elements)
  (when (seq elements)
    (lazy-seq
     (let [e (first elements)]
       (let [f (gen-event e)]
       (cons f
             (flatten-elements (next-events e (rest elements)))))))))

(defn parse-elt-args
  [attrs content]
  ;; (println "parse-elt-args ATTRS: " attrs " CONTENT: " content)
  ;; (let [fst attrs] ;; (first args)]
    (cond
      ;;TODO support boolean, etc. for CDATA elts
      (number? attrs)
      (do ;;(println "number? attrs: " attrs)
          ;; (span 3) => <span>3</span>
        [{} (remove empty? (list (str attrs) content))])

      (symbol? attrs)
      (do ;;(println "keyword? attrs: " attrs)
          ;; (span 'foo) => <span>{{foo}}</span>
        [{} (remove empty? (list attrs content))])

      (keyword? attrs)
      (do ;;(println "keyword? attrs: " attrs)
          ;; (span :foo) => <span>[[foo]]</span>
          (if (nil? (namespace attrs))
            [{} (list attrs content)]
            ;; (span ::foo) => <span id="foo"></span>
            ;; (span ::foo.bar) => <span id="foo" class="bar"></span>
            ;; (span ::.foo.bar) => <span class="foo bar"></span>
            (let [tokstr (name attrs)
                  no-id (.startsWith tokstr ".")
                  toks (filter identity (str/split tokstr #"\."))
                  attrs (first toks)
                  ;; (doall toks)
                  ;; (println "TOKS: " toks attrs)
                  ;; (println "REST TOKS: " (rest toks))
                  result (if no-id
                           [{:class (str/trim (str/join " " toks))} content]
                           (if (seq (rest toks))
                             [{:id attrs :class (str/trim (str/join " " (rest toks)))} content]
                             [{:id attrs} content]))]
              ;; (println "CONTENT: " (last result))
              (if (map? (first (last result)))
                (if (instance? miraj.markup.Element (first (last result)))
                  result
                  [(merge (first result) (first (last result))) (rest (last result))])
                result))))

      (map? attrs)
      (do ;;(println "map? attrs" attrs)
        (if (instance? miraj.markup.Element attrs)
          (do ;;(println "Element instance")
            [{} (remove empty? (list attrs content))])
          (do ;;(println "NOT Element instance")
            [attrs content])))

      :else (do ;;(println "NOT map attrs: " attrs)
                [{} (remove empty? (list attrs content))])))

(defn element [tag & [attrs & content]]
  ;; (println "ELEMENT: TAG: " tag " ATTRS: " attrs " CONTENT: " content)
  (let [[attribs contents] (parse-elt-args (or attrs {}) (or content '()))
        ;; _ (println "ATTRIBS: " attribs)
        ;; _ (println "CONTENT: " content)
        e (Element. tag (or attribs {}) (or contents '()))]
        ;; e (if (= (type attrs) miraj.markup.Element)
        ;;     (Element. tag {} (remove nil? (apply list attrs content)))
        ;;     (if (map? attrs)
        ;;       (Element. tag (or attrs {}) (flatten (remove nil? content)))
        ;;       (Element. tag {} (remove nil? (apply list attrs)))))]
    #_(println "E: " e)
    e))

(defn cdata [content]
  (CData. content))

(defn xml-comment [content]
  (Comment. content))

;=== Parse-related functions ===
(defn seq-tree
  "Takes a seq of events that logically represents
  a tree by each event being one of: enter-sub-tree event,
  exit-sub-tree event, or node event.

  Returns a lazy sequence whose first element is a sequence of
  sub-trees and whose remaining elements are events that are not
  siblings or descendants of the initial event.

  The given exit? function must return true for any exit-sub-tree
  event.  parent must be a function of two arguments: the first is an
  event, the second a sequence of nodes or subtrees that are children
  of the event.  parent must return nil or false if the event is not
  an enter-sub-tree event.  Any other return value will become
  a sub-tree of the output tree and should normally contain in some
  way the children passed as the second arg.  The node function is
  called with a single event arg on every event that is neither parent
  nor exit, and its return value will become a node of the output tree.

  (seq-tree #(when (= %1 :<) (vector %2)) #{:>} str
            [1 2 :< 3 :< 4 :> :> 5 :> 6])
  ;=> ((\"1\" \"2\" [(\"3\" [(\"4\")])] \"5\") 6)"
 [parent exit? node coll]
  (lazy-seq
    (when-let [[event] (seq coll)]
      (let [more (rest coll)]
        (if (exit? event)
          (cons nil more)
          (let [tree (seq-tree parent exit? node more)]
            (if-let [p (parent event (lazy-seq (first tree)))]
              (let [subtree (seq-tree parent exit? node (lazy-seq (rest tree)))]
                (cons (cons p (lazy-seq (first subtree)))
                      (lazy-seq (rest subtree))))
              (cons (cons (node event) (lazy-seq (first tree)))
                    (lazy-seq (rest tree))))))))))

(defn event-tree
  "Returns a lazy tree of Element objects for the given seq of Event
  objects. See source-seq and parse."
  [events]
  (ffirst
   (seq-tree
    (fn [^Event event contents]
      (cond
        (= :start-element (.type event))
        (Element. (.name event) (.attrs event) contents)
        ;; (= :void-element (.type event))
        ;; (Element. (.name event) (.attrs event) contents))
        ))
    (fn [^Event event] (= :end-element (.type event)))
    (fn [^Event event] (.str event))
    events)))

(defprotocol AsElements
  (as-elements [expr] "Return a seq of elements represented by an expression."))

(defn sexp-element [tag attrs child]
  (cond
   (= :-cdata tag) (CData. (first child))
   (= :-comment tag) (Comment. (first child))
   :else (Element. tag attrs (mapcat as-elements child))))

(extend-protocol AsElements
  clojure.lang.IPersistentVector
  (as-elements [v]
    (let [[tag & [attrs & after-attrs :as content]] v
          [attrs content] (if (map? attrs)
                            [(into {} (for [[k v] attrs]
                                        [k (str v)]))
                             after-attrs]
                            [{} content])]
      [(sexp-element tag attrs content)]))

  clojure.lang.ISeq
  (as-elements [s]
    (mapcat as-elements s))

  clojure.lang.Keyword
  (as-elements [k]
    [(Element. k {} ())])

  java.lang.String
  (as-elements [s]
    [s])

  nil
  (as-elements [_] nil)

  java.lang.Object
  (as-elements [o]
    [(str o)]))

(defn sexps-as-fragment
  "Convert a compact prxml/hiccup-style data structure into the more formal
   tag/attrs/content format. A seq of elements will be returned, which may
   not be suitable for immediate use as there is no root element. See also
   sexp-as-element.

   The format is [:tag-name attr-map? content*]. Each vector opens a new tag;
   seqs do not open new tags, and are just used for inserting groups of elements
   into the parent tag. A bare keyword not in a vector creates an empty element.

   To provide XML conversion for your own data types, extend the AsElements
   protocol to them."
  ([] nil)
  ([sexp] (as-elements sexp))
  ([sexp & sexps] (mapcat as-elements (cons sexp sexps))))

(defn sexp-as-element
  "Convert a single sexp into an Element"
  [sexp]
  (let [[root & more] (sexps-as-fragment sexp)]
    (when more
      (throw
       (IllegalArgumentException.
        "Cannot have multiple root elements; try creating a fragment instead")))
    root))


(defn- attr-prefix [^XMLStreamReader sreader index]
  (let [p (.getAttributePrefix sreader index)]
    (when-not (str/blank? p)
      p)))

(defn- attr-hash [^XMLStreamReader sreader] (into {}
    (for [i (range (.getAttributeCount sreader))]
      [(keyword (attr-prefix sreader i) (.getAttributeLocalName sreader i))
       (.getAttributeValue sreader i)])))

; Note, sreader is mutable and mutated here in pull-seq, but it's
; protected by a lazy-seq so it's thread-safe.
(defn- pull-seq
  "Creates a seq of events.  The XMLStreamConstants/SPACE clause below doesn't seem to
   be triggered by the JDK StAX parser, but is by others.  Leaving in to be more complete."
  [^XMLStreamReader sreader]
  (lazy-seq
   (loop []
     (condp == (.next sreader)
       XMLStreamConstants/START_ELEMENT
       (cons (event :start-element
                    (keyword (.getLocalName sreader))
                    (attr-hash sreader) nil)
             (pull-seq sreader))
       XMLStreamConstants/END_ELEMENT
       (cons (event :end-element
                    (keyword (.getLocalName sreader)) nil nil)
             (pull-seq sreader))
       XMLStreamConstants/CHARACTERS
       (if-let [text (and (not (.isWhiteSpace sreader))
                          (.getText sreader))]
         (cons (event :characters nil nil text)
               (pull-seq sreader))
         (recur))
       XMLStreamConstants/END_DOCUMENT
       nil
       (recur);; Consume and ignore comments, spaces, processing instructions etc
       ))))

(def ^{:private true} xml-input-factory-props
  {:allocator javax.xml.stream.XMLInputFactory/ALLOCATOR
   :coalescing javax.xml.stream.XMLInputFactory/IS_COALESCING
   :namespace-aware javax.xml.stream.XMLInputFactory/IS_NAMESPACE_AWARE
   :replacing-entity-references javax.xml.stream.XMLInputFactory/IS_REPLACING_ENTITY_REFERENCES
   :supporting-external-entities javax.xml.stream.XMLInputFactory/IS_SUPPORTING_EXTERNAL_ENTITIES
   :validating javax.xml.stream.XMLInputFactory/IS_VALIDATING
   :reporter javax.xml.stream.XMLInputFactory/REPORTER
   :resolver javax.xml.stream.XMLInputFactory/RESOLVER
   :support-dtd javax.xml.stream.XMLInputFactory/SUPPORT_DTD})

(defn- new-xml-input-factory [props]
  ;; (let [fac (javax.xml.stream.XMLInputFactory/newInstance)]
  (let [fac (javax.xml.stream.XMLInputFactory/newFactory)]
    (doseq [[k v] props
            :let [prop (xml-input-factory-props k)]]
      (.setProperty fac prop v))
    fac))

(defn source-seq
  "Parses the XML InputSource source using a pull-parser. Returns
   a lazy sequence of Event records.  Accepts key pairs
   with XMLInputFactory options, see http://docs.oracle.com/javase/6/docs/api/javax/xml/stream/XMLInputFactory.html
   and xml-input-factory-props for more information.
   Defaults coalescing true and supporting-external-entities false."
  [s & {:as props}]
  (let [merged-props (merge {:coalescing true
                             :supporting-external-entities false}
                            props)
        fac (new-xml-input-factory merged-props)
        ;; Reflection on following line cannot be eliminated via a
        ;; type hint, because s is advertised by fn parse to be an
        ;; InputStream or Reader, and there are different
        ;; createXMLStreamReader signatures for each of those types.
        sreader (.createXMLStreamReader ^XMLInputFactory fac ^java.io.StringReader s)
        ]
    (pull-seq sreader)))

(defn parse
  "Parses the source, which can be an
   InputStream or Reader, and returns a lazy tree of Element records. Accepts key pairs
   with XMLInputFactory options, see http://docs.oracle.com/javase/6/docs/api/javax/xml/stream/XMLInputFactory.html
   and xml-input-factory-props for more information. Defaults coalescing true."
  [source & props]
  (event-tree (apply source-seq source props)))

(defn parse-str
  "Parses the passed in string to Clojure data structures.  Accepts key pairs
   with XMLInputFactory options, see http://docs.oracle.com/javase/6/docs/api/javax/xml/stream/XMLInputFactory.html
   and xml-input-factory-props for more information. Defaults coalescing true."
  [s & props]
  (let [sr (java.io.StringReader. s)]
    (apply parse sr props)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; XML Emitting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn check-stream-encoding [^java.io.OutputStreamWriter stream xml-encoding]
  (when (not= (Charset/forName xml-encoding) (Charset/forName (.getEncoding stream)))
    (throw (Exception. (str "Output encoding of stream (" xml-encoding
                            ") doesn't match declaration ("
                            (.getEncoding stream) ")")))))

(defn emit
  "Prints the given Element tree as XML text to stream.
   Options:
    :encoding <str>          Character encoding to use
    :with-xml-declaration <bool>, default false"
  [e ^java.io.Writer writer & {:as opts}]
  ;; (println "emit: " e " OPTS: " opts)
  (let [^javax.xml.stream.XMLStreamWriter stream-writer
        (-> (javax.xml.stream.XMLOutputFactory/newInstance)
            (.createXMLStreamWriter writer))]
    (when (instance? java.io.OutputStreamWriter writer)
      (check-stream-encoding writer (or (:encoding opts) "UTF-8")))

    ;; (if (:doctype opts)
    ;;   (do (println "DOCTYPE!!!!")
    ;;       (.writeDTD stream-writer "<!doctype html>")))
    (if (:with-xml-declaration opts)
      (.writeStartDocument stream-writer (or (:encoding opts) "UTF-8") "1.0"))
    (doseq [event (flatten-elements [e])]
      (do ;;(log/trace "event: " event)
          (emit-event event stream-writer writer)))
    ;; (.writeEndDocument stream-writer)
    writer))

#_(defn emit-str
  "Emits the Element to String and returns it.
   Options:
    :encoding <str>          Character encoding to use
    :with-xml-declaration <bool>, default false"
  [e & opts]
  (let [^java.io.StringWriter sw (java.io.StringWriter.)]
    (apply emit e sw opts)
    (.toString sw)))

(defn ^javax.xml.transform.Transformer indenting-transformer []
  (doto (-> (javax.xml.transform.TransformerFactory/newInstance) .newTransformer)
    (.setOutputProperty (javax.xml.transform.OutputKeys/INDENT) "yes")
    (.setOutputProperty (javax.xml.transform.OutputKeys/METHOD) "xml")
    (.setOutputProperty "{http://xml.apache.org/xslt}indent-amount" "2")))

(defn indent
  "Emits the XML and indents the result.  WARNING: this is slow
   it will emit the XML and read it in again to indent it.  Intended for
   debugging/testing only."
  [e ^java.io.Writer stream & {:as opts}]
  (let [sw (java.io.StringWriter.)
        _ (apply emit e sw (apply concat opts))
        source (-> sw .toString java.io.StringReader. javax.xml.transform.stream.StreamSource.)
        result (javax.xml.transform.stream.StreamResult. stream)]
    (.transform (indenting-transformer) source result)))

(defn indent-str
  "Emits the XML and indents the result.  Writes the results to a String and returns it"
  [e]
  (let [^java.io.StringWriter sw (java.io.StringWriter.)]
    (indent e sw)
    (.toString sw)))


#_(defn make-fns
  [tags]
  (log/trace "make-fns " tags) ;; (type tags))
  (doseq [tag tags]
    ;; (log/trace "make-fns tag: " tag (type tag))
    (let [ftag (symbol tag)
          kw   (keyword tag)
          func `(defn ~ftag ;; (symbol (str tag))
                  [& htags#]
                  ;; (log/trace "HTML FN: " ~kw (pr-str htags#))
                  (if (empty? htags#)
                    (element ~kw)
                    (let [first# (first htags#)
                          attrs# (if (map? first#)
                                   (do ;(log/trace "map? first")
                                       (if (instance? miraj.markup.Element first#)
                                         (do ;(log/trace "Element instance")
                                             {})
                                         (do ;(log/trace "NOT Element instance")
                                             first#)))
                                   (do ;(log/trace "NOT map? first")
                                       {}))
                          content# (if (map? first#)
                                     (if (instance? miraj.markup.Element first#)
                                       htags#
                                       (rest htags#))
                                     htags#)
                          func# (apply element ~kw attrs# content#)]
                      ;; (log/trace "htags: " htags#)
                      ;; (log/trace "kw: " ~kw)
                      ;; (log/trace "tags: " attrs#)
                      ;; (log/trace "content: " content# " (" (type content#) ")")
                      ;; (log/trace "func: " func# (type func#))
                      func#)))
          f (eval func)])))

(defn make-void-elt-fns
  [tags]
  ;; (log/trace "make-void-elt-fns " tags) ;; (type tags))
  (doseq [tag tags]
    ;; (log/trace "make-void-elt-fns fn: " tag) ;; (type tag))
    (let [ftag (symbol tag)
          kw   (keyword tag)
          func `(defn ~ftag ;; (symbol (str tag))
                  [& htags#]
                  ;; (log/trace "HTML VOID FN: " ~kw (pr-str htags#))
                  (if (empty? htags#)
                    (element ~kw)
                    (if (not (map? (first htags#)))
                      (throw (Exception. (str "content not allowed in HTML void element " ~kw)))
                      (if (instance? miraj.markup.Element (first htags#))
                        (throw (Exception. (str "content not allowed in HTML void element " ~kw)))
                        (if (not (empty? (rest htags#)))
                          (throw (Exception. (str "content not allowed in HTML void element " ~kw)))
                        (let [func# (apply element ~kw htags#)]
                        ;;   ;; (log/trace "htags: " htags#)
                          ;; (log/trace "kw: " ~kw)
                          ;; (log/trace "tags: " (first htags#))
                          ;; (log/trace "func: " func# (type func#))
                          func#))))))
          f (eval func)])))

(defn make-meta-tag-fns
  [rules]
  ;; (log/trace "make-meta-tag-fns " rules) ;; (type rules))
  (let [meta-name (first rules)
        rules (first (rest rules))]
    (doseq [rule rules]
      (do ;;(println "rule: " rule)
          (let [fn-tag (symbol (subs (str (first rule)) 1))
                ;; _ (println "make-meta-tag-fns fn: " fn-tag)
                fn-validator (last (fnext rule))
                ;; _ (println "make-meta-tag-fns validator: " fn-validator)
                elt (first (fnext rule))
                ;; _ (println (str "make-meta-tag-fns elt: " elt))
                ]
            (eval `(defn ~fn-tag ;; (symbol (str tag))
                     [& fn-args#]
                     ;; (println "FN-args: " fn-args# (count fn-args#))
                     ;; (println "fn-validator: " ~fn-validator)
                     (if-let [msg# (:non-conforming ~fn-validator)]
                       (throw
                        (Exception.
                         (str ~meta-name "='" '~elt "' is a non-conforming feature. " msg#))))
                     (if (empty? fn-args#)
                       (throw (Exception. (str "HTML meta element cannot be empty" ~elt)))
                       (if (> (count fn-args#) 1)
                         (throw (Exception. (str "content not allowed in HTML meta element " ~elt)))
                         (let [attribs# (merge {}
                                               {(keyword ~meta-name) ~(str elt)
                                                :content (str (first fn-args#))})
                               ;; _# (println "ATTRIBS: " attribs# (type attribs#))
                               func# (apply element "meta" (list attribs#))]
                           func#))))))))))

(defn make-tag-fns
  [pfx tags sfx]
  ;; (println "make-tag-fns " pfx tags sfx)
  (doseq [tag tags]
    (do ;;(println "make-tag-fn " tag)
        (let [fn-tag (cond
                     (string? tag) (symbol tag)
                     (vector? tag) (symbol (last tag)))
              elt (keyword (str pfx (cond
                                      (string? tag) tag
                                      (vector? tag) (last tag))))
              ;; log (println "make-tag-fns fn-tag: " fn-tag " (" (type fn-tag) ")")
              func `(defn ~fn-tag ;; (symbol (str tag))
                      [& parms#]
                      ;; (println "HTML FN: " ~elt) ;; (pr-str parms#))
                      (let [args# (flatten parms#)]
                        ;; (println "HTML FLAT: " ~elt (pr-str (flatten args#)))
                        (if (empty? args#)
                          (element ~elt)
                          (let [first# (first args#)
                                rest# (rest args#)
                                [attrs# content#] (parse-elt-args first# rest#)
                                ;; content# (if (map? first#)
                                ;;            (if (instance? miraj.markup.Element first#)
                                ;;              args#
                                ;;              (rest args#))
                                ;;            (if (keyword? first#)
                                ;;              (if (nil? (namespace first#))
                                ;;                args#
                                ;;                (rest args#))
                                ;;              args#))
                                func# (with-meta (apply element ~elt attrs# content#)
                                        {:co-fn true
                                         :elt-kw ~elt
                                         :elt-uri "foo/bar"})]
                          ;; (log/trace "args: " htags#)
                          ;; (log/trace "elt: " ~elt)
                          ;; (log/trace "tags: " attrs#)
                          ;; (log/trace "content: " content# " (" (type content#) ")")
                          ;; (log/trace "func: " func# (type func#))
                            func#))))
              f (eval func)]))))

(defn html-constructor
  [ns-sym nm-sym elt-kw uri & docstring]
  (println "HTML-CONSTRUCTOR:" ns-sym nm-sym elt-kw uri docstring)
  (let [ds (if (empty? docstring) "" (first docstring))]
    (intern ns-sym (with-meta (symbol (str nm-sym)) {:doc ds :uri uri :component true})
            (fn [& args]
              (let [elt (if (empty? args)
                          (do (println "COMPONENT FN NO ARGS: " elt-kw)
                              (element elt-kw))
                           (let [first (first args)
                                 rest (rest args)
                                 [attrs content] (parse-elt-args first rest)]
                             (apply element elt-kw attrs content)))]
                elt)))
                ns-sym))
  ;; (alter-meta! (find-var ns-sym nm-sym)
  ;;              (fn [old new]
  ;;                (merge old new))
  ;;              {:miraj {:co-fn true
  ;;                       :component typ
  ;;                       :doc docstring
  ;;                       :elt-kw elt-kw
  ;;                       :elt-uri elt-uri}}))

(defn make-resource-fns
  [typ tags]
  (do ;;(println "make-resource-fns: " typ tags)
        (doseq [[fn-tag elt-kw elt-uri docstring] tags]
          (do #_(println "make resource:" fn-tag elt-kw elt-uri docstring)
              (eval `(defn ~fn-tag ~docstring
                       [& args#]
;;                       (println "invoking " ~fn-tag)
                       (let [elt# (if (empty? args#)
                                    (with-meta (element ~elt-kw)
                                      {:miraj
                                       {:co-fn true
                                        :component ~typ
                                        :doc ~docstring
                                        :elt-kw ~elt-kw
                                        :elt-uri ~elt-uri}})

                                    ;; (let [attrib-args# (first args#)
                                    ;;       attrs# (if (map? attrib-args#)
                                    ;;                (do ;(log/trace "map? first")
                                    ;;                  (if (instance? miraj.markup.Element attrib-args#)
                                    ;;                    (do ;(log/trace "Element instance")
                                    ;;                      {})
                                    ;;                    (do ;(log/trace "NOT Element instance")
                                    ;;                      attrib-args#)))
                                    ;;                (do ;(log/trace "NOT map? attrib-args#")
                                    ;;                  {}))
                                    ;;       content# (if (map? attrib-args#)
                                    ;;                  (if (instance? miraj.markup.Element attrib-args#)
                                    ;;                    args#
                                    ;;                    (rest args#))
                                    ;;                  args#)]
                                    (let [first# (first args#)
                                          rest# (rest args#)
                                          [attrs# content#] (parse-elt-args first# rest#)]
                                      (with-meta (apply element ~elt-kw attrs# content#)
                                        {:miraj {:co-fn true
                                                 :component ~typ
                                                 :doc ~docstring
                                                 :elt-kw ~elt-kw
                                                 :elt-uri ~elt-uri}})))]
                         elt#)))
              (alter-meta! (find-var (symbol (str *ns*) (str fn-tag)))
                            (fn [old new]
                              (merge old new))
                            {:miraj {:co-fn true
                                     :component typ
                                     :doc docstring
                                     :elt-kw elt-kw
                                     :elt-uri elt-uri}})
              #_(println "var: " (find-var (symbol (str *ns*) (str fn-tag))))))))

(defmacro co-fn
  [fn-tag docstring elt-kw elt-uri typ]
  (do #_(println "co-fn:" typ fn-tag elt-kw elt-uri docstring)
              (eval `(defn ~fn-tag ~docstring
                       [& args#]
;;                       (println "invoking " ~fn-tag)
                       (let [elt# (if (empty? args#)
                                    (with-meta (element ~elt-kw)
                                      {:miraj
                                       {:co-fn true
                                        :component ~typ
                                        :doc ~docstring
                                        :elt-kw ~elt-kw
                                        :elt-uri ~elt-uri}})
                                    (let [first# (first args#)
                                          rest# (rest args#)
                                          [attrs# content#] (parse-elt-args first# rest#)]
                                      (with-meta (apply element ~elt-kw attrs# content#)
                                        {:miraj {:co-fn true
                                                 :component ~typ
                                                 :doc ~docstring
                                                 :elt-kw ~elt-kw
                                                 :elt-uri ~elt-uri}})))]
                         elt#)))
              (alter-meta! (find-var (symbol (str *ns*) (str fn-tag)))
                            (fn [old new]
                              (merge old new))
                            {:miraj {:co-fn true
                                     :component typ
                                     :doc docstring
                                     :elt-kw elt-kw
                                     :elt-uri elt-uri}})))

(defn optimize-js
  [doc]
  ;; (println "JS optimizer: " doc)
  (with-meta
    (xsl-xform xsl-optimize-js doc)
    (meta doc)))

  ;; (let [doc-zip (zip/xml-zip doc)]
  ;;   (seq (-> doc-zip zip/down))

  ;;   ))

(defn optimize-css
  [doc]
   (println "CSS optimizer"))

(defn optimize
  ;;FIXME handle null strategy correctly
  [strategy & doc]
  ;; (println "optimize: " strategy " :: " doc)
  (reset! mode :html)
  (case strategy
    :js (apply optimize-js doc)
    :css (apply optimize-css doc)
    (if (keyword? strategy)
      (throw (Exception. (str "Unrecognize optimization strategy: " strategy)))
      (if (nil? doc)
        (optimize-js strategy)
        (optimize-js doc)))))
;;    (println "Unrecognized optimizer: " strategy)))

(declare <<!)

(defn build-component
  [[html-output-dir cljs-output-dir]
   [csym cvar]]
  ;; [^clojure.lang.Symbol component & mode]
  ;; [file doc & mode]
  (println "Building component: " csym cvar)
  ;; (println "codom: " (-> cvar meta :miraj :codom))
  ;; (println "Output dirs: " html-output-dir cljs-output-dir)
  (let [path (str/replace (str (:ns (meta cvar))) #"\." "/")
        hfile (str/join "/" [html-output-dir path (str csym ".html")])
        cfile (str/join "/" [cljs-output-dir path (str csym ".cljs")])]
    (io/make-parents hfile)
    (io/make-parents cfile)
    (spit hfile (with-out-str (pprint (-> cvar meta :miraj :codom))))
    (spit cfile (-> cvar meta :miraj :prototype))))

  ;; (let [s (if (= (first mode) :pprint)
  ;;           (do (println "pprint")
  ;;               (with-out-str (pprint doc)))
  ;;           (serialize doc))]
  ;;   (spit file s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  ATTRIBS

(def attrs-regex
  #" *[^>]* *>")

(def attrs-overlap-start-regex
  ;; e.g. for a, b, em, i, li etc
  ;; match <li>, <li >, <li/>, <li />
  #" */?>")

(def attrs-overlap-attrs-regex
  ;; e.g. for a, b, em, i, li etc
  ;; match <li>, <li >, <li/>, <li />, <li foo="bar">, <li foo="bar">
  #" +[^>]* */?>")

(def encoding-decl-regex
  ;; https://encoding.spec.whatwg.org/#names-and-labels
  [#"(?i)unicode-1-1-utf-8"
   #"(?i)utf-8"
   #"(?i)utf8"])

;; global attrs: http://www.w3.org/html/wg/drafts/html/master/dom.html#global-attributes
;; meta standard names: :application-name, :author, :description, :generator, :keywords
;; :charset;  :itemprop?
;; extensions: https://wiki.whatwg.org/wiki/MetaExtensions
;; see https://gist.github.com/kevinSuttle/1997924
;; :viewport gets special treatment
;; e.g.  :dc {:created ".." :creator "foo" ...}


;; syntax:
;;    keyword values represent type constraints by name
;;    sets represent enum types
;;    vectors map clj value to html value, e.g. [true "yes"]
;;    quoted vals represent type, plus translate to <link> instead of <meta>

(def mobile-meta-tags
  {:mobile {:agent ^:compound {:format #{:wml :xhtml :html5}
                               :url :_}
            :web-app-capable [true "yes"]}})

(def apple-meta-tags
  ;; https://developer.apple.com/library/safari/documentation/AppleApplications/Reference/SafariHTMLRef/Articles/MetaTags.html
  {:apple {:itunes-app :_
           :mobile-web-app {:capable [true "yes"]
                            :status-bar-style #{:default :black :black-translucent}
                            :title :string}
           :touch {:icon ^:compound {:uri :uri :sizes :sizes}
                   :startup-image :image}
                   ;; apple-touch-fullscreen :_ ;; "not needed anymore"
           :format-detection {:disable "telephone=no"}}})

(def ms-meta-tags
  {:ms {:config :uri
        :navbutton-color :color
        :notification {:?frequency #{30 60 360 720 1440}
                       :?cycle #{0 1 2 3 4 5 6 7}
                       :uri :uri
                       :?uris [:uri] ;; up to 5 total
                       }
        :square-70x70-logo :uri
        :square-150x150-logo :uri
        :square-310x310-logo :uri
        :starturl :uri
        :task {:name :string :action-uri :uri :icon-uri :uri
               :window-type #{:tab :self :window}}
        :tile-color :color
        :tile-image :uri
        :tooltip :string
        :tap-highlight :no
        :wide-310x150-logo :uri
        :window {:width :pixels, :height :pixels}
        ;; other ms metas https://msdn.microsoft.com/library/dn255024(v=vs.85).aspx
        :ms-pinned ^:nonstandard {:allow-domain-api-calls :bool
                                  :allow-domain-meta-tags :bool
                                  :badge {:frequency #{30 60 360 720 1440}
                                          :polling-uri :uri}
                                  :start-url :uri
                                  :task-separator :_}}})

(def twitter-meta-tags
  )

(def html5-meta-attribs-standard
  {:charset :encoding-decl   ;; :content is implicit as value of map entry
   ;; standard vals for name attrib
   :application-name :string
   :author :string
   :description :string
   :generator :string
   :keywords :tokens})

(def html5-meta-attribs-extended
   ;; extended name attribs https://wiki.whatwg.org/wiki/MetaExtensions
  {:viewport ^:compound {:width #{:auto :device :pixels}
                         :height #{:auto :device :pixels}
                         :scale {:initial :number
                                 :minimum :number
                                 :maximum :number}
                         :user-scalable :boolean}
   ;; dublin core
   :dc {:created :_, :creator :_} ;; etc.
   :dc-terms {:created :_, :creator :_} ;; etc.
   :fragment "!"
   :geo {:position :geocoord, :country :iso3166-1} ;; etc.
   :referrer #{:no-referrer :no-referrer-when-downgrade
               :origin :origin-when-cross-origin
               :unsafe-url}
   :revision :_
   :theme-color :color
   :twitter {:card :_
             :domain :_
             :url :_
             :title :_
             :description :_
             ;; etc.
             }})

(def html5-link-meta
  ;; link elements treated as meta-data
 {:icon ^:compound {:uri :uri :sizes :sizes}
  :manifest :uri})

(def html5-miraj-meta-attribs
  {:platform (merge apple-meta-tags ms-meta-tags mobile-meta-tags)})

(def html5-meta-attribs
  (merge {} html5-global-attrs
         html5-meta-attribs-standard
         html5-meta-attribs-extended
         html5-miraj-meta-attribs
         html5-link-meta))
         ;; apple-meta-tags ms-meta-tags))

(defn do-viewport
  [tag key val ruleset]
      ;; :viewport {:width :device
      ;;        :scale {:initial "1.0" :min "1.0"}
      ;;        :user-scalable true}
  ;; (println "do-viewport: " tag " | " key "|" val "|" ruleset)
  (let [w (:width val)
        width (str "width=" (if (= :device w) "device-width" w))
        initial-scale (str "initial-scale=" (get-in val [:scale :initial]))
        min-scale (str "minimum-scale=" (get-in val [:scale :min]))
        max-scale (str "maximum-scale=" (get-in val [:scale :max]))
        user-scalable (str "user-scalable=" (if (:user-scalable val) "yes" "no"))
        content (str/join ", " [width initial-scale min-scale max-scale user-scalable])]
    (element :meta {:name "viewport" :content content})))

(defn apply-meta-rule
  [tag key val ruleset]
  ;; (println (str "APPLY META RULE: " tag " | " key " | " val " | " ruleset))
  (condp = key
    :viewport (do-viewport tag key val ruleset)
    (let [this-tag (if (= :ms key) "msapplication" (subs (str key) 1))
          result (for [[k v] val]
                   (do ;;(println "key: " k ", val: " v)
                       (if-let [rule (get ruleset k)]
                         (let [k-tag (subs (str k) 1)]
                           ;; (println "rule: " rule)
                           (cond
                             (keyword? rule)
                             (do ;;(println "meta keyword rule: " k ", " val ": " rule)
                                 (let [val-param (get val k)
                                       nm (if (= "platform-" tag)
                                            (str this-tag "-" k-tag)
                                            (str tag this-tag "-" k-tag))
                                       elt (condp = rule
                                             :color (element :meta {:name nm
                                                                    :content (str val-param)})
                                             :boolean (element :meta {:name nm
                                                                      :content (str val-param)})
                                             :number (element :meta {:name nm
                                                                     :content (str val-param)})
                                             :pixels (element :meta {:name nm
                                                                     :content (str val-param)})
                                             :string (element :meta {:name nm
                                                                     :content (str val-param)})
                                             :sizes (element :meta {:name nm
                                                                    :content (str val-param)})
                                             :uri (element :meta {:name nm
                                                                  :content (str val-param)})
                                             :_ (element :meta {:name nm
                                                                :content (str val-param)})
                                             ;; :tokens
                                             )]
                                   ;;(log/trace "elt: " elt)
                                   elt))

                             (map? rule) (do ;;(println "meta map key: " k)
                                             ;; (println "meta map val: " (get val k))
                                             ;; (println "rule: " rule)
                                             ;; (println "tag: " tag)
                                             ;; (println "this-tag: " this-tag)
                                             (if (:compound (clojure.core/meta rule))
                                               (do
                                                   (let [nm (if (= "platform-" tag)
                                                              (str this-tag "-" k-tag)
                                                              (str tag this-tag "-" k-tag))
                                                         content (str/join
                                                                  "; " (for [[k v] (get val k)]
                                                                         (str (subs (str k) 1)
                                                                              "="
                                                                              (if (keyword? v)
                                                                                (subs (str v) 1)
                                                                                (str v)))))]
                                                 (element :meta {:name nm :content content})))
                                               (apply-meta-rule
                                                (if (= this-tag :platform)
                                                  ""
                                                  (str this-tag "-"))
                                                  k (get val k) rule)))

                             (set? rule)
                             (do ;;(println "meta set rule: " k rule)
                                 (let [val-param (get val k)]
                                   ;; (println "val: " val-param)
                                   (if (contains? rule val-param)
                                     (let [nm (str tag this-tag "-" k-tag)
                                           content (subs (str val-param) 1)]
                                       (element :meta {:name nm :content content}))
                                     (throw (Exception. (str "META: unrecognized enum option: "
                                                             key " {" k " " v"}"))))))

                             (vector? rule)
                             (do ;;(log/trace "meta vector rule: " k ", " val ": " rule)
                               (let [v (val k)]
                                 ;;(log/trace "found val: " v)
                                 (if (= v (first rule))
                                   (let [nm (if (= "platform-" tag)
                                              (str this-tag "-" k-tag)
                                              (str tag this-tag "-" k-tag))
                                         content (second rule)]
                                     ;;(log/trace nm  "=\"" content "\"")
                                     (element :meta {:name nm :content content}))
                                   (throw (Exception. (str "META: unrecognized option: " key " {" k " " v"}"))))))
                             :else (throw (Exception.
                                           (str "META: unrecognized option type: "
                                                key " {" k " " v"}" (type rule)))))))))]
      (doall result)
    result)))

(defn get-metas
  [metas]
  ;; (println "GET-METAS " metas)
  ;; (println "HTML5-METAS " (keys html5-meta-attribs))
  (let [ms (for [[tag val] metas]
             (let [rule (get html5-meta-attribs tag)]
               ;; (println "META: " tag (pr-str val) " RULE: " rule)
               (if (nil? rule) (throw (Exception. (str "unknown meta name: " (str tag)))))
               (if (keyword? rule)
                 ;; FIXME: validation
                 (let [m (element :meta {:name (subs (str tag) 1) :content (str val)})]
                   ;; (println "META ELT: " m)
                   m)
                 (apply-meta-rule "" tag val rule))))]
               ;; (case tag
               ;;   :apple (let [apple (apply-meta-rule "" tag val rule)]
               ;;            #_(log/trace "APPLE: " apple) apple)
               ;;   :msapplication (let [ms (apply-meta-rule "msapplication" tag val rule)]
               ;;                    #_(log/trace "MSAPP: " ms) ms)
               ;;   :mobile (let [ms (apply-meta-rule "" tag val rule)]
               ;;             #_(log/trace "MOBILE: " ms) ms)
               ;;   (element :meta {:name (subs (str tag) 1)
               ;;            :content (str val)}))))]
    ;; force eval, for printlns
    (doall ms)
    ;; (println "METAS: " ms)
    ms))

(defn platform
  [{:keys [apple ms mobile]}]
  ;; (println "apple: " apple)
  ;; (println "ms: " ms)
  ;; (println "mobile: " mobile)
  (merge (apply-meta-rule "" :apple apple (:apple apple-meta-tags))
         (apply-meta-rule "" :msapplication ms (:msapplication ms-meta-tags))
         (apply-meta-rule "" :mobile mobile (:mobile mobile-meta-tags))))

(defn get-meta-elts
  [args]
  ;; (println "get-meta-elts: " args)
  (for [[k v] (apply meta args)]
    (element :meta {:name (kw->nm k)
                    :content (str v)})))

(defn normalize
  "inspect args, if necessary create <head> etc."
  [& args]
  ;; (println "normalize HTML args: " args)
  ;; (println "normalize HTML meta: " (apply meta args))
  (reset! mode :html)
  (let [meta-elts (get-metas (dissoc (apply meta args) :elt-kw :elt-uri :co-fn))
        ;; _ (println "META-ELTS: " meta-elts)
        h (list
           (update (first args)
                   :content
                   (fn [content]
                     (if meta-elts
                       (concat meta-elts content)
                       content))))
        ;; _ (println "H: " h)
        normh (apply xsl-xform xsl-normalize h)
        ]
    normh))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  require/import
(defn verify-resource
  [uri spec]
  (println "verify-resource: " uri spec)
  (if (.startsWith uri "http")
    true
    (if-let [res (io/resource uri)]
      res (throw (Exception. (str "Resource '" uri "' not found in classpath; referenced by spec: " spec))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;FIXME: do we really want polymer stuff in here?
(def polymer-nss #{"iron" "paper" "google" "gold" "neon" "platinum" "font" "molecules"})

(defn get-href
  ([pfx sfx]
   (println "get-href: " pfx " - " sfx (type sfx))
   (let [pfx (str/split (str pfx) #"\.")
         hd (first pfx)]
     ;; (log/trace "get-href pxf: " pfx)
     (cond
       (= hd "polymer")
       (let [pns (second pfx)]
         (if (not (contains? polymer-nss pns))
           (throw (RuntimeException. (str "unsupported namespace: " pns " | " pfx " | " polymer-nss))))
         (if sfx
           (cond
             (and (= pns "paper") (= sfx 'textarea))
             (do ;;(log/trace "TEXTAREA!!!!!!!!!!!!!!!!")
               (str "polymer/paper-input/paper-textarea.html"))

             (= pns "font") (str hd "/" pns "-" sfx "/" sfx ".html")
             :else (str hd "/" pns "-" sfx "/" pns "-" sfx ".html"))
           (str hd "/" pns "-elements.html")))
       :else
       (str (str/join "/" pfx) "/" sfx))))
  ([sym]
  (println "get-href: " sym (type sym))
  (let [pfx (if (namespace sym) (str/split (namespace sym) #"\.") "")
        sfx (name sym)
         hd (first pfx)]
    ;; (log/trace "get-href pxf: " pfx)
    (cond
      (= hd "polymer")
      (let [pns (second pfx)]
        (if (not (contains? polymer-nss pns))
          (throw (RuntimeException. (str "unsupported namespace: " pns " | " pfx " | " polymer-nss))))
        (if sfx
          (cond
            (and (= pns "paper") (= sfx 'textarea))
            (do ;;(log/trace "TEXTAREA!!!!!!!!!!!!!!!!")
                (str "polymer/paper-input/paper-textarea.html"))

            (= pns "font") (str hd "/" pns "-" sfx "/" sfx ".html")
            :else (str hd "/" pns "-" sfx "/" pns "-" sfx ".html"))
          (str hd "/" pns "-elements.html")))
      :else
      (str (str/join "/" pfx) "/" sfx)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti get-resource-elt
  (fn [typ nsp sym spec]
    ;; (println (str "GET-RESOURCE-elt: " typ " " nsp " " sym))
    typ))

(defmethod get-resource-elt :default
  [typ nsp sym spec]
  ;; (println (str "get-resource-elt :default: " typ " " nsp " " sym))
  (throw (Exception. (str "Unrecognized resource type for require: " spec))))
  ;; (element :link
  ;;          {:rel "import" :href (get-href (ns-name nsp) ref)}))

(defmethod get-resource-elt :polymer
  [typ nsp sym spec]
  (println "GET-RESOURCE-ELT polymer: NS: " nsp " SYM: " sym)
  (println "spec: " spec)
  (let [pfx (:resource-pfx (meta nsp))
        path (:elt-uri (:miraj (meta (find-var sym))))
        uri (str pfx "/" path)]
    (println "META on sym:")
    (pp/pprint (meta (find-var sym)))
    (println "META KEYS: " (keys (meta (find-var sym))))
    (println "URI: " uri)
    ;; (let [iores (if-let [res (io/resource uri)]
    ;;               res (throw (Exception.
    ;;                           (str/join "\n"
    ;;                                     ["Polymer resource: "
    ;;                                      (str \tab uri)
    ;;                                      "not found in classpath; referenced by 'require' spec:"
    ;;                                      (str \tab spec \newline)]))))
    ;;       ;; _ (println "IO RES: " iores)
    ;;       ]
      (element :link {:rel "import" :href uri})))

  ;; (get-href (ns-name nsp) ref)}))

(defmethod get-resource-elt :link
  [typ nsp sym spec]
  (println "GET-RESOURCE-ELT :link: " (str typ " " nsp " " sym))
  (element :link
           {:rel "import" :href (get-href (ns-name nsp) ref)}))

;; FIXME:  css and js should be imported, not required
;; (defmethod get-resource-elt :css
;; ;; FIXME: support all attribs
;;   [typ nsp sym spec]
;;   ;; (println "get-resource-elt :css: " (str typ " " nsp " " sym))
;;   (let [uri (deref (find-var sym))]
;;   (element :link {:rel "stylesheet" :href (:uri uri)
;;                   :media (if (:media uri) (:media uri) "all")})))

;; (defmethod get-resource-elt :js
;; ;;FIXME support all attribs
;;   [typ nsp sym spec]
;;   ;; (println "get-resource-elt :js: " (str typ " " nsp " " sym))
;;   (element :script {:src (deref (find-var sym))}))

(defn require-resource
  [spec]
  ;; (println (str "REQUIRE-RESOURCE: " spec))
  ;; at this point, the ns has been required, but not the fns
  ;; tasks:  1. create the fns based on :refer; 2. generate the <link> elts
  (let [ns-sym (first spec)
        ;; _ (println "ns sym: " ns-sym)
        ns-obj (find-ns ns-sym)
        ;; _ (println "ns obj: " ns-obj)
        ;; _ (println "ns obj map?: " (map? ns-obj))
        ;; _ (println "ns meta: " (meta ns-obj))
        pfx-sym (symbol (str ns-sym) "pfx")
        ;; _ (println "ns pfx-sym: " pfx-sym)
        pfx-var  (resolve pfx-sym)
        ;; _ (println "ns pfx-var: " pfx-var)
        pfx (if pfx-var (deref pfx-var) "") ;;(throw (Exception. (str "Resource " pfx-sym " not found."))))
        ;; _ (println "ns pfx val: " (pr-str pfx))
        options (apply hash-map (rest spec))
        as-opt (:as options)
        ;; _ (println ":as " as-opt)
        refer-syms (doall (:refer options))
        ;; _ (println ":refer " refer-syms)
        resource-type (:resource-type (meta ns-obj))
        ;; _ (println "RESOURCE-TYPE: " resource-type)
        ]

    (if (= :polymer resource-type)
      (do
        (if as-opt
          (do (println "ALIASING 1: " as-opt (type as-opt) ns-sym (type ns-sym))
              (clojure.core/alias as-opt ns-sym)
              (println "aliases for " *ns* ": " (ns-aliases *ns*))))
        (cond
          (nil? refer-syms)
          (let [uri ;;(get-href ns-sym)
                (str ns-sym)
                #_(str/replace (str ns-sym) #"\." "/")]
            ;;(if verify? (verify-resource uri spec))
            (element :link {:rel "import" :href uri}))

          ;; (= :all refer-syms)
          ;; iterate over everything in the ns map

          :else
          ;; at this point, the ns has been required
          ;; we just need to get the metadata for the sym
          (do ;;(println "FOR [REF " refer-syms "]")
            (for [ref refer-syms]
              (let [ref-sym (symbol (str ns-sym) (str ref))
                    _ (println "\tREF: " ref)
                    _ (println "\trefsym: " ref-sym)
                    _ (println "\trefsym meta:")
                    _ (pp/pprint (meta (find-var ref-sym)))
                    ref-var (find-var ref-sym)]
                (if ref-var
                  (do ;;(println "ref-var: " ref-var)
                    ;; (println "ref-var meta: " (meta ref-var))
                    (if (bound? ref-var)
                      (if (:component (meta ref-var))
                        (let [uri (:uri (meta ref-var))]
                          ;; FIXME: this verify doesn't work with user-defined components
                          ;; (if verify? (verify-resource uri spec))
                          (element :link {:rel "import" :href uri}))
                        (throw (Exception. (str "var " ref-var " not a component"))))
                      (throw (Exception. (str "ref-var " ref-var " not bound")))))
                  (let [;;_ (println "var for sym: " ref-sym " not found in ns; searching map")
                        ns-map-sym (symbol (str ns-sym) (str ns-sym))
                        ;; _ (println "ns-map-sym: " ns-map-sym)
                        ns-map-var (resolve ns-map-sym)
                        ;; _ (println "ns-map-var: " ns-map-var)
                        ns-map (if ns-map-var
                                 (deref ns-map-var)
                                 (throw (Exception. (str "Symbol '" ref-sym "' unresolvable"))))
                        ;; _ (println "ns-map val: " ns-map)

                        ref-kw (keyword ref)
                        ;; _ (println "ref kw:" ref-kw)
                        elt-spec (get ns-map ref-kw)
                        ;; _ (println "elt-spec: " (pr-str elt-spec))
                        elt-kw (first elt-spec)
                        ;; _ (println "elt-kw: " elt-kw)
                        uri (str pfx "/" (second elt-spec))
                        ;; _ (println "link href: " uri)
                        ]
                    (if (nil? elt-kw) (throw (Exception. (str "definition for '" ref-sym "' not found"))))
                    ;; step 2
                    ;;(html-constructor ns-sym ref elt-kw uri)
                    ;; (println "*NS* " *ns*)
                    ;; step 3
                    #_(if verify? (verify-resource uri spec))
                    (element :link {:rel "import" :href uri})))))))))))
;; (get-resource-elt ns-type ns-obj ref-sym comp)))))))

(defn resolve-require-refs
  [spec]
  (println (str "RESOLVE-REQUIRE-REFS: " spec))
  ;; at this point, the ns has been required, but not the fns
  ;; tasks:  1. create the fns based on :refer; 2. generate the <link> elts
  (let [ns-sym (first spec)
        _ (println "ns sym: " ns-sym)
        ns-obj (find-ns ns-sym)
        _ (println "ns obj: " ns-obj)
        _ (println "ns obj map?: " (map? ns-obj))
        _ (println "ns meta: " (meta ns-obj))
        pfx-sym (symbol (str ns-sym) "pfx")
        _ (println "ns pfx-sym: " pfx-sym)
        pfx-var  (resolve pfx-sym)
        _ (println "ns pfx-var: " pfx-var)
        pfx (if pfx-var (deref pfx-var) "") ;;(throw (Exception. (str "Resource " pfx-sym " not found."))))
        _ (println "ns pfx val: " (pr-str pfx))
        options (apply hash-map (rest spec))
        as-opt (:as options)
        _ (println ":as " as-opt)
        refer-syms (doall (:refer options))
        _ (println ":refer " refer-syms)
        resource-type (:resource-type (meta ns-obj))
        _ (println "RESOURCE-TYPE: " resource-type)
        ]
    (println "LET FINISHED")
    (if (= :polymer resource-type)
      (do
        (if as-opt
          (do (println "ALIASING 2: " *ns* ": " as-opt ns-sym)
              (alias as-opt ns-sym)
              #_(println "aliases for " *ns* ": " (ns-aliases *ns*))))
        (doseq [[isym# ivar#] (ns-interns ns-sym)] (println "INTERNED: " isym# ivar#))
        (cond
          (nil? refer-syms)
          nil
          ;; (= :all refer-syms)
          ;; iterate over everything in the ns map

          :else
          (do
            (println "FOR [REF " refer-syms "]")
            (doall
             (for [ref refer-syms]
               (let [ref-sym (symbol (str ns-sym) (str ref))
                     _ (println "\tREF: " ref)
                     _ (println "\trefsym: " ref-sym)
                     _ (println "\trefsym meta:")
                     _ (pp/pprint (meta (find-var ref-sym)))
                     ref-var (find-var ref-sym)
                 ;; (if ref-var
                 ;;   (do (println "ref-var: " ref-var)
                 ;;       (println "ref-var meta: " (meta ref-var))
                 ;;       (if (bound? ref-var)
                 ;;         (do (println "VAR IS BOUND: " ref-var)
                 ;;             (if (not (:component (meta ref-var)))
                 ;;               ;; (let [ref-nm (-> (meta ref-var) :name)]
                 ;;               ;;   (html-constructor ns-sym ref-nm ref-nm
                 ;;               ;;                     (-> (meta ref-var) :uri))
                 ;;               (throw (Exception. (str "var " ref-var " not a component"))))
                 ;;             (throw (Exception. (str "ref-var " ref-var " not bound")))))
                 ;;   (let [_ (println "var for sym: " ref-sym " not found in ns; searching map")
                     ns-map-sym (symbol (str ns-sym) "components")
                     _ (println "ns-map-sym: " ns-map-sym)
                     ns-map-var (resolve ns-map-sym)
                     _ (println "ns-map-var: " ns-map-var)
                     ns-map (if ns-map-var
                              (deref ns-map-var)
                              (throw (Exception. (str "Symbol '" ref-sym "' unresolvable"))))
                     ;; _ (println "ns-map val: " ns-map)

                     ref-kw (keyword ref)
                     ;; _ (println "ref kw:" ref-kw)
                     elt-spec (get ns-map ref-kw)
                     ;; _ (println "elt-spec: " (pr-str elt-spec))
                     elt-kw (first elt-spec)
                     ;; _ (println "elt-kw: " elt-kw)
                     uri (str pfx "/" (second elt-spec))
                     ;; _ (println "link href: " uri)
                     ]
                 (if (nil? elt-kw) (throw (Exception. (str "definition for '" ref-sym "' not found"))))
                    ;; step 2
                 (html-constructor ns-sym ref elt-kw uri)
                 (println "interned: " (ns-resolve ns-sym ref)))))))))))

;; (require [[polymer.paper :as paper :refer [button card]]])
(defmacro require
  "1. clojure.core/require the ns  2. generate the <link> elts
  NB: for clojure.core/require to work the :refer items must be def'd in the ns
  so to support jit loading we need to require first w/o :refer, then define the :refers,
  then alias as needed"
  [& args]
  ;; step 1: clojure.core/require the namespaces, without options
  (doseq [arg args]
    ;;HACK TODO: validate
    (println "COMPILING REQUIRE")
    (eval
     (macroexpand
      `(let [ns-basic# (first ~arg)]
         (println "CLOJURE.CORE/REQUIRE: " ns-basic#)
         (clojure.core/require ns-basic# :reload)
         ;; (doseq [[isym# ivar#] (ns-interns ns-basic#)] (println "INTERNED: " isym# ivar#))
         ;; (doseq [[isym# ivar#] (ns-aliases ns-basic#)] (println "ALIAS: " isym# ivar#))
         ;; make sure file actually has ns decl
         (if (find-ns ns-basic#) nil (throw (Exception. (str "ns not declared: " ns-basic#))))
         ;; make sure components map is defined
         (if (not (ns-resolve ns-basic# (symbol "components")))
           (throw (Exception. (str "components map not defined in : " ns-basic#))))
  ;; step 2: resolve the referenced syms and generate html element fns
         (resolve-require-refs ~arg)))))

  ;; step 3: for each :refer, generate a <link> element
  ;; require-resource does both
  (println "EXPANDING REQUIRE")
  `(do
     (println "REQUIRing: " [~@args])
     (let [link-elts# (for [arg# [~@args]]
                        (do ;;(println "GET-REQ: " arg#)
                          (let [r# (require-resource arg#)]
                            (doall r#)
                            r#)))]
       (doall link-elts#)
       (println "REQUIREd: " link-elts#)
                          link-elts#)))

           ;;   (element :foo))))))

;;             (first r#))))))

;    (element :foo)))

;;  `(clojure.core/require ~@args))

  ;; `(flatten
  ;;   (for [arg# ~args]
  ;;     (do
  ;;       (println REQUIRING: " arg#)
  ;;       (clojure.core/require arg#)
  ;;       (flatten (require-resource arg#))))))

;;;;;;;;;;;;;;;;  importing

(defmulti import-resource
  (fn [typ spec]
    ;; (println (str "IMPORT-RESOURCE: " typ " " spec))
    typ))

(defmethod import-resource :default
  [typ spec]
  ;; (println "import-resource :default: " typ spec)
  (element :FAKEELEMENT))

(defmethod import-resource :css
  [typ spec]
  (println "IMPORT-RESOURCE :CSS: " typ spec)
  (let [nsp (first spec)
        _ (println "NSP: " nsp (type nsp) (namespace nsp))
        import-ns (find-ns nsp)
        ;; _ (eval (macroexpand
        _ (use (list nsp))
        _ (println "import ns: " import-ns)
        _ (println "import ns meta: " (meta import-ns))
        resource-type (:resource-type (meta import-ns))
        styles (rest spec)
        ;; _ (println "styles : " styles)
        result
        (for [style styles]
          (do ;;(println "style name: " style)
              (let [style-sym (symbol (str (ns-name import-ns)) (str style))
                    _ (println "style-sym: " style-sym (type style-sym))
                    style-ref (if-let [sref (find-var style-sym)]
                                (deref sref)
                                (throw (Exception. (str "CSS resource '" style-sym "' not found; referenced by 'import' spec: " spec))))
                    ;; _ (println "style ref: " style-ref)
                    uri (:uri style-ref)
                    ;; _ (println "uri: " uri)

                    ;; iores (if-let [res (io/resource uri)]
                    ;;         res (throw (Exception. (str "CSS resource '" uri "' not found in classpath; referenced by 'import' spec: " spec))))
                    ;; _ (println "IO RES: " iores)
                    style-var (resolve style-sym)]
                (if (nil? style-var)
                  (throw (Exception. (str "Style '" style "' not found in namespace '" nsp "'")))
                  (do ;;(println "style var: " style-var)
                      (let [style-ref (deref (find-var style-sym))
                            ;; _ (println "style ref: " style-ref)
                            uri (:uri style-ref)]
                        #_(if verify? (verify-resource uri spec))
                        (element :link {:rel "stylesheet"
                                         :href uri})))))))]
    (doall result) ;; force printlns
    result))

(defmethod import-resource :js
  [typ spec]
  (println "import-resource :js: " typ spec)
  (let [nsp (first spec)
        import-ns (find-ns nsp)
        ;; _ (println "import ns: " import-ns)
        ;; _ (println "import ns meta: " (meta import-ns))
        _ (clojure.core/require nsp)
        resource-type (:resource-type (meta import-ns))
        scripts (rest spec)
        ;; _ (println "scripts : " scripts)
        result (into '()
                     (for [script (reverse scripts)]
                       (do ;;(println "script name: " script)
                           (let [script-sym (symbol
                                             (str (ns-name import-ns)) (str script))
                                 ;; _ (println "script-sym: " script-sym)
                                 ;; script-ref (deref (find-var script-sym))
                                 script-ref (if-let [sref (find-var script-sym)]
                                             (deref sref)
                                             (throw (Exception. (str "Javascript resource '" script-sym "' not found; referenced by 'import' spec: " spec "; configured by " import-ns))))
                                 ;; _ (println "script ref: " script-ref)
                                 uri (:uri script-ref)
                                 ;; _ (println "uri: " uri)
                                 ;; iores (verify-resource uri spec)
                                 ;; _ (println "IO RES: " iores)
                                 ]
                             #_(if verify? (verify-resource uri spec))
                             (element :script {:type "text/javascript"
                                               :src uri})))))]
    ;; (doall result)
    ;; (println "RESULT: " result)
    result))

(defmethod import-resource :html-import
  [typ spec]
  (println "IMPORT-RESOURCE :HTML-IMPORT: " typ spec)
  (let [nsp (first spec)
        import-ns (find-ns nsp)
        ;; _ (println "import ns: " import-ns)
        ;; _ (println "import ns meta: " (meta import-ns))
        _ (clojure.core/require nsp)
        resource-type (:resource-type (meta import-ns))
        themes (rest spec)
        ;; _ (println "themes : " themes)
        result (into '()
                     (for [theme (reverse themes)]
                       (do ;;(println "theme name: " theme)
                           (let [theme-sym (symbol
                                             (str (ns-name import-ns)) (str theme))
                                 ;; _ (println "theme-sym: " theme-sym)
                                 ;; theme-ref (deref (find-var theme-sym))
                                 theme-ref (if-let [sref (find-var theme-sym)]
                                             (deref sref)
                                             (throw (Exception. (str "Theme resource '" theme-sym "' not found; referenced by 'import' spec: " spec "; configured by " import-ns))))
                                 ;; _ (println "theme ref: " theme-ref)
                                 uri (:uri theme-ref)
                                 ;; _ (println "uri: " uri)
                                 ;; iores (verify-resource uri spec)
                                 ;; _ (println "IO RES: " iores)
                                 ]
                             #_(if verify? (verify-resource uri spec))
                             (element :link {:rel "import"
                                             :href uri})))))]
    ;; (doall result)
    ;; (println "RESULT: " result)
    result))

(defmethod import-resource :polymer-style-module
  [type spec]
  (println "IMPORT-RESOURCE :POLYMER-STYLE-MODULE: " spec)
  (let [nsp (first spec)
        import-ns (find-ns nsp)
        ;; _ (println "import ns: " import-ns)
        ;; _ (println "import ns meta: " (meta import-ns))
        resource-type (:resource-type (meta import-ns))
        styles (rest spec)
        ;; _ (println "styles : " styles)

        style-pfx-sym (symbol (str (ns-name import-ns)) "pfx")
        _ (println "style-pfx-sym: " style-pfx-sym)
        style-pfx-var (find-var style-pfx-sym)
        _ (println "style-pfx-var: " style-pfx-var)
        style-pfx (if style-pfx-var (deref style-pfx-var)
                      (throw (Exception. (str "Resource " style-pfx-sym " not found"))))
        _ (println "style-pfx: " style-pfx)

        style-path-sym (symbol (str (ns-name import-ns)) "uri")
        ;; _ (println "style-path-sym: " style-path-sym)
        style-path-var (find-var style-path-sym)
        ;; _ (println "style-path-var: " style-path-var)
        style-path (deref style-path-var)
        ;; _ (println "style-path: " style-path)

        style-uri (str style-pfx "/" style-path)
        ;; _ (println "style-uri: " style-uri)
        ;; iores (if verify? (verify-resource style-uri spec))

        result
        (concat
         (list (element :link {:rel "import" :href (str "/" style-uri)}))
         ;; SHARED STYLES!
         (for [style styles]
           (do (println "style name: " style)

               ;;FIXME - this verification doesn't work with style modules
               ;; (let [style-sym (symbol
               ;;                  (str (ns-name import-ns)) (str style))
               ;;       _ (println "style-sym: " style-sym)
               ;;       style-var (find-var style-sym)
               ;;       _ (println "style-var: " style-var)
               ;;       style-ref (if style-var
               ;;                   (deref style-var)
               ;;                   (throw (Exception. (str "symbol '" style-sym "' not found"))))]

                 ;;TODO verify ref'ed custom style is actually in the style module resource
                 ;; (println "style ref: " style-ref)
                 (element :style {;; :is "custom-style"
                                  :include (str style)}))))]
    result))

(defn get-import
  [import]
  ;; (println (str "get-import: " import))
  ;; (println (str "ns: " (first import) " " (type (first import))))
  (let [nsp (first import)]
    (clojure.core/require nsp)
    (let [import-ns (find-ns nsp)
          resource-type (:resource-type (meta import-ns))
          ;; (println "import ns: " nsp (meta import-ns))
          ;; (println "import resource type: " resource-type)
          result (import-resource resource-type import)]
      ;; (doall result)
      ;; (println "get-import RESULT: " result)
      result)))

(defmacro import
  [& args]
  (println "IMPORT: " args)
  (let [args (if (= :verify (first args))
               (do
                 (reset! verify? true)
                 (rest args))
               (do
                 (reset! verify? false)
                 args))]
    `(do
       (println "IMPORTING: " [~@args])
       (let [reqs# (flatten (for [arg# [~@args]]
                              (do ;;(println "GET-IMP: " arg#)
                                (let [r# (get-import arg#)]
                                  ;; force realization, for printlns
                                  ;; (doall r#)
                                  r#))))]
         ;; force realization of lazy seq, so printlns will work
         ;; (doall reqs#)
         ;; (println "IMPORTed: " reqs#)
         reqs#))))

(defn meta-map
  [m]
  ;; (println "meta-map: " m)
  (get-metas m))

(defmacro codom
  [nm & args]
  (println "CODOM: " (str nm)) ;; " ARGS: " args)
  `(do ;;(println "codom: " ~(str nm) ~@args)
       (let [tree# (apply element :CODOM_56477342333109 {:id ~(str nm)} (list ~@args))]
         (xsl-xform xsl-normalize-codom tree#))))

(defmacro def-codom
  [nm & args]
  ;; (println "DEF-CODOM: " (str nm)) ;; " ARGS: " args)
  (let [ns-sym *ns* ; (ns-name *ns*)
        nm-sym (symbol (name nm))
        [docstr args] (if (string? (first args))
                        [(first args) (rest args)]
                        ["" args])
        [argvec args] [(first args) (rest args)]]
    ;; (println "    NS: " (pr-str ns-sym))
    ;; (println "    NM_SYM: " (pr-str nm-sym))
    ;; (println "    DOCSTR: " (pr-str docstr))
    ;; (println "    DOCSTR: " (pr-str docstr))
    ;; (println "    ARGVEC: " (pr-str argvec))
    ;; (println "    ARGS: " (pr-str args))
  `(do ;;(println "codom: " ~(str nm) ~@args)
       (let [tree# (apply element ;;~(keyword nm)
                          :CODOM_56477342333109
                          {:id ~(str nm)} (list ~@args))
             codom# (xsl-xform xsl-normalize-codom tree#)]
         (intern *ns* (with-meta (symbol ~(str nm-sym)) {:doc ~docstr :codom true})
                 codom#)
         codom#))))

                 ;; (fn [& args#]
                 ;;   (let [elt (if (empty? args#)
                 ;;               (element elt-kw)
                 ;;               (let [first (first args)
                 ;;                     rest (rest args)
                 ;;                     [attrs content] (parse-elt-args first rest)]
                 ;;                 (apply element elt-kw attrs content)))]
                 ;;     elt)))))))

(def polymer-prop-descriptors
  {:type "type", :value "value", :notify "notify", :read-only "readOnly",
   :reflect-to-attribute "reflectToAttribute", :observer "observer", :computed "computed"})

(defn construct-properties-js
  [props]
  (str "properties: {\n\t    "
       (str/join ",\n\t    "
                 (for [prop props]
                   (str prop ": {"
                        (str/join ", " (for [[k v] (meta prop)]
                                         (let [nm (get polymer-prop-descriptors k)]
                                           (if nm
                                             (str nm ": " (pr-str v))
                                             (throw (Exception. (str "Invalid property descriptor: " k)))))))
                        "}")))
       "\n\t  }"))

(defn normalize-methods
  [methods]
  ;; (println "NORMALIZE-METHODS")
  (into {} (for [method methods]
             {(if (= 'with-element (first method))
                (keyword (str (name (-> method next first)) "." (-> method nnext first first)))
                (keyword (first method)))
              (if (= 'with-element (first method))
                [(keyword (gensym (str "_" (name (-> method next first)) "." (-> method nnext first first))))
                 (apply list 'fn (-> method nnext first next))]
                [(keyword (gensym (str "_" (first method))))
                 (apply list 'fn (rest method))])})))

(defn normalize-rawmethods
  [methods]
  ;; (println "NORMALIZE-RAWMETHODS")
  (into {} (for [method methods]
             {(if (= 'with-element (first method))
                (keyword (str (name (-> method next first)) "." (-> method nnext first first)))
                (keyword (first method)))
              (if (= 'with-element (first method))
                [(keyword (str (name (-> method next first)) "." (-> method nnext first first)))
                 (apply list 'fn (-> method nnext first next))]
                [(keyword (str (first method)))
                 (apply list 'fn (rest method))])})))

(defn protos->rawbehaviors
  [protos]
  (let [ls (filter (fn [p] (and (symbol? p)
                                (= :polymer-behaviors
                                   (:resource-type
                                    (meta (resolve (interface-sym->protocol-sym p)))))))
                   protos)]
    ;; (println "FILTERED BEHAVIORS: " ls)
    (if (seq ls)
      {:behaviors
       (loop [proto (first protos)
                       tail (rest protos)
                       result {}]
         (if (nil? proto)
           result
           (let [proto (interface-sym->protocol-sym proto)
                 ;; _ (println "BEH META: " (meta (resolve proto)))
                 resource-type (:resource-type (meta (resolve proto)))
                 resource-name (:resource-name (meta (resolve proto)))
                 methods (take-while seq? tail)
                 next-proto (drop-while seq? tail)]
             (recur (first next-proto)
                    (next next-proto)
                    (if (= :polymer-behaviors resource-type)
                      (merge result {resource-name (let [ms (normalize-methods methods)]
                                                     ;; (println "NORMED BEHS: " ms)
                                                     ms)})
                      result)))))})))

(defn protos->rawlisteners
  [protos]
  (let [ls (filter (fn [p] (and (symbol? p)
                                (= :polymer-events
                                   (:resource-type
                                    (meta (resolve (interface-sym->protocol-sym p)))))))
                   protos)]
    ;; (println "FILTERED LISTENERS: " ls)
    (if (seq ls)
      {:listeners
       (loop [proto (first protos)
                       tail (rest protos)
                       result {}]
         (if (nil? proto)
           result
           (let [proto (interface-sym->protocol-sym proto)
                 resource-type (:resource-type (meta (resolve proto)))
                 methods (take-while seq? tail)
                 next-proto (drop-while seq? tail)]
             (recur (first next-proto)
                    (next next-proto)
                    (if (= :polymer-events resource-type)
                      (merge result {proto (let [ms (normalize-methods methods)]
                                             ;; (println "NORMED LISTENERS: " ms)
                                             ms)})
                      result)))))})))

(defn protos->rawmethods
  [protos]
  (let [ls (filter (fn [p] (and (symbol? p)
                                (:co-protocol?
                                 (meta (resolve (interface-sym->protocol-sym p))))))
                   protos)]
    ;; (println "FILTERED METHODS: " ls)
    (if (seq ls)
      (let [ms {:methods
                (loop [proto (first protos)
                       tail (rest protos)
                       result {}]
                  (if (nil? proto)
                    result
                    (let [proto (interface-sym->protocol-sym proto)
                          resource-type (:resource-type (meta (resolve proto)))
                          methods (take-while seq? tail)
                          next-proto (drop-while seq? tail)]
                      (recur (first next-proto)
                             (next next-proto)
                             (if (:co-protocol? (meta (resolve proto)))
                               (merge result {proto (let [ms (normalize-rawmethods methods)]
                                                      ;; (println "NORMED METHS: " ms)
                                                      ms)})
                               result)))))}]
        ;; (println "METHODS X: " ms)
        ms))))

(defn protos->behaviorvec
  [protos]
  ;; (println "PROTOS->BEHAVIORVEC: " protos)
  (let [ls (filter (fn [p] (and (symbol? p)
                                (= :polymer-behaviors
                                   (:resource-type
                                    (meta (resolve (interface-sym->protocol-sym p)))))))
                   protos)]
    ;; (println "FILTERED BEHAVIORS: " ls)
    (if (seq ls)
      {:listeners
       (into []
                     ;;(doall
                     (loop [proto (first protos)
                            tail (rest protos)
                            result ""]
                       (if (nil? proto)
                         result
                         (let [proto (interface-sym->protocol-sym proto)
                               resource-type (:resource-type (meta (resolve proto)))]
                           ;; (println "LISTENER PROTO: " proto)
                           ;; (println "LISTENER TYPE: " resource-type)
                           ;; (println "LISTENER TAIL: " tail)
                           ;; (println "LISTENER RESULT: " result)
                           (let [methods (take-while seq? tail)
                                 next-proto (drop-while seq? tail)]
                             (println "LISTENER METHODS: " methods (type methods))
                             (println "NEXT PROTO: " next-proto)
                             (let [meths (for [method methods]
                                           (let [_ (println "LISTENER METHOD: " method)
                                                 evt (if (= 'with-element (first method))
                                                       (str (name (first (next method))) "."
                                                            (first (first (nnext method))))
                                                       (first method))
                                                 _ (println "LISTENER EVT: " evt)
                                                 handler (str "_"
                                                              (if (= 'with-element (first method))
                                                                (str (name (first (next method))) "_"
                                                                     (first (first (nnext method))))
                                                                (first method)))
                                                 _ (println "LISTENER HANDLER: " handler)
                                                 ]
                                             (str "'" evt "' : '" handler "'")))]
                               (println "LISTENER METHS: " (doall meths))
                               (recur (first next-proto)
                                      (next next-proto)
                                      (if (= :polymer-behaviors resource-type)
                                        (concat result meths)
                                        result))))))))
           })))

(defn construct-behaviors-js
  [protocols]
  ;; (println "CONSTRUCT-BEHAVIORS-JS: " protocols)
  (let [protos (filter (fn [p] (if (not (seq? p))
                                 (and
                                  (not (seq? p))
                                  (= :polymer-behaviors
                                     (:resource-type
                                      (meta (resolve (interface-sym->protocol-sym p))))))))
                       protocols)]
    (if (seq protos)
      (str "behaviors: [\n\t    "
           (str/join ",\n\t    "
                     (for [proto protos #_(filter (fn [p]
                                           (if (not (seq? p))
                                             (and
                                              (not (seq? p))
                                              (= :polymer-behaviors
                                                 (:resource-type
                                                  (meta (resolve (interface-sym->protocol-sym p))))))))
                                         protos)]
                       (do
                         (println "P META: " proto
                                  (meta (resolve (interface-sym->protocol-sym proto))))

                         (str (:resource-name (meta (resolve
                                                     (interface-sym->protocol-sym proto))))))))
           "\n\t  ]"))))

;; clojurescript stuff, from david nolen's tutorial
;; https://github.com/swannodette/hello-cljsc

;; FIXME:  migrate the cljs compile stuff to another namespace
;; ;; A simple helper to emit ClojureScript compiled to JavaScript
;; ;; as a string.
;; #_(defn emit-str [ast]
;;   (with-out-str (c/emit ast)))

;; ;; A simple helper which allows us to read ClojureScript source from a string
;; ;; instead of having to bother with files.
;; (defn string-reader [s]
;;   (clojure.lang.LineNumberingPushbackReader. (java.io.StringReader. s)))

;; ;; A simple helper that takes a stream and returns a lazy sequences of
;; ;; read forms.
;; (defn forms-seq [stream]
;;   (let [rdr (readers/indexing-push-back-reader stream 1)
;;         forms-seq* (fn forms-seq* []
;;                       (lazy-seq
;;                         (if-let [form (reader/read rdr nil nil)]
;;                           (cons form (forms-seq*)))))]
;;     (forms-seq*)))

;; ;; First we need to setup a basic analyzer environment.
;; (def user-env '{:ns {:name cljs.user} :locals {}})

;; (defn read1 [str]
;;   (first (forms-seq (string-reader str))))

;; (read1 "[1 :a 3]")
;; (let [form (read1 "[:a :b]")]
;;   (with-out-str (c/emit (ana/analyze user-env form))))

;; (def cenv (atom {}))

;; (let [form (read1 "[:a :b]")]
;;   (binding [ana/*cljs-static-fns* true]
;;     (emit-str
;;       (env/with-compiler-env cenv
;;         (ana/analyze user-env form)))))

;; (let [form (read1 "js# [:a :b])")]
;;   (cc/optimize {:optimizations :simple}
;;     (emit-str (ana/analyze user-env form))))

;; (defn cljs-compile
;;   [method]
;;   (println "CLJS-COMPILE: " method (type method))
;;   (let [pgm (cc/optimize {:optimizations :simple
;;                           :pretty-print true}
;;                          (emit-str (ana/analyze user-env method)))]
;;     ;(println "CLJS PGM: " pgm)
;;     pgm))

;; (defn cljs-compile-str
;;   [method]
;;   (println "CLJS-COMPILE-STR: " method (type method))
;;   (let [form (read1 method)]
;;     (cc/optimize {:optimizations :advanced
;;                   :pretty-print true}
;;                  (emit-str (ana/analyze user-env form)))))

(defn construct-listeners-js
  [protos]
  (println "CONSTRUCT-LISTENERS-JS: " protos)
  (let [ls (filter (fn [p] (and (symbol? p)
                                (= :polymer-events
                                   (:resource-type
                                    (meta (resolve (interface-sym->protocol-sym p)))))))
                   protos)]
    (println "FILTERED LISTENERS: " ls)
    (if (seq ls)
      (str "listeners: {\n\t    "
           (str/join ",\n\t    "
                     ;;(doall
                     (loop [proto (first protos)
                            tail (rest protos)
                            result ""]
                       (if (nil? proto)
                         result
                         (let [proto (interface-sym->protocol-sym proto)
                               resource-type (:resource-type (meta (resolve proto)))]
                           (println "LISTENER PROTO: " proto)
                           (println "LISTENER TYPE: " resource-type)
                           (println "LISTENER TAIL: " tail)
                           (println "LISTENER RESULT: " result)
                           (let [methods (take-while seq? tail)
                                 next-proto (drop-while seq? tail)]
                             (println "LISTENER METHODS: " methods (type methods))
                             (println "NEXT PROTO: " next-proto)
                             (let [meths (for [method methods]
                                           (let [_ (println "LISTENER METHOD: " method)
                                                 evt (if (= 'with-element (first method))
                                                       (str (name (first (next method))) "."
                                                            (first (first (nnext method))))
                                                       (first method))
                                                 _ (println "LISTENER EVT: " evt)
                                                 handler (str "_"
                                                              (if (= 'with-element (first method))
                                                                (str (name (first (next method))) "_"
                                                                     (first (first (nnext method))))
                                                                (first method)))
                                                 _ (println "LISTENER HANDLER: " handler)
                                                 ]
                                             (str "'" evt "' : '" handler "'")))]
                               (println "LISTENER METHS: " (doall meths))
                               (recur (first next-proto)
                                      (next next-proto)
                                      (if (= :polymer-events resource-type)
                                        (concat result meths)
                                        result))))))))
           "\n\t  }"))))

#_(defn construct-defns-js
  [protos]
  (println "CONSTRUCT-DEFNS: " protos)
  (if (seq protos)
  (str/join ",\n\t  "
            ;;(doall
            (loop [proto (interface-sym->protocol-sym (first protos))
                   tail (rest protos)
                   result ""]
              (if (nil? proto)
                result
                (let [proto (interface-sym->protocol-sym proto)
                      resource-type (:resource-type (meta (resolve proto)))]
                  (println "DEFN PROTO: " proto)
                  (println "DEFN TAIL: " tail)
                  (println "DEFN RESULT: " result)
                  (let [methods (take-while seq? tail)
                        next-proto (drop-while seq? tail)]
                    (println "DEFN METHODS: " methods (type methods))
                    (println "NEXT PROTO: " next-proto)
                    (let [meths (for [method methods]
                                  (let [_ (println "DEFN METHOD1: " method)
                                        cljs-var (str
                                                      (if (= 'with-element (first method))
                                                        (str (name (first (next method))) "_"
                                                             (first (first (nnext method))))
                                                        (first method)))
                                        _ (println "cljs-var: " cljs-var)
                                        elt-id  (if (= 'with-element (first method))
                                                  (first (next method)) nil)
                                        _ (println "elt-id: " elt-id)
                                        fn-type (if elt-id
                                                  (if (vector? (first (next (first (nnext method)))))
                                                    :js :cljs)
                                                  (if (vector? (first (next method))) :js :cljs))
                                        _ (println "fn-type: " fn-type)
                                        args (if elt-id
                                               (first (next (first (nnext method))))
                                               (first (rest method)))
                                        _ (println "args: " args)
                                        raw-form (if elt-id
                                                   (if (= :cljs fn-type)
                                                     (first (next (first (nnext method))))
                                                     (rest (next (first (nnext method)))))
                                                   (rest (rest method)))
                                        _ (println "RAW FORM: " raw-form (type raw-form))
                                        form (if (= :cljs fn-type)
                                               (cljs-compile raw-form)
                                               (str "function("
                                                    (str/join ", " args)
                                                    ") { " (apply str raw-form) "\n\t\t}"))
                                        _ (println "FORM: " form)
                                        fn-name (if (= :polymer-events resource-type)
                                                  (str "_" cljs-var) (str cljs-var))]
                                    (do (println "DEFN METHOD: " cljs-var ": " form)
                                        (str fn-name
                                             ": "
                                             ;; HACK!  GHASTLY HACK!
                                             (if (= 'fn (first raw-form))
                                               (subs form
                                                     1 (- (.length form) 2))
                                               (str form))))))]
                      (println "DEFN METHS: " (doall meths))
                      (recur (first next-proto)
                             (rest next-proto)
                             (concat result meths))))))))))

#_(defn js-constructor
  [nm props & protos]
  (println "JS-CONSTRUCTOR: " (str nm) " PROPS: " props " PROTOS: " protos (seq protos))
  (let [is-str (str "is: '" nm "'")
        props-str (construct-properties-js props)
        behaviors-str (apply construct-behaviors-js protos)
        listeners-str (apply construct-listeners-js protos)
        defns-str (apply construct-defns-js protos)
        ctor-str (str "\n(function () {\n\t 'use strict';\n"
                  "\n\tPolymer({\n\t  "
                      (str/join ",\n\t  "
                                (remove nil? [is-str
                                 (if props-str props-str)
                                 (if behaviors-str behaviors-str)
                                 (if listeners-str listeners-str)
                                 (if defns-str defns-str)]))
                      "\n\t});\n})();\n\t")]
    ;; (println "PROPS: " props-str)
    (element :script ctor-str)))

;; from clojure/core_deftype.clj
(defn- parse-opts [s]
  ;; (println "parse-opts: " s)
  (loop [opts {} [k v & rs :as s] s]
    (if (keyword? k)
      (recur (assoc opts k v) rs)
      [opts s])))

(defn- parse-impls [specs]
  ;; (println "PARSE-IMPLS: " specs)
  (loop [ret {} s specs]
    (if (seq s)
      (recur (assoc ret (first s) (take-while seq? (next s)))
             (drop-while seq? (next s)))
      ret)))

(defn ^{:private true}
  maybe-destructured
  [params body]
  (println "maybe-destructured: " params body)
  (if (every? symbol? params)
    (cons params body)
    (loop [params params
           new-params (with-meta [] (meta params))
           lets []]
      (if params
        (if (symbol? (first params))
          (recur (next params) (conj new-params (first params)) lets)
          (let [gparam (gensym "p__")]
            (recur (next params) (conj new-params gparam)
                   (-> lets (conj (first params)) (conj gparam)))))
        `(~new-params
          (let ~lets
            ~@body))))))

(defn coprotocol?
  [maybe-p]
  (:co-protocol? (meta (resolve maybe-p))))

(defn protocol?
  [maybe-p]
  (boolean (:on-interface maybe-p)))

(defn- ->protomap [opts+specs]
  "emit elements for protos"
  (println "->protomap: " opts+specs)
  (let [[opts specs] (parse-opts opts+specs)
        _ (println "OPTS: " opts)
        _ (println "SPECS: " specs)
        impls (parse-impls (first specs))
        _ (println "IMPLS: " impls (count impls))
        sigs (into {} (map (fn [arg]
                             (let [_ (println "PSYM: " (first arg))
                                   psym (interface-sym->protocol-sym (first arg))
                                   psym-var (resolve psym)]
                               (if (nil? psym-var)
                                 (if (not= 'This (first arg))
                                   (throw (Exception. (str "Symbol " psym " unresolvable")))))
                               [psym (if (= 'This (first arg))
                                       '()
                                       (:sigs (deref psym-var)))]))
                           impls))
        _ (println "SIGS: " sigs)
        ;; we need URIs for behaviors
        uris (into {} (map (fn [arg]
                             (let [psym (first arg)]
                               (println "PROTO: " psym)
                               (println "PROTO var: " (resolve psym))
                               (println "meta PROTO var: " (meta (resolve psym)))
                               (println "PROTO resource-type: " (:resource-type (meta (resolve psym))))
                               (if (= :polymer-behaviors
                                      (:resource-type (meta (resolve psym))))
                                 [psym (:uri (meta (resolve psym)))])))
                           sigs))
        _ (println "URIs: " uris)
        interfaces (-> (map #(interface-sym->protocol-sym %) (keys impls))
                       set
                       (disj 'Object 'java.lang.Object)
                       vec)
        _ (println "INTERFACES: " interfaces)
        _ (doseq [intf interfaces] (println "coprotocol? " intf (coprotocol? intf)))
        ;; methods (map (fn [[name params & body]]
        ;;                (cons name (maybe-destructured params body)))
        ;;              (apply concat (vals impls)))
        ;; _ (println "METHODS: " methods)
        ]
    (when-let [bad-opts (seq (remove #{:no-print :load-ns} (keys opts)))]
      (throw (IllegalArgumentException. (apply print-str "Unsupported option(s) -" bad-opts))))
    ;; (validate-impls impls)
    (doseq [[k v] impls]
      (let [proto (interface-sym->protocol-sym k)
            sig (get sigs proto)]
        (println "PARSING PROTOCOL: " sig)
        (doseq [method-impl v]
          (println "interface sym: " k)
          (println "method-impl: " method-impl)
          (let [method-kw (if (= 'with-element (first method-impl))
                            (keyword (first (first (nnext method-impl))))
                            (keyword (first method-impl)))
                _ (println "method-kw: " method-kw)
                method-sig (get sig method-kw)
                method-impl (if (= 'with-element (first method-impl))
                              (next (first (nnext method-impl)))
                              method-impl)]
            (println "method-impl: " method-impl)
            (println "method-sig: " method-sig)
            (if (nil? method-sig)
              (if (not= 'This k)
                (throw (Exception. (str "Method '" (first method-impl) "' "
                                        "not declared in protocol '" proto "'"))))
              ;; if arity not correct throw bad arity exception
              ;; if fnext is fn, then fnext of next should be arg vector
              ;;FIXME impl-arity
              (let [impl-arity 1
                    proto-arities (set (->> (:arglists method-sig)
                                           (map count)))]
                (println "PROT-ARITIES: " proto-arities)
                #_(if (not-any? proto-arities [impl-arity])
                    (throw (Exception. (str "Bad arity: " method-impl " v. " method-sig))))))))))
    (for [[proto uri] uris]
      (if uri
        (element :link {:rel "import" :href uri})
        (element :link {:rel "import" :href (str proto)})))))

(defn- behaviors->elements [opts+specs]
  "emit elements for protos"
  ;; (println "behaviors->elements: " opts+specs)
  (let [[opts specs] (parse-opts opts+specs)
        ;; _ (println "OPTS: " opts)
        ;; _ (println "SPECS: " specs)
        impls (parse-impls (first specs))
        ;; _ (println "IMPLS: " impls (count impls))
        ;; obtain sig defns by resolving the protocol symbol
        sigs (into {} (map (fn [arg]
                             (let [;;_ (println "PSYM: " (first arg))
                                   psym (interface-sym->protocol-sym (first arg))
                                   psym-var (resolve psym)]
                               (if (nil? psym-var)
                                 (if (not= 'This (first arg))
                                   (throw (Exception. (str "Symbol " psym " unresolvable")))))
                               [psym (if (= 'This (first arg))
                                       '()
                                       (:sigs (deref psym-var)))]))
                           impls))
        ;; _ (println "SIGS: " sigs)
        ;; we need URIs for behaviors
        uris (into {} (map (fn [arg]
                             (let [psym (first arg)]
                               ;; (println "PROTO: " psym)
                               ;; (println "PROTO var: " (resolve psym))
                               ;; (println "meta PROTO var: " (meta (resolve psym)))
                               ;; (println "PROTO resource-type: " (:resource-type (meta (resolve psym))))
                               (if (= :polymer-behaviors
                                      (:resource-type (meta (resolve psym))))
                                 [psym (:uri (meta (resolve psym)))])))
                           sigs))
        ;; _ (println "URIs: " uris)
        ;; interfaces (-> (map #(interface-sym->protocol-sym %) (keys impls))
        ;;                set
        ;;                (disj 'Object 'java.lang.Object)
        ;;                vec)
        ;; _ (println "INTERFACES: " interfaces)
        ;; _ (doseq [intf interfaces] (println "coprotocol? " intf (coprotocol? intf)))
        ;; methods (map (fn [[name params & body]]
        ;;                (cons name (maybe-destructured params body)))
        ;;              (apply concat (vals impls)))
        ;; _ (println "METHODS: " methods)
        ]
    (when-let [bad-opts (seq (remove #{:no-print :load-ns} (keys opts)))]
      (throw (IllegalArgumentException. (apply print-str "Unsupported option(s) -" bad-opts))))
    ;; (validate-impls impls)
    ;; (doseq [[k v] impls]
    ;;   (let [proto (interface-sym->protocol-sym k)
    ;;         sig (get sigs proto)]
    ;;     (println "PARSING PROTOCOL: " sig)
    ;;     (doseq [method-impl v]
    ;;       (println "interface sym: " k)
    ;;       (println "method-impl: " method-impl)
    ;;       (let [method-kw (if (= 'with-element (first method-impl))
    ;;                         (keyword (first (first (nnext method-impl))))
    ;;                         (keyword (first method-impl)))
    ;;             _ (println "method-kw: " method-kw)
    ;;             method-sig (get sig method-kw)
    ;;             method-impl (if (= 'with-element (first method-impl))
    ;;                           (next (first (nnext method-impl)))
    ;;                           method-impl)]
    ;;         (println "method-impl: " method-impl)
    ;;         (println "method-sig: " method-sig)
    ;;         (if (nil? method-sig)
    ;;           (if (not= 'This k)
    ;;             (throw (Exception. (str "Method '" (first method-impl) "' "
    ;;                                     "not declared in protocol '" proto "'"))))
    ;;           ;; if arity not correct throw bad arity exception
    ;;           ;; if fnext is fn, then fnext of next should be arg vector
    ;;           ;;FIXME impl-arity
    ;;           (let [impl-arity 1
    ;;                 proto-arities (set (->> (:arglists method-sig)
    ;;                                        (map count)))]
    ;;             (println "PROT-ARITIES: " proto-arities)
    ;;             #_(if (not-any? proto-arities [impl-arity])
    ;;                 (throw (Exception. (str "Bad arity: " method-impl " v. " method-sig))))))))))
    (for [[proto uri] uris]
      (if uri
        (element :link {:rel "import" :href uri})
        (element :link {:rel "import" :href (str proto)})))))
    ;; [interfaces methods opts]))

;; (defn get-proto-codefsX
;;   "proto names are actually js prototype names; polymer behaviors
;;   lookup table is predefined in polymer.behaviors, user-defined behaviors must be configured in a namespace matching the proto name, e.g. MyBehaviors.HighlightBehavior is configured in MyBehaviors.clj"
;;   [proto+mmaps]
;;   (println "GET-PROTO-CODEFS: " proto+mmaps)
;;   ;; (let [[interfaces methods opts] (behaviors->elements protos)]  ;;opts+specs)]
;;   ;;   (println "INTERFACES: " interfaces)
;;   ;;   (println "METHODS: " methods)
;;   ;;   (println "OPTS: " opts)
;;   (let [result (for [[proto mmap] (partition 2 proto+mmaps)]
;;                  (let [_ (println "proto: " proto (var? proto) (class proto))
;;                        proto-var (if (var? (try (resolve proto)
;;                                                 (catch Exception e
;;                                                   (throw (Exception.
;;                                                           (str "Interface object for " proto " not found"))))))
;;                                    (resolve proto)
;;                                    (let [p (interface-sym->protocol-sym proto)]
;;                                      (if (var? (resolve p))
;;                                        (resolve p)
;;                                        (throw (Exception. "FOO")))))
;;                        _ (println "proto-var: " proto-var (var? proto-var) (protocol? proto-var))
;;                        proto (deref proto-var)]
;;                    (println "PROTOCOL: " proto-var (protocol? proto))
;;                    (when-not (protocol? proto)
;;                      (throw (IllegalArgumentException.
;;                              (str proto " is not a protocol"))))
;;                    (println "FOOBARBAASDFF")
;;                    (str proto)))]
;;     (doall result)
;;     (println "RESULT: ")
;;     result))

(defn get-proto-codefs
  "proto names are actually js prototype names; polymer behaviors
  lookup table is predefined in polymer.behaviors, user-defined behaviors must be configured in a namespace matching the proto name, e.g. MyBehaviors.HighlightBehavior is configured in MyBehaviors.clj"
  ;; NB: a co-def is a ref to def code, e.g. clojure :require clauses are co-defs
  ;; here, <link> refs to impl code are polymer behavior (protocol) co-defs
  ;; 1. verify protocols exist
  ;; 2. validate method impl sigs
  ;; 3. generate <link> markups
  [opts+specs]
  (println "GET-PROTO-CODEFS: " opts+specs)
  (let [links (behaviors->elements opts+specs)]
    ;;[interfaces methods opts] (behaviors->elements opts+specs)]
    ;; (println "INTERFACES: " interfaces)
    ;; (println "METHODS: " methods)
    ;; (println "OPTS: " opts)
    links))
      ;;   (element :link {:rel "import"
      ;;                   :href uri}))))))

(defn- parse-cotype-args
  [args]
  ;; (println "PARSE COTYPE ARGS: " (pr-str args))
  (let [[docstr args] (if (string? (first args))
                        [(first args) (rest args)]
                        ["" args])
        codom (let [cd (filter (fn [x] (if (symbol? x)
                                         (:codom (meta (resolve x))))) args)]
                (if cd
                  cd ;; else get the (codom ...) form
                  ))
        _ (if (> (count codom) 1) (throw (IllegalArgumentException. (str "Only one codom arg allowed"))))
        props (let [props (filter (fn [x] (if (symbol? x)
                                            (:properties (meta (resolve x))))) args)]
                (if props
                  props
                  ))
        ;; _ (println "PROPERTIES: " props)
        ;; exclude docstring and codom
        protos (filter (fn [x] (if (or (list? x)
                                       (and (symbol? x)
                                            (not (:codom (meta (resolve x))))))
                                 x)) args)
        ;; behaviors
        ]
    [docstr args codom protos]))

(defn- props->propmap
  [args]
  ;; (println "PROPS->PROPMAP: " args)
  (let [props (filter (fn [x] (and (symbol? x)
                                       (:properties (meta (resolve x))))) args)]
    ;; (println "PROPS: " props)
    (if props
      (let [properties (into {} (for [prop props]
                                  (do ;; (println "PROP: " prop)
                                      (let [pvar (resolve prop)
                                            ;; _ (println "PVAR: " pvar)
                                            ps (:props (deref pvar))
                                            ;; _ (println "PS: " ps)
                                            ]
                                        ps))))
            html-attrs (into {} (for [prop props]
                                  (do ;; (println "HTMLATTRS PROP: " prop)
                                      (let [pvar (resolve prop)
                                            ;; _ (println "PVAR: " pvar)
                                            html-attrs (:html-attrs (deref pvar))
                                            ;; _ (println "HTMLATTRS: " html-attrs)
                                            ]
                                        html-attrs))))]
        {:properties properties :html-attrs html-attrs}))))

(defn props->cljs
  [propmap]
  ;; (println "PROPS->CLJS: " propmap)
  ;; (if (not (:props propmap))
  ;;   (throw (IllegalArgumentException. (str "props->cljs arg must be a Properties map"))))
  (let [props (:properties propmap)
        prop-keys (keys props)
        html-attrs (:html-attrs propmap)]
    ;; (println (str (:on propmap) ": " prop-keys))
    {:hostAttributes html-attrs
     :properties (into {}
                       (for [prop-key prop-keys]
                         (let [prop (get props prop-key)
                               ;; _ (println "PROP: " prop)
                               descriptors (keys prop)
                               type+val (merge {:type (cljtype->jstype (:type prop))}
                                               (if (= 'String (:type prop))
                                                 (if (nil? (:value prop))
                                                   {}
                                                   (if (empty? (:value prop))
                                                     {:value "\"\""}
                                                     {:value (:value prop)}))
                                                 (if (not (nil? (:value prop)))
                                                   {:value (:value prop)}
                                                   {})))
                               flags (into {} (for [flag (:flags prop)]
                                                [(cljkey->jskey flag) true]))
                               ;; _ (println "FLAGS: " flags)
                               ]
                           ;; (println "procesing property: " (pr-str prop))
                           ;; (println "descriptors: " descriptors)
                           {(keyword (:name prop)) (merge type+val flags
                                                          (if (:observer prop)
                                                            {:observer (first (:observer prop))}))})))}))

(defn listeners->cljs
  [ls]
  ;; (println "LISTENERS->CLJS: " (:listeners ls))
  {:listeners
   (into {}
         (merge
          (concat
           (flatten (for [[evsuite listeners] (:listeners ls)]
                      (do ;;(println "EVENT SUITE: " evsuite)
                        (for [[ev listener] listeners]
                          (do ;;(println "EV/LISTENER: " ev "/" listener)
                            {ev (keyword (first listener))}
                            ))))))))
         })

(defn methods->cljs
  [ms]
  ;; (println "METHODS->CLJS: " ms)
  (let [observers (map #(:observer %)
                       (filter #(:observer %) (vals (:properties ms))))
        ;; _ (println "OBSERVERS: " observers)
        ls (vals (reduce merge (vals (:listeners ms))))
        ;; _ (println "LISTENER METHODS: " ls)
        behs (vals (reduce merge (vals (:behaviors ms))))
        ;; _ (println "BEH METHODS: " behs)
        this-methods (vals (reduce merge (vals (:methods ms))))
        ;; _ (println "THIS METHODS: " this-methods)
        methods (concat observers ls behs this-methods)
        ;; _ (println "METHODS: " methods)
        ]
    (into {} methods)))

  ;; {:methods
  ;;  (into {}
  ;;        (merge
  ;;         (concat
  ;;          (flatten (for [[evsuite listeners] (:listeners ms)]
  ;;                     (do ;;(println "EVENT SUITE: " evsuite)
  ;;                       (for [[ev listener] listeners]
  ;;                         (do ;;(println "EV/LISTENER: " ev "/" listener)
  ;;                           {ev (keyword (first listener))}
  ;;                           ))))))))
  ;;        })

(defn behaviors->cljs
  [behs]
  ;; (println "BEHAVIORS->CLJS: " (:behaviors behs))
  (let [behaviors (keys  (:behaviors behs))]
    {:behaviors (into [] (map keyword behaviors))}))

(defn component->prototype
  [cvar rawprops rawlisteners rawbehaviors rawmethods]
  ;; (println "COMPONENT->PROTOTYPE: " cvar (type cvar))
  ;; (println "RAWLISTENERS: " rawlisteners)
  ;; (println "RAWMETHODS: " rawmethods)
  ;; (println "RAWBEHAVIORS: " rawbehaviors)
  (let [;; v (resolve nm)
        ;; _ (println "component var: " cvar)
        ;; _ (println "props: " props)
        ;; namesp (:ns (meta cvar))
        ;; ns-name (ns-name namesp)
        uri (str "tmp/" (var->path cvar) ".cljs")
        cljs-ns (var->cljs-ns cvar)
        propmap (props->cljs rawprops)
        listeners (listeners->cljs rawlisteners)
        methmap (methods->cljs (merge rawprops rawlisteners rawbehaviors rawmethods))
        ;; _ (println "METHODS MAP: " methmap)
        behaviors (behaviors->cljs rawbehaviors)
        cljs (str/join "\n" [(pprint-str (list 'ns cljs-ns))
                             ;; (pprint-str '(enable-console-print!))
                             (pprint-str (list 'js/Polymer
                                               (list 'clj->js
                                                     (merge {:is (keyword (:name (meta cvar)))}
                                                            propmap
                                                            listeners
                                                            methmap
                                                            behaviors))))])]
    ;; (println "CLJS:\n" cljs)
    ;; (println "URI: " uri)
    ;; (io/make-parents uri)
    ;; (spit uri cljs)
    (alter-meta! cvar
               (fn [old new]
                 (merge old new))
               {:miraj {:prototype cljs}})))

(defmacro defcomponent
  [nm & args]
  ;; (println "COMPONENT: " (str nm)) ;; " ARGS: " args)
  (if (not (str/includes? nm "-")) (throw (IllegalArgumentException.
                                           (str "Component name must contain at least one dash '-'."))))
  ;; (println "COMPONENT ARGS: " (pr-str args))
  (let [[docstr arglist codom & protos] (parse-cotype-args args)
        ;; _ (println "DOCSTR: " (pr-str docstr))
        ;; _ (println "ARGLIST: " (pr-str arglist))
        ;; _ (println "COTYPE PROTOS: " protos (seq protos))
        ;; _ (println "CODOM: " codom)
        rawprops (props->propmap args)

        rawmethods (apply protos->rawmethods protos)

        rawbehaviors (apply protos->rawbehaviors protos)

        rawlisteners (apply protos->rawlisteners protos)

        ;; codom (drop 1 cod)
        behavior-elts (behaviors->elements protos)
        ;; _ (println (str "BEHAVIOR-ELTS: " (pr-str behavior-elts)))
        ;; protomap (->protomap protos)
        ;; _ (println "PROTOMAP: " protomap)

        ;; js-ctor (js-constructor nm arglist protos)
;;        js-ctor (element :script (str *ns* "." nm "()"))
        ]
    `(let [tree# ~@codom
       ;; html-constructor interns the name, binding it to ->html fn
           cvar# (html-constructor ~*ns* '~nm (keyword '~nm) (str (ns->uri ~*ns*) "/" '~nm))
           _# (apply component->prototype cvar# '~rawprops '~rawlisteners '~rawbehaviors '~rawmethods)
           ;; _# (println "PROTOTYPE: " (:prototype (:miraj (meta cvar#))))
             ;; (if (instance? miraj.markup.Element ~(first codom))
             ;;         ~@codom
             ;;         (codom ~nm ~@codom))
             content# (:content tree#)
             result# (update tree#
                            :content (fn [c#]
                                       (let [dom# (last c#)
                                             ;; _# (println "DOM#: " dom#)
                                             newdom# (update dom#
                                                             :attrs
                                                             (fn [old-attrs#]
                                                               (assoc old-attrs# :id (str '~nm))))
                                             ;; newdom# (update newdom#
                                             ;;                 :content (fn [domc#]
                                             ;;                            (concat domc# [~js-ctor])))
                                             ]
                                         ;; (println "NEWDOM#: " newdom#)
                                         (concat (butlast c#)
                                                 [~@behavior-elts]
                                                 #_[dom#]
                                                 [newdom#]
                                                 ))))
             ;; var# (find-var (symbol (str ~*ns*) (str '~nm)))
           ]
         ;; (println "ALTERING META FOR " cvar#)
         (alter-meta! cvar#
                      (fn [old# new#]
                        (do ;;(println "old#: " old#)
                           ;;(println "new#: " new#)
                            (merge old#
                                   {:miraj (merge (:miraj old#)
                                                  new#)})))
                      {:component true
                       :codom result#})
         ;; (println "ALTERED META FOR " cvar#)
         cvar#)))

;; from clojure/core_deftype.clj
;; reduce is defined again later after InternalReduce loads
(defn ^:private ^:static
  reduce1
       ([f coll]
             (let [s (seq coll)]
               (if s
         (reduce1 f (first s) (next s))
                 (f))))
       ([f val coll]
          (let [s (seq coll)]
            (if s
              (if (chunked-seq? s)
                (recur f
                       (.reduce (chunk-first s) f val)
                       (chunk-next s))
                (recur f (f val (first s)) (next s)))
         val))))

(defn- assert-same-protocol [protocol-var method-syms]
  (doseq [m method-syms]
    (let [v (resolve m)
          p (:protocol (meta v))]
      (when (and v (bound? v) (not= protocol-var p))
        (binding [*out* *err*]
          (println "Warning: protocol" protocol-var "is overwriting"
                   (if p
                     (str "method " (.sym v) " of protocol " (.sym p))
                     (str "function " (.sym v)))))))))

(def property-types
  {'Vector ^{:doc " (i.e. satisfy vector?)"} (fn [x] (vector? x))
   'Map ^{:doc " (i.e. satisfy map?)"} (fn [x] (map? x))
   'Boolean ^{:doc "  Allowed values are 'true' and 'false'."} (fn [x] (or (= 'true x) (= 'false x)))
   'Date ^{:doc "[year month? day? hour? minute? second? millisecond?] (see clj-time) "} (fn [x]
           (println "DATE ARG: " x (type x))
           (and (vector? x)
                (<= (count x) 7)
                (not (empty? x))
                (every? number? x)))
   'Number number?
   'String string?})

(defn- emit-properties [name opts+sigs]
  (let [iname (symbol (str (munge (namespace-munge *ns*)) "." (munge name)))
        [opts sigs]
        (loop [opts {:on (list 'quote iname)} sigs opts+sigs]
          (condp #(%1 %2) (first sigs)
            string? (recur (assoc opts :doc (first sigs)) (next sigs))
            keyword? (recur (assoc opts (first sigs) (second sigs)) (nnext sigs))
            [opts sigs]))
        ;; _ (println "PRESIGS: " sigs)
        html-attrs (first (filter map? sigs))
        ;; _ (println "HTMLATTRS: " html-attrs)
        sigs (filter list? sigs)
        ;; _ (println "LIST PRESIGS: " sigs)
        all-props (->> (map first (filter #(:tag (meta (first %))) sigs)))
        ;; _ (println "all-props: " all-props)
        sigs (when sigs
               (reduce1 (fn [m s]
                          ;; (println "REDUCE1 M: " m)
                          ;; (println "REDUCE1 S: " s)
                          (let [name-meta (meta (first s))
                                ;; _ (println "name-meta: " name-meta)
                                name-meta (dissoc (assoc name-meta :type (:tag name-meta))
                                                              :tag)
                                ;; _ (println "name-meta: " name-meta)
                                doc (:doc name-meta)
                                prop-type (if (some (set [(:type name-meta)])
                                                    (set (keys property-types)))
                                              (:type name-meta)
                                              'Observer)
                                ;; _ (println "prop-type: " prop-type)

                                name-meta (if (= prop-type 'Observer)
                                            (assoc name-meta :type 'Observer)
                                            name-meta)
                                ;; _ (println "name-meta: " name-meta)
                                mname (with-meta (first s) nil)
                                ;; _ (println "mname: " mname)

                                _ (let [strs (filter string? s)]
                                    ;; (println "STRINGS: " strs)
                                    (if (= prop-type 'String)
                                      (if (> (count strs) 1)
                                              (throw (IllegalArgumentException.
                                                      (str "Too many String args for property: " mname))))
                                      (if (not= prop-type 'Observer)
                                        (if (> (count strs) 0)
                                          (throw (IllegalArgumentException.
                                                  (str "Illegal String arg for property: " mname)))))))

                                default-val (let [v (if (= :computed (first (rest s)))
                                                      (if (fn? (eval (first (nnext s))))
                                                        (do ;;(println "COMPUTED " (first (nnext s)))
                                                            (first (nnext s)))
                                                        (throw (IllegalArgumentException.
                                                                (str "Computed value for "
                                                                     mname
                                                                     " must be a fn."))))
                                                      (if (= prop-type 'Observer)
                                                        nil
                                                        (first (rest s))))]
                                              #_(println "V: " v (fn? (eval v)))
                                              (if (list? v) ;; a fn?
                                                (if (= (first v) 'fn)
                                                  (first (nnext v))
                                                  v)
                                                #_(throw (Exception. (str "don't understand " v))))
                                              (if (= prop-type 'Date)
                                                v
                                                #_(.toString (apply t/date-time v))
                                                v))
                                ;; _ (println "default-val: " default-val (type default-val))
                                _ (if (not= prop-type 'Observer)
                                    (if (not (nil? default-val))
                                      (let [pred (get property-types prop-type)]
                                        ;; (println "pred: " pred)
                                        (if (fn? (eval default-val))
                                          (if (not (= :computed (first (rest s))))
                                            (throw (IllegalArgumentException. (str "Default value "
                                                                                   (pr-str default-val)
                                                                                   " for "
                                                                                   mname
                                                                                   " must match type "
                                                                                   prop-type
                                                                                   ". "
                                                                                   (:doc (meta pred))))))
                                          (if (not (apply pred [default-val]))
                                            (throw (IllegalArgumentException. (str "Default value "
                                                                                   (pr-str default-val)
                                                                                   " for "
                                                                                   mname
                                                                                   " must match type "
                                                                                   prop-type
                                                                                   ". "
                                                                                   (:doc (meta pred))))))))))
                                flags (filter keyword? s)
                                ;; _ (println "flags: " flags)
                                _  (if (some #{:computed} flags)
                                     (if (not (= :computed (nth s 1)))
                                       (throw (Exception. (str "Flag :computed must be first arg: "
                                                               mname)))))
                                observer (let [obs (if (some #{:computed} flags)
                                                     (do ;;(println "COMPUTED HIT")
                                                         (filter list? (next (nnext s))))
                                                     (if (= prop-type 'Observer)
                                                       (list (conj (rest s) 'fn))
                                                       (filter list? s)))]
                                           obs)
                                ;; doc (let [ss (filter string? s)]
                                ;;       (if (= 2 (count ss))
                                ;;         (last ss)
                                ;;         (if (= (first (next s)) (first ss)) nil (first ss))))
                                ;; _ (println "observer: " observer) ; (-> (first observer) next first))
                                ;; _ (println "doc: " doc)
                                ]
                            (when (> (count observer) 1)
                              (throw (IllegalArgumentException. (str "Only one observer allowed for property " mname))))
                            (if (not (empty? observer))
                              (if (not= prop-type 'Observer)
                                (let [argcount (count (-> (first observer) next first))]
                                  (when (not= 2 argcount)
                                    (throw (IllegalArgumentException.
                                            (str "Definition of observer function for property "
                                                 mname
                                                 " must take exactly two args, for new and old vals, in that order.")))))))

                            (cond
                              (= prop-type 'Observer)
                              (if (not (empty? flags))
                                (throw (IllegalArgumentException. (str "Flags "
                                                                       (pr-str flags)
                                                                       " not allowed on multi-prop observer: " mname)))
                                (let [args (fnext (first observer))]
                                  ;; (println "OBSERVER ARGS: " args)
                                  (if (empty? args)
                                    (throw (IllegalArgumentException. (str "Argument vector for multi-prop observer " mname " must not be empty."))))
                                  (if (not (every? (set all-props) args))
                                    (throw (IllegalArgumentException. (str "Argument vector for multi-prop observer " mname " must contain property names:" args)))))))

                            (when (m (keyword mname))
                              (throw (IllegalArgumentException. (str "Function " mname " in protocol " name " was redefined. Specify all arities in single definition."))))


                            (assoc m (keyword mname)
                                   (merge name-meta
                                          {:value default-val}
                                           {:flags flags}
                                           ;; :raw s
                                          {:name (vary-meta mname assoc :doc doc :observer observer)}
                                           (if (not (empty? observer))
                                             {:observer [(keyword (gensym (str "_" mname)))
                                                              (first observer)]})
                                           (if (not (nil? doc))
                                           {:doc doc})))))
                        {} sigs)) ;; end reduce1

        ;; _ (println "SIGS: " sigs)
        ;; meths (mapcat (fn [sig]
        ;;                 (let [m (munge (:name sig))]
        ;;                   (map #(vector m (vec (repeat (dec (count %))'Object)) 'Object)
        ;;                        (:observer sig))))
        ;;               (vals sigs))
        ;; _ (println "METHS: " meths)
        ]
    ;; (println "DEFPROPS A")
  `(do
     (defonce ~name {})
     ;; (gen-interface :name ~iname :methods ~meths)
     (alter-meta! (var ~name) assoc :doc ~(:doc opts) :properties true)
     ~(when sigs
        `(#'assert-same-protocol (var ~name) '~(map :name (vals sigs))))
     ;; (println "DEFPROPS VAR: " (var ~name))
     ;; (println "DEFPROPS OPTS: " '~opts)
     ;; (println "DEFPROPS SIGS: " (str '~sigs))
     (alter-var-root (var ~name)
                     merge
                     (assoc ~opts
                            :html-attrs '~html-attrs
                            :props '~sigs
                            :var (var ~name)
                       ;; :method-map
                       ;;   ~(and (:on opts)
                       ;;         (apply hash-map
                       ;;                (mapcat
                       ;;                 (fn [s]
                       ;;                   [(keyword (:name s)) (keyword (or (:on s) (:name s)))])
                       ;;                 (vals sigs))))
                       ;; :method-builders
                       ;;  ~(apply hash-map
                       ;;          (mapcat
                       ;;           (fn [s]
                       ;;             [`(intern *ns* (with-meta '~(:name s) (merge '~s {:protocol (var ~name)})))
                       ;;              #_(emit-method-builder (:on-interface opts) (:name s) (:on s) (:observer s))])
                       ;;           (vals sigs)))
                        ))
    ;; (println "DEFPROPS X")
    ;;  (-reset-methods ~name)
    ;; (println "DEFPROPS " '~name)
     '~name)))

(defmacro defproperties
  [name & opts+sigs]
  ;; (println "DEFPROPERTIES: " name opts+sigs)
  (try (emit-properties name opts+sigs)
       (catch Exception e
         (throw (IllegalArgumentException. (str "defproperties " name ": " (.getMessage e)))))))

#_(defmacro compile-prototype
  [o]
  ;; [props listeners behaviors methods]
  (println "COMPILE-CLJS")
  (let [v (resolve o)
        nm (:name (meta v))
        namesp (:ns (meta v))
        ns-name (ns-name namesp)
        uri (str "tmp/" (var->path v))
        cljs (str/join "\n" [(pprint-str (list 'ns (symbol (str ns-name "." nm))))
                             ;; (pprint-str '(enable-console-print!))
                             (pprint-str (list 'js/Polymer (eval `(props->cljs ~o))))])]
    (println "CLJS: " cljs)
    (println "URI: " (io/as-file uri))
    (io/make-parents uri)
    (spit (io/as-file uri) cljs)))

;;    (cljs-compile-str (str cljs))))

;; s (if (= (first mode) :pprint)
;;             (do (println "pprint")
;;                 (with-out-str (pprint doc)))
;;             (serialize doc))]
;;     (spit file s)))

         ;; (str/join ",\n" (for [prop-key prop-keys]
         ;;                   (let [prop (get props prop-key)
         ;;                         descriptors (keys prop)
         ;;                         typeval (remove empty? [(str "type: " (:type prop))
         ;;                                                 (if (= 'String (:type prop))
         ;;                                                   (if (nil? (:value prop))
         ;;                                                     '()
         ;;                                                     (if (empty? (:value prop))
         ;;                                                       (str "value: \"\"")
         ;;                                                       (str "value: \"" (:value prop) "\"")))
         ;;                                                   (if (not (nil? (:value prop)))
         ;;                                                     (str "value: " (:value prop))
         ;;                                                     '()))])
         ;;                         flags (for [flag (:flags prop)]
         ;;                                 (str (cljkey->jskey flag) ": true"))]
         ;;                     (println "procesing property: " (pr-str prop))
         ;;                     (println "descriptors: " descriptors)
         ;;                     (str (name (:name prop)) ": {\n\t"
         ;;                          (str/join ",\n\t"
         ;;                                    (concat typeval
         ;;                                            flags))
         ;;                          "\n}"))))


(defmacro makepolymer
  [nm docstr & args]
  (println "makepolymer: " nm docstr args))

(defmacro defpolymer
  [nm & args]
  (println "DEFPOLYMER: " nm args)
  (let [docstr (if (string? (first args)) (first args) "")
        args (if (string? (first args)) (rest args) args)]
    (println "defpolymer: " nm docstr args)))

;; we need co-syntax for co-application (observation)
;; Alternatives:
;; (observe my-foo)
;; (my-foo :observe)
;; (<<! my-foo)
(defmacro <<!
  [elt]
  ;; (println "OBSERVING: " elt (symbol? elt))
  (if (symbol? elt)
    `(let [e# ~(resolve elt)
           ;; _# (println "OBSERVING2: " e# (type e#))
           ;; _# (println "META: "  (str (meta e#)))
           result# (:codom (:miraj (meta e#)))]
       ;; (println "CO-CTOR: " result#)
       #_(serialize result#)
       result#)
    `(let [e# ~elt
           ;; _# (println "OBSERVING3: " e# (type e#))
           ;; _# (println "META: "  (str (meta e#)))
           result# (:co-ctor (:miraj (meta e#)))]
       ;; (println "CO-CTOR: " result#)
       #_(serialize result#)
       result#)))

;;(println "loaded miraj.markup")
