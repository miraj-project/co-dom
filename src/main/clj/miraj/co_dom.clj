;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "derived from clojure.data.xml"
      :author "Gregg Reynolds, Chris Houser"}
  miraj.co-dom
  (:refer-clojure :exclude [import require])
  (:require [clojure.spec :as spec]
            [clojure.string :as str]
            [clojure.data.json :as json]
            ;; [cheshire.core :as json :refer :all]
            [clj-time.core :as t]
            [clojure.pprint :as pp]
            [clojure.java.io :as io]
            ;; [clojure.tools.reader :as reader]
            ;; [clojure.tools.reader.reader-types :as readers]
            ;; [cljs.analyzer :as ana]
            ;; [cljs.compiler :as c]
            ;; [cljs.closure :as cc]
            ;; [cljs.env :as env]
            [clojure.tools.logging :as log :only [trace debug error info]]
            )
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
           ;; [miraj NSException]))
;;           [java.util Date]))

(println "loading miraj.co-dom")
;;FIXME:  support comment nodes

(defonce mode (atom nil))
(defonce verify? (atom false))

(defonce miraj-boolean-tag "__MIRAJ_BOOLEAN_5HgyKgZoQSuzPt9U")
(defonce miraj-pseudo-kw :__MIRAJ_PSEUDO_sfmWqa5HptMJ6ATR)

(defn pprint-str [m]
  (let [w (StringWriter.)] (pp/pprint m w)(.toString w)))


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
  #{:alternate
    :author
    :bookmark
    :help
    :icon
    :import ;; an extension, but needed for HTML5 imports
    :license
    :next
    :no-follow
    :no-referrer
    :prefetch
    :prev
    :search
    :stylesheet
    :tag
    })

(def html5-link-type-extentions
  ;; http://microformats.org/wiki/existing-rel-values#HTML5_link_type_extensions
  #{:archived :dns-prefetch :external :first ;; etc.
    :index :last :ping-back :preconnect :preload :prerender :sidebar :up})

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

(def html5-script-attrs
  ;; https://www.w3.org/TR/html5/scripting-1.html#the-script-element
  ;; https://developer.mozilla.org/en-US/docs/Web/HTML/Element/script
  {:async :bool
   :charset :_
   :crossorigin :_
   :defer :_
   :integrity :string  ;; mozilla
   :src :uri
   :type :_})

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
    ;; (log/debug (format "write-attributes %s" attrs))
    (let [[attr-ns nm] (qualified-name k)
          attr-name (if (= :html @mode)
                      (validate-html5-attr-name nm v)
                      (str nm))

          ;; FIXME: only do polymer annotations in HTML mode
          attr-val ;;(if (= :html @mode)
                     (cond
                       (= :rel k) (if (keyword? v) (subs (str v) 1) v)
                       ;; (if (contains? html5-link-types
                       ;;                           (if (string? v) (keyword v) v))
                       ;;              (if (keyword? v) (subs (str v) 1) v)
                       ;;              (throw (Exception.
                       ;;                      (str "Invalid link type value for rel attribute: {"
                       ;;                           k " " v "}; valid values are: "
                       ;;                           html5-link-types))))

                       (keyword? v)
                       (do ;;(println "KEYWORD")
                         (if (nil? (namespace v))
                           (str "{{" (get-two-way-token v) "}}")
                           ;;FIXME
                           (str "{{" (subs (str v) 1) "}}")))

                       (symbol? v) (str "[[" (str v) "]]")

                       ;;(nil? v) miraj-boolean-tag

                       (map? v) (let [json-str (json/write-str v)]
                                  (str/replace json-str #"\"" "_Q_4329750"))

                       :else (str v))
                     ;;(str v))
                     ]
      (if attr-ns
        (try (.writeAttribute writer attr-ns attr-name attr-val)
             (catch javax.xml.stream.XMLStreamException e
              (log/fatal (format "EXCEPTION writing attr: %s=%s\nMsg: %s"
                                                   k v
                                                   ;; attr-ns attr-name attr-val
                                                   (.getMessage e)))
              (throw e))
             (catch Exception e
              (log/fatal (format "EXCEPTION writing attr: %s=%s Msg: %s"
                                                   k v
                                                   ;; attr-ns attr-name attr-val
                                                   (.getMessage e)))
              (throw e)))
        (.writeAttribute writer attr-name attr-val)))))

(declare serialize serialize-impl)

; Represents a node of an XML tree
(defrecord Element [tag attrs content]
  java.lang.Object
  (toString [x]
    (do ;; (print "Element toString: " x "\n")
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

;; from http://webcomponents.org/polyfills/ :
;; Note: Due to the nature of some of the polyfills, to maximize
;; compatibility with other libraries, make sure that webcomponents.js is
;; the first script tag in your document's <head>.

(declare element parse-str)

(def js-optimizer "miraj/co_dom/optimize-js.xsl")

(defn xsl-xform
  [ss elts]
  ;; (println "xsl-xform ss: " ss)
  ;; (println "xsl-xform doc: " elts)
  (let [ml (do
             (if (not (instance? miraj.co_dom.Element elts)) ;
               (do ;;(println (type elts))
                   (throw (Exception. "xsl-xform only works on miraj.co-dom.Element"))))
             (serialize :xml elts))
        ;; _ (println "XF SOURCE: " ml)
        xmlSource (StreamSource.  (StringReader. ml))
        xmlOutput (StreamResult. (StringWriter.))
        factory (TransformerFactory/newInstance)
        transformer (let [r (io/resource ss)
                          xsl (slurp r)]
                      (.newTransformer factory (StreamSource. (StringReader. xsl))))
        ;;(.newTransformer factory (StreamSource. (StringReader. ss)))
        ]
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
  ;; (log/info "PPRINT-IMPL: " elts)
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
             (throw (Exception. "xml pprint only works on miraj.co-dom.Element"))
             (if (> (count s) 3)
               (do ;;(println "pprint-impl FOREST")
                   (let [s (serialize :xml (element :CODOM_56477342333109 s))]
                     (reset! mode fmt)
                     s))
               (let [s (serialize :xml s)]
                 (reset! mode fmt)
                 s)))
        ;; _ (log/info "XML SERIALIZED: " ml)
        xmlSource (StreamSource.  (StringReader. ml))
        xmlOutput (StreamResult.
                   (let [sw (StringWriter.)]
                     (if (.startsWith ml "<!doctype")
                       (.write sw "<!doctype html>\n"))
                     sw))
        factory (TransformerFactory/newInstance)

        ;; _ (log/debug (format "XSL-ID-X %s" xsl-identity-transform-html))
        transformer (if (= :html @mode)
                      (let [r (io/resource "miraj/co_dom/identity-html.xsl")
                            xsl (slurp r)]
                        ;; (StringReader. xsl-identity-transform-html))))
                        (.newTransformer factory (StreamSource. (StringReader. xsl))))
                      (let [r (io/resource "miraj/co_dom/identity-xml.xsl")
                            xsl (slurp r)]
                        ;;(log/trace "transforming with xsl-identity-transform-xml")
                        (.newTransformer factory (StreamSource. (StringReader. xsl)))))]
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
                     regx (re-pattern (str "=\"" miraj-boolean-tag "\""))
                     ;;regx (re-pattern (str miraj-boolean-tag "="))
                     ]
                 ;; boolean attribs: value must be ""
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
             (throw (Exception. "xml pprint only works on miraj.co-dom.Element"))
             (if (> (count s) 1)
               (throw (Exception. "forest input not yet supported for serialize"))
               (let [s (serialize :xml s)]
                 (reset! mode fmt)
                 s)))
        ;; _ (log/debug (format "SERIALIZED %s" ml))
        xmlSource (StreamSource.  (StringReader. ml))
        xmlOutput (StreamResult.
                   (let [sw (StringWriter.)]
                     (if (.startsWith ml "<!doctype")
                       (.write sw "<!doctype html>"))
                     sw))
        factory (TransformerFactory/newInstance)
        transformer (if (= :html @mode)
                      (let [r (io/resource "miraj/co_dom/identity-html.xsl")
                            xsl (slurp r)]
                        ;; (StringReader. xsl-identity-transform-html))))
                        (.newTransformer factory (StreamSource. (StringReader. xsl))))
                      (let [r (io/resource "miraj/co_dom/identity-xml.xsl")
                            xsl (slurp r)]
                        ;;(log/trace "transforming with xsl-identity-transform-xml")
                        (.newTransformer factory (StreamSource. (StringReader. xsl)))))

        ;; transformer (if (= :html @mode)
        ;;               (do
        ;;                 ;;(println "transforming with xsl-identity-transform-html")
        ;;               (.newTransformer factory (StreamSource. (StringReader. xsl-identity-transform-html))))
        ;;               (do
        ;;                 ;;(log/trace "transforming with xsl-identity-transform-xml")
        ;;               (.newTransformer factory (StreamSource. (StringReader. xsl-identity-transform-xml)))))]
        ]
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
                        regx (re-pattern (str "=\"" miraj-boolean-tag "\""))
                        ;;regx (re-pattern (str miraj-boolean-tag "="))
                        ]
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
                    (do ;(log/trace "emitting HTML:" args)
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

;; test
#_(defn body
  ""
  [page-var & args]
  (println "BODY: " page-var)
  (println "body args: " args)
  (let [ns (-> page-var meta :ns ns-name)
        _ (clojure.core/require ns :reload-all :verbose)
        content (map #(eval %) args)]
        ;;bod (x/element :body content)]
    ;; (println ":BODY " content)
    [:body content]))

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
  ;; (log/info "EMIT-EVENT: " event)
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
    (throw (Exception. (format "emit-event: no matching clause for event %s " event)))))

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
    (do ;(log/debug "NEXT evt: " (:tag element))
        ;(log/debug "NEXT element:" element)
        ;(log/debug "NEXT items:" next-items)
        ;(log/debug "NEXT content:" (:content element))
        ;(if (= (:tag element) :link)
         ; next-items
        (let [result (cons (:content element)
                           (cons (Event. :end-element (:tag element) nil nil) next-items))]
          ;(log/debug "NEXT-EVENTS RESULT:" result)
          result)))

  Event
  (gen-event [event] event)
  (next-events [_ next-items]
    next-items)

  clojure.lang.PersistentArrayMap
  (gen-event [coll]
    (println "GEN-EVENT PAM: " coll))
  (next-events [coll next-items])

  ;; literal content of script elt:
  clojure.lang.PersistentList
  (gen-event [l]
    ;; (log/debug "GEN-EVENT PERSISTENT LIST: " l)
    (Event. :chars nil nil (str/join "\n" l)))
    ;; (Event. :cdata nil nil l))
  (next-events [_ next-items]
    ;; (log/debug "NEXT-EVENTS PERSISTENT LIST: " next-items)
    next-items)

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
  ;; (log/debug "FLATTEN-elements:" elements)
  ;; (prn elements)
  (when (seq elements)
    (lazy-seq
     (let [e (first elements)]
       ;; (log/debug "FLATten:" (seq e))
       (let [f (gen-event e)]
         ;; (log/debug "GENNED EVT:" f)
         (cons f
               (flatten-elements (next-events e (rest elements)))))))))

(declare parse-elt-args)

(def pseudo-attrs
  #{:$active :$checked :$disabled :$enabled :$focus :$hover :$indeterminate :$lang :$link :$target :$visited
    :$root :$nth-child :$nth-last-child :$nth-of-type :$nth-last-of-type :$first-child :$last-child :$first-of-type :$last-of-type :$only-child :$only-of-type :$empty
    :$after :$before :$first-line :$first-letter})

(defn attr-map?
  [m]
  ;; NB: if this file is reloaded (e.g. during dev) then (instance?
  ;; Element m) will fail for uses of Element that have not also been
  ;; reloaded.  So we compare classnames instead of types.
  (and (map? m)
       ;; (not (= miraj.co_dom.Element (type m)))))
       (not (= (.getCanonicalName (class m))
               (.getCanonicalName miraj.co_dom.Element)))))

(defn co-dom-node?
  [m]
  ;; NB: if this file is reloaded (e.g. during dev) then (instance?
  ;; Element m) will fail for uses of Element that have not also been
  ;; reloaded.  So we compare classnames instead of types.
  (= (.getCanonicalName (class m))
     (.getCanonicalName miraj.co_dom.Element)))

(defn- special-kw?
  [tok]
  (and (keyword? tok)
       (nil? (namespace tok))
       (let [tokstr (name tok)]
         (or (.startsWith tokstr "#")
             (.startsWith tokstr ".")
             (.startsWith tokstr "!")))))

(defn- validate-kw-attrib
  [tag attr]
  ;; id, class, and boolean attr: must come first, and be chained
  ;; e.g.  :#foo.bar.baz?centered
  ;; (span ::foo) => <span id="foo"></span>
  ;; (span ::foo.bar) => <span id="foo" class="bar"></span>
  ;; (span ::.foo.bar) => <span class="foo bar"></span>
  ;; (span :bool/foo) => <span foo></span>
  ;; (log/info "VALIDATE-KW-ATTRIB: " attr)
  (if (nil? (namespace attr)) ;; (span :foo) => <span>[[foo]]</span>
    (let [token (name attr)]
      (if (or (.startsWith token "#") (.startsWith token ".") (.startsWith token "!"))
        (let [tokens (str/split
                      (str/trim (str/replace token #"(#|\.|\!)" " $1")) #" ")
              ;; _ (log/debug (format "TOKENS %s => %s" attr tokens))

              id-tokens (filter (fn [t] (str/starts-with? t "#")) tokens)
              ;; _ (log/debug (format "ID TOKS %s" (seq id-tokens)))
              id-tokens (map (fn [t] (subs t 1)) id-tokens)
              id-attribs (if (empty? id-tokens) {} {:id (str/join " " id-tokens)})
              ;; _ (log/debug (format "ID ATTRIBS %s" id-attribs))

              class-tokens (filter (fn [t] (str/starts-with? t ".")) tokens)
              ;; _ (log/debug (format "CLASS TOKENS %s" (seq class-tokens)))
              class-tokens (map (fn [t] (subs t 1)) class-tokens)
              class-attribs (if (empty? class-tokens)
                              {} {:class (str/join " " class-tokens)})
              ;; _ (log/debug (format "CLASS ATTRIBS %s" class-attribs))

              boolean-tokens (filter (fn [t] (str/starts-with? t "!")) tokens)
              boolean-tokens (map (fn [t] (subs t 1)) boolean-tokens)
              ;; _ (log/debug (format "BOOLEAN TOKENS %s" (seq boolean-tokens)))
              boolean-attribs (if (empty? boolean-tokens)
                                {} (into {} (for [t boolean-tokens]
                                              {(keyword t) (str miraj-boolean-tag)})))
              ;; _ (log/debug (format "BOOLEAN ATTRIBS %s" boolean-attribs))

              attribs (merge id-attribs class-attribs boolean-attribs)
              result [attribs]
              ]
          attribs)))))
    ;;       ;; (log/debug (format "RESULT ATTRIBS %s" attribs))
    ;;       ;; (let [is-id (.startsWith token "#")
    ;;       ;;       is-class (.startsWith token ".")
    ;;       ;;       is-boolean (.startsWith token ""!")
    ;;       ;;       classes (filter identity (str/split token #"\."))
    ;;       ;;       attr (first classes)
    ;;       ;;       ;; (doall classes)
    ;;       ;;       ;; _ (println "CLASSES: " classes attr)
    ;;       ;;       ;; _ (println "REST CLASSES: " (rest classes))
    ;;       ;;       ;; result (if is-class
    ;;       ;;       ;;          [{:class (str/trim (str/join " " classes))} content]
    ;;       ;;       ;;          (if (seq (rest classes))
    ;;       ;;       ;;            [{:id attr :class (str/trim (str/join " " (rest classes)))} content]
    ;;       ;;       ;;            [{:id attr} content]))]
    ;;       ;;       ]
    ;;       ;; (log/debug "RESULT CONTENT: " content)
    ;;       #_(if (map? (first (last result)))
    ;;         (if (instance? miraj.co_dom.Element (first (last result)))
    ;;           (do ;; (log/debug (format "ELT" ))
    ;;               result)
    ;;           (do ;; (log/debug (format "MAPP" ))
    ;;               (parse-elt-args tag
    ;;                (merge (first result) (first (last result))) (rest (last result)))))
    ;;         (do ;; (log/debug (format "NONMAP" ))
    ;;             result)))
    ;;     ;; else not a special kw
    ;;     ;;[{} (list attr content)]
    ;;     attribs
    ;;     ))
    ;; ;; else namespace not nil - disallow?
    ;; ))

(defn get-specials-map
  [tag specials]
  (let [sm (apply merge-with (fn [a b] (str/join " " [a b]))
                  (for [special specials] (validate-kw-attrib tag special)))]
    ;; (log/debug (format "SPECIALS MAP %s" sm))
    sm))

(defn- get-pseudo-styles
  [tag attrs uuid]
  (let [style (for [[nm val] attrs]
                (let [selector (str  "." uuid ":" (subs (name nm) 1)) ;;(name tag)
                      style (str/join "" (for [[k v] val]
                                           (str (name k) ":"
                                                (if (= k :content) (str "'" v "'") v)
                                                ";")))]
                  (str selector "{"style"}")))]
    ;; (log/debug (format "GET-PSEUDO %s" (seq style)))
    (str/join " " style)))

(defn- attr-sym?
  [arg]
  (if (list? arg)
    (let [fst (first arg)]
      ;; (log/debug (format "First %s %s" fst (type fst) (= 'clojure.core/deref fst)))
      (if (= fst 'clojure.core/deref)
        (do ;; (log/debug (format "deref"))
            true)
        false))))

(defn- normalize-attributes
  [tag attrs content]
  ;; FIXME: support maps as values
  ;; FIXME: handle html5 custom attrs, data-*
  ;; list of attrs:  http://w3c.github.io/html/fullindex.html#attributes-table
  ;; (log/debug (format "NORMALIZE-attributes %s %s" tag attrs))
  (if (instance?  miraj.co_dom.Element attrs)
    (do ;; (log/debug "Element instance")
      [{} (remove empty? (list attrs content))])
    (let [other-attrs (apply hash-map
                             (flatten (filter (fn [[k v]]
                                                ;; (log/debug (format "KV %s %s" k v))
                                                (if (not (or (keyword? k) (attr-sym? k)))
                                                  (throw (Exception.
                                                          (format "Only keyword attr names supported: %s is %s"
                                                                  k (type k)))))
                                                (or (= k miraj-pseudo-kw)
                                                    (and (list? k) (= 'clojure.core/deref
                                                                      (first k)))
                                                    (let [attr-ns (namespace k)
                                                          attr-name (name k)
                                                          c (get attr-name 0)]
                                                      (java.lang.Character/isLetter c))))
                                              ;; FIXME: why did we dissoc :content?
                                              ;; (dissoc attrs :content))))
                                              attrs)))
          ;; _ (log/debug (format "OTHER Attrs  %s" other-attrs))

          pseudo-attrs (apply hash-map
                              (flatten (filter (fn [[k v]] (contains? pseudo-attrs k)) attrs)))
          pseudo-class (if (empty? pseudo-attrs) nil (str "miraj_"
                                                          #_(rand-int 100000)
                                                          (java.util.UUID/randomUUID)))
          pseudo (if (empty? pseudo-attrs)
                   {}
                   {miraj-pseudo-kw (get-pseudo-styles tag pseudo-attrs pseudo-class)})
          ;; _ (log/debug (format "PSEUDO %s" pseudo))

          other-attrs (if (empty? pseudo) other-attrs
                          (update-in other-attrs [:class]
                                     (fn [old] (str old " " pseudo-class))))
          ;; _ (log/debug (format "WITH PSEUDO %s" other-attrs))

          style-attrs (apply hash-map
                              (flatten (filter (fn [[k v]]
                                                 (and (not (contains? pseudo-attrs k))
                                                      (let [attr-ns (namespace k)
                                                            attr-name (name k)
                                                            c (get attr-name 0)]
                                                        (= c \$))))
                                                 attrs)))
          ;; _ (log/debug (format "STYLE attrs %s empty? %s" style-attrs (empty? style-attrs)))

          style (if (empty? style-attrs)
                  (do ;; (log/debug (format "EMPTY" ))
                      {})
                  {:style (str/join ""
                                    (for [[k v] style-attrs] (str (subs (name k) 1) ":" v ";")))})
          ;; _ (log/debug (format "STYLE  %s" style))
          valids (merge style other-attrs pseudo)
          ]
      ;; (log/debug (format "VALIDS %s" valids))
      valids)))

(defn parse-elt-args
  [tag attrs content]
  (log/info "parse-elt-args TAG " tag " ATTRS: " attrs " CONTENT: " content)
  (if (empty? attrs)
    [attrs content]
    (let [special-kws (filter (fn [tok]
                                (and (keyword? tok)
                                     (nil? (namespace tok))
                                     (let [tokstr (name tok)]
                                       (or (.startsWith tokstr "#")
                                           (.startsWith tokstr ".")
                                           (.startsWith tokstr "!")))))
                              content)
          content (filter (fn [tok]
                            (or (not (keyword? tok))
                                (not (nil? (namespace tok)))
                                (let [tokstr (name tok)]
                                  (not (or (.startsWith tokstr "#")
                                           (.startsWith tokstr ".")
                                           (.startsWith tokstr "!"))))))
                          content)]
      ;; (log/debug (format "SPECIAL KWS %s" (seq special-kws)))
      ;; (log/debug (format "CLEANED CONTENT %s" (seq content)))
      (cond
        ;;TODO support boolean, etc. for CDATA elts
        (number? attrs)
        (do ;;(println "number? attrs: " attrs)
          ;; (span 3) => <span>3</span>
          [{} (remove empty? (list (str attrs) content))])

        (symbol? attrs) ;; (span 'foo) => <span>{{foo}}</span>
        [{} (list attrs (remove nil? content))]

        (keyword? attrs) (validate-kw-attrib tag attrs content)

        (map? attrs) (normalize-attributes tag attrs content)

        :else (do ;;(println "NOT map attrs: " attrs)
                [{} (remove empty? (list attrs content))])))))

(defn element
  [tag & args]
  ;; (log/debug "ELEMENT: " tag) ;; " ARGS: " args)
  ;; (log/debug (format "ARGS %s" args))
  (let [;; args (first args)

        special-attrs (filter #(special-kw? %) args)

        ;; _ (log/debug (format "Specials %s" (seq special-attrs)))

        specials-map (get-specials-map tag special-attrs)
        ;; _ (log/debug (format "Specials MAP %s" specials-map))

        attr-map (into {} (filter attr-map? args))
        ;; _ (log/debug (format "ATTR-MAP %s" attr-map))

        content (filter (fn [arg]
                          ;; (log/debug (format "filtering %s" arg))
                          (and (not (special-kw? arg))
                               (not (attr-map? arg))))
                        args)
        ;; _ (log/debug (format "Content %s" (seq content)))

        attr-map (normalize-attributes tag attr-map content)
        ;; _ (log/debug (format "Attr-map %s" attr-map))

        ids (let [c1 (:id attr-map)
                  c2 (:id specials-map)
                  id (str/trim (str/join " " [c1 c2]))]
              ;; (log/debug (format "ID COUNT %s %s" id (count (str/split #" " id))))
              (if (> (count (str/split id #" "))
                     1) (throw (Exception. (format "Only one ID allowed: %s" id))))
              id)
        ;; _ (log/debug (format "IDS: %s" ids))

        classes (let [c1 (:class attr-map)
                      c2 (:class specials-map)]
                  (str/trim (str/join " " [c1 c2])))
        ;; _ (log/debug (format "Classes %s" classes))

        attrs (merge attr-map specials-map)
        attrs (if (empty? classes)
                attrs
                (assoc attrs :class classes))

        ;; _ (log/debug (format "Attrs %s" attrs))


        ;; [attrs content] (if (empty? args) [{} '()]
        ;;                     (let [arg1 (first args)]
        ;;                       (if (instance? miraj.co_dom.Element arg1)
        ;;                         [{} arg1]
        ;;                         (if (map? arg1)
        ;;                           [arg1 (rest args)]
        ;;                           (if (keyword? arg1)
        ;;                             [arg1 (rest args)]
        ;;                             [{} args])))))
        ;; _ (log/debug (format "ELT ATTRS %s" attrs))
        ;; _ (log/debug "ELT CONTENT " content)
        ;; [attribs contents] (parse-elt-args tag (or attrs {}) (or content '()))
        ;; [attribs contents] (do (log/debug (format "FIRST CONTENTS %s" (first contents)))
        ;;                        (if (map? (first contents))
        ;;                          (parse-elt-args tag
        ;;                           (merge attribs (first contents)) (rest contents))
        ;;                          [attribs contents]))
        ;; _ (log/debug "ELEMENT ATTRIBS: " attribs)
        ;; _ (log/debug "Element content: " content)
        e (Element. tag (or attrs {}) (or content '()))]
        ;; e (if (= (type attrs) miraj.co-dom.Element)
        ;;     (Element. tag {} (remove nil? (apply list attrs content)))
        ;;     (if (map? attrs)
        ;;       (Element. tag (or attrs {}) (flatten (remove nil? content)))
        ;;       (Element. tag {} (remove nil? (apply list attrs)))))]
    ;; (log/debug "NODE: " e)
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
  ;; (log/debug "EMIT: " e " OPTS: " opts)
  (let [^javax.xml.stream.XMLStreamWriter
        stream-writer (-> (javax.xml.stream.XMLOutputFactory/newInstance)
                          (.createXMLStreamWriter writer))]
    (when (instance? java.io.OutputStreamWriter writer)
      (check-stream-encoding writer (or (:encoding opts) "UTF-8")))

    ;; (if (:doctype opts)
    ;;   (do (println "DOCTYPE!!!!")
    ;;       (.writeDTD stream-writer "<!doctype html>")))
    (if (:with-xml-declaration opts)
      (.writeStartDocument stream-writer (or (:encoding opts) "UTF-8") "1.0"))
    (let [events (flatten-elements [e])]
      ;; (log/trace "FLATTENED EVENTS:" events)
      (doseq [event events]
        (do ;; (log/trace "event: " event)
            (emit-event event stream-writer writer)))
      ;; (.writeEndDocument stream-writer)
      writer)))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; begining of miraj customization

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
                                       (if (instance? miraj.co-dom.Element first#)
                                         (do ;(log/trace "Element instance")
                                             {})
                                         (do ;(log/trace "NOT Element instance")
                                             first#)))
                                   (do ;(log/trace "NOT map? first")
                                       {}))
                          content# (if (map? first#)
                                     (if (instance? miraj.co-dom.Element first#)
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

#_(defn make-void-elt-fns
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
                      (if (instance? miraj.co_dom.Element (first htags#))
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

#_(defn make-meta-tag-fns
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

#_(defn make-tag-fns
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
                      ;; (println "HTML FN: " ~elt (pr-str parms#))
                      (let [args# (flatten parms#)]
                        ;; (println "HTML FLAT: " ~elt (pr-str (flatten args#)))
                        (if (empty? args#)
                          (element ~elt)
                          (if (symbol? args#) ;; e.g. (h/span 'index)
                            (println "SYMBOL: " args#)
                            (let [first# (first args#)
                                  rest# (rest args#)
                                  [attrs# content#] (parse-elt-args first# rest#)
;                                  _# (println "content: " content#)
                                  ;; f#  (apply element ~elt attrs# content#)
                                  ;; _# (println "F: " f#)
                                  func# (with-meta (apply element ~elt attrs# content#)
                                          {:co-fn true
                                           :elt-kw ~elt
                                           :elt-uri "foo/bar"})]
                              func#)))))
              f (eval func)]))))

(defn html-constructor
  [ns-sym nm-sym elt-kw uri & docstring]
  (println "HTML-CONSTRUCTOR:" ns-sym nm-sym elt-kw uri docstring)
  (let [ds (if (empty? docstring) "" (first docstring))
        newvar (intern ns-sym (with-meta (symbol (str nm-sym)) {:doc ds}) ;; :uri uri :_webcomponent true})
                       (fn [& args]
                         (let [elt (if (empty? args)
                                     (do (println "COMPONENT FN NO ARGS: " elt-kw)
                                         (element elt-kw))
                                     (let [first (first args)
                                           rest (rest args)
                                           [attrs content] (parse-elt-args first rest)]
                                       (apply element elt-kw attrs content)))]
                           elt)))]
    ;; (println "NS-SYM: " ns-sym)
    ;; (println "NM-SYM: " nm-sym)
    ;; (println "VAR: " newvar)
    newvar))
  ;; (alter-meta! (find-var ns-sym nm-sym)
  ;;              (fn [old new]
  ;;                (merge old new))
  ;;              {:miraj/miraj {:co-fn true
  ;;                       :component typ
  ;;                       :doc docstring
  ;;                       :elt-kw elt-kw
  ;;                       :elt-uri elt-uri}}))

(declare <<!)

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

;; (defn get-metas
;;   [metas]
;;   ;; (println "HTML5-METAS " (keys html5-meta-attribs))
;;   (log/debug "GET-META " metas)  ;; HTML5 META ATTRIBS: " hmtl5-meta-attribs)
;;   (let [html-tags (into {} (filter (fn [[k v]] (= "miraj.html" (namespace k))) metas))
;;         _ (log/debug "HTML TAGS: " html-tags)
;;         ms (for [[tag val] html-tags]
;;              (do
;;              ;;(let [rule (get html5-meta-attribs tag)]
;;                (log/debug "META: " tag (pr-str val)) ;; " RULE: " rule)
;;                ;;(if (keyword? rule)
;;                  ;; FIXME: validation
;;                  ;; (let [m (element :meta
;;                  ;;                   {:name (name tag)
;;                  ;;                         #_(subs (str tag) 1)
;;                  ;;                         :content (str val)})]
;;                  ;;   ;; (println "META ELT: " m)
;;                  ;;   m)
;;                  ;; FIXME: hack for pseudo-metas
;;                  (if (= :miraj.html/title tag)
;;                    (element :title val)
;;                    (if (= :miraj.html/base tag)
;;                      (element :base {:href val})))))]
;; ;;                     (apply-meta-rule "" tag val rule)))))]
;;                ;; (case tag
;;                ;;   :apple (let [apple (apply-meta-rule "" tag val rule)]
;;                ;;            #_(log/trace "APPLE: " apple) apple)
;;                ;;   :msapplication (let [ms (apply-meta-rule "msapplication" tag val rule)]
;;                ;;                    #_(log/trace "MSAPP: " ms) ms)
;;                ;;   :mobile (let [ms (apply-meta-rule "" tag val rule)]
;;                ;;             #_(log/trace "MOBILE: " ms) ms)
;;                ;;   (element :meta {:name (subs (str tag) 1)
;;                ;;            :content (str val)}))))]
;;     ;; force eval, for printlns
;;     (doall ms)
;;     ;; (println "METAS: " ms)
;;     ms))

#_(defn platform
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  require/import

;;FIXME: migrate to:
;;
;;      miraj/core/require.clj
;;      miraj/core/import.clj

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
  "Given pfx and sfx (e.g. 'paper' and 'input'), generate the href
  value needed to load polymer component."
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
#_(defmulti get-resource-elt
  "Returns e.g. <link rel='import' href='foo/bar/baz.html'>"
  (fn [typ nsp sym spec]
    ;; (println (str "GET-RESOURCE-elt: " typ " " nsp " " sym))
    typ))

#_(defmethod get-resource-elt :default
  [typ nsp sym spec]
  ;; (println (str "get-resource-elt :default: " typ " " nsp " " sym))
  (throw (Exception. (str "Unrecognized resource type for require: " spec))))
  ;; (element :link
  ;;          {:rel "import" :href (get-href (ns-name nsp) ref)}))

#_(defmethod get-resource-elt :polymer
  [typ nsp sym spec]
  (println "GET-RESOURCE-ELT polymer: NS: " nsp " SYM: " sym)
  (println "spec: " spec)
  (let [pfx (:resource-pfx (meta nsp))
        path (:elt-uri (:miraj/miraj (meta (find-var sym))))
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

#_(defmethod get-resource-elt :link
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;  REQUIRING - moved to miraj core, in miraj/webc.clj

#_(defn require-resource
  "Implements handling of :require clauses in html expressions."
  [spec]
  ;; (println (str "REQUIRE-RESOURCE: " spec))
  ;; at this point, the ns has been required, but not the fns
  ;; tasks:  1. create the fns based on :refer; 2. generate the <link> elts
  (let [ns-sym (first spec)
        ;; _ (println "ns sym: " ns-sym)
        ns-obj (find-ns ns-sym)
        ;; _ (println "ns obj: " ns-obj)
        options (apply hash-map (rest spec))
        as-opt (:as options)
        ;; _ (println ":as " as-opt)
        refer-syms (doall (:refer options))
        ;; _ (println ":refer " refer-syms)
        ]

    (if as-opt
      (do ;; (println "ALIASING 1: " as-opt (type as-opt) ns-sym (type ns-sym))
          (clojure.core/alias as-opt ns-sym)
          #_(println "aliases for " *ns* ": " (ns-aliases *ns*))))
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
      (for [ref refer-syms]
        (let [ref-sym (symbol (str ns-sym) (str ref))
              ;; _ (println "REF: " ref-sym)
              ref-var (if-let [v (find-var ref-sym)]
                        v (throw (Exception. "var not found for config sym: " ref-sym)))
              ref-val (deref ref-var)]
          (if ref-val
            (if-let [uri (:uri (meta ref-var))]
              (do
                ;; FIXME: this verify doesn't work with user-defined components
                ;; (if verify? (verify-resource uri spec))
                (element :link {:rel "import" :href uri}))
              (throw (Exception. (str ":uri key expected in: " ref-var))))
            (throw (Exception. (str "ref-var " ref-var " not bound")))))))))
        ;; #_(if verify? (verify-resource uri spec))
        ;;       (element :link {:rel "import" :href uri}))))))))
;; (get-resource-elt ns-type ns-obj ref-sym comp)))))))

(defn resolve-require-refs
  [spec]
  ;; (println (str "RESOLVE-REQUIRE-REFS: " spec))
  ;; at this point, the ns has been required, but not the fns
  ;; tasks:  1. create the fns based on :refer; 2. generate the <link> elts
  (let [ns-sym (first spec)
        ;; _ (println "ns sym: " ns-sym)
        ns-obj (find-ns ns-sym)
        ;; _ (println "ns obj: " ns-obj)
        options (apply hash-map (rest spec))
        as-opt (:as options)
        ;; _ (println ":as " as-opt)
        refer-syms (doall (:refer options))
        ;; _ (println ":refer " refer-syms)
        ]
    (if as-opt
      (do ;; (println "ALIASING 2: " *ns* ": " as-opt ns-sym)
          (alias as-opt ns-sym)
          #_(println "aliases for " *ns* ": " (ns-aliases *ns*))))
    ;; (doseq [[isym# ivar#] (ns-interns ns-sym)] (println "INTERNED: " isym# ivar#))
    (cond
      (nil? refer-syms)
      nil

      (= :all refer-syms)
      nil ;; iterate over everything in the ns map

      :else
      (doall
       (for [ref refer-syms]
         (let [ref-sym (symbol (str ns-sym) (str ref))
               _ (log/info "REF: " ref)
               _ (log/info "\trefsym: " ref-sym)
               ref-var (find-var ref-sym)
               _ (println "ref-var: " ref-var)
               ;; ;; config map must be named "components"
               ;; ns-map-sym (symbol (str ns-sym) "components")
               ;; _ (println "ns-map-sym: " ns-map-sym)
               ;; ns-map-var (resolve ns-map-sym)
               ;; _ (println "ns-map-var: " ns-map-var)
               ;; ns-map (if ns-map-var
               ;;          (deref ns-map-var)
               ;;          (throw (NSException. (str "Config map '" ns-map-var "' unresolvable"))))
               ;; ;; _ (println "ns-map val: " ns-map)

               ref-val (if-let [v (deref ref-var)]
                         v (throw (Exception. (str "unbound config var: " ref-var))))
               _ (println "ref-val: " ref-val)
               _ (println "evaled: " (ref-val))
               ref-kw (:kw ref-val)
               uri (:uri ref-val)
               ;; _ (println "link href: " uri)
               ]
           (if (nil? ref-kw) (throw (Exception. (str "HTML tag (kw) for '" ref-sym "' not found"))))
           ;; step 2
           (html-constructor ns-sym ref ref-kw uri)
           #_(println "interned: " (ns-resolve ns-sym ref))))))))

;; (require [[polymer.paper :as paper :refer [button card]]])
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

;;FIXME: migrate to miraj/core/import.clj ?

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
                    _ (println "style ref: " style-ref)
                    uri (:uri style-ref)
                    _ (println "uri: " uri)

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
  (println (str "get-import: " import))
  (println (str "ns: " (first import) " " (type (first import))))
  (let [ns (first import)]
    (clojure.core/require ns)
    (let [import-ns (if-let [n (find-ns ns)] n (throw (Exception. (str "import ns not found: " ns))))
          refs (rest import)
          _ (println "REFS: " refs (type refs))
          result (for [ref refs]
                   (do (println "REF: " ref)
                       (let [ref-var (resolve (symbol (str ns) (str ref)))
                             ref-val (deref ref-var)]
                       (import-resource (:type ref-val) import))))]
      (doall result)
      (println "get-import RESULT: " result)
      result)))

(defmacro import
  "Handle :import clauses in html expressions. Used to generate HTML
  'head' elements responsible for loading non-component resources like
  stylesheets, images, etc."
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

;; (defn meta-map
;;   [m]
;;   ;; (println "meta-map: " m)
;;   (get-metas m))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  miraj-specific elements

#_(defmacro codom
  [nm & args]
  (println "CODOM: " (str nm)) ;; " ARGS: " args)
  `(do ;;(println "codom: " ~(str nm) ~@args)
       (let [tree# (apply element :CODOM_56477342333109 {:id ~(str nm)} (list ~@args))]
         (xsl-xform xsl-normalize-codom tree#))))


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
#_(defmacro <<!
  [elt]
  ;; (println "OBSERVING: " elt (symbol? elt))
  (if (symbol? elt)
    `(let [e# ~(resolve elt)
           ;; _# (println "OBSERVING2: " e# (type e#))
           ;; _# (println "META: "  (str (meta e#)))
           result# (:codom (:miraj/miraj (meta e#)))]
       ;; (println "CO-CTOR: " result#)
       #_(serialize result#)
       result#)
    `(let [e# ~elt
           ;; _# (println "OBSERVING3: " e# (type e#))
           ;; _# (println "META: "  (str (meta e#)))
           result# (:co-ctor (:miraj/miraj (meta e#)))]
       ;; (println "CO-CTOR: " result#)
       #_(serialize result#)
       result#)))

(defn js
  [form]
  (log/debug (format "reading JS %s" form))
  #_(str "#js "form)
  (list 'clj->js form))


(println "loaded miraj.co-dom")
