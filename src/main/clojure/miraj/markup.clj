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
  (:require [clojure.string :as str])
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

;; (println "loading miraj/markup.clj")
;;FIXME:  support comment nodes

(defonce mode (atom nil))
(defonce miraj-boolean-tag "__MIRAJ_BOOLEAN_955196")

(def html5-void-elts
  #{"area" "base" "br" "col"
   "embed" "hr" "img" "input"
   "keygen" "link" "meta" "param"
   "source" "track" "wbr"})

(def html5-global-attrs
  "https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes"
  ;;FIXME - handle data-*
  {:access-key :class :content-editable :context-menu :dir :draggable
   :drop-zone :hidden :id :item-id :item-prop :item-ref :item-scope :item-type
   :lang :spellcheck :style :tab-index :title :translate})

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
  (str nm))

(defn write-attributes [attrs ^javax.xml.stream.XMLStreamWriter writer]
  (doseq [[k v] attrs]
    ;; (println "ATTR: " k " = " v " " (type v))
    (let [[attr-ns nm] (qualified-name k)
          attr-name (if (= :html @mode)
                      (validate-html5-attr-name nm v)
                      (str nm))
          attr-val (if (= :html @mode)
                     (cond
                       (= :rel k) (if (contains? html5-link-types
                                                 (if (string? v) (keyword v) v))
                                    (if (keyword? v) (subs (str v) 1) v)
                                    (throw (Exception.
                                            (str "Invalid link type value for rel attribute: {"
                                                 k " " v "}; valid values are: "
                                                 html5-link-types))))
                       (keyword? v) (str "{{" (subs (str v) 1) "}}")
                       (symbol? v) (str "[[" (str v) "]]")
                       ;; (= (subs (str k) 1) (str v)) miraj-boolean-tag
                       ;; (empty? v) miraj-boolean-tag
                       (nil? v) miraj-boolean-tag
                       :else (str v))
                     (str v))]
      (if attr-ns
        (.writeAttribute writer attr-ns attr-name attr-val)
        (.writeAttribute writer attr-name attr-val)))))

(declare serialize)

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
   "<xsl:output method='xml' encoding='UTF-8' indent='yes'/>"

   "<xsl:template match='node()'>"
     "<xsl:copy>"
       "<xsl:apply-templates select='@*|node()'/>"
     "</xsl:copy>"
   "</xsl:template>"

   "<xsl:template match='html'>"
     "<xsl:text disable-output-escaping='yes'>&lt;!doctype html&gt;&#xA;</xsl:text>"
     "&#xA;"
     "<xsl:copy>"
       "<xsl:apply-templates select='@*|node()'/>"
     "</xsl:copy>"
   "</xsl:template>"

   "<xsl:template priority=\"99\" match=\"" (str/join "|" html5-void-elts) "\">"  ;;*[.='']\">"
     "<xsl:copy>"
       "<xsl:apply-templates select='@*|node()'/>"
       "VOID"
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

(def xsl-optimize-js
  (str
   "<xsl:stylesheet version='1.0' xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>"
   "<xsl:strip-space elements='*' />"
   "<xsl:output method='xml' encoding='UTF-8' indent='yes'/>"

   "<xsl:template match='@*|node()'>"
     "<xsl:copy>"
       "<xsl:apply-templates select='@*|node()'/>"
     "</xsl:copy>"
   "</xsl:template>"

   "<xsl:template match='script'/>"
   "<xsl:template match='script' mode='optimize'>"
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

(declare parse-str)

(defn xsl-xform
  [ss elts]
  ;; (println "xsl-xform ss: " ss)
  ;; (println "xsl-xform doc: " elts)
  (let [ml (do
             (if (not (instance? miraj.markup.Element elts))
               (do (println (type elts))
                   (throw (Exception. "xml-xform only works on clojure.data.xml.Element"))))
             (serialize :xml elts))
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
    ;; (.toString (.getWriter xmlOutput))))

;;FIXME: support non-tree input
;;FIXME: support :xhtml option
(defn pprint-impl
  [& elts]
  ;; (log/trace "pprint-impl: " elts)
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
             (if (> (count s) 1)
               (throw (Exception. "forest input not yet supported for pprint"))
               (let [s (serialize :xml s)]
                 (reset! mode fmt)
                 s)))
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
               ;(str/replace (.toString (.getWriter xmlOutput)) #"VOID<[^>]+>" "")
               (let [string-writer (.getWriter xmlOutput)
                     s (.toString string-writer)
                     void (.flush string-writer)
                     s (str/replace s #"VOID<[^>]+>" "")
                     s (str/replace s #"_EMPTY_333109" "")
                     regx (re-pattern (str "=\"" miraj-boolean-tag "\""))]
                 ;; boolean attribs: value must be "" or must match attrib name
                 ;;FIXME: make this more robust
                 (str/replace s regx ""))
               (.toString (.getWriter xmlOutput))))))

(defn pprint
  [& elts]
  ;; (log/trace "PPRINT elts: " elts)
  (if (keyword? (first elts))
    (do ;(log/trace "fnext elts: " (fnext elts))
        (if (nil? (fnext elts))
          nil
          (apply pprint-impl elts)))
    (if (nil? (first elts))
      nil
      (apply pprint-impl elts))))

(defn emit-start-tag [event ^javax.xml.stream.XMLStreamWriter writer]
  ;; (println "emit-start-tag: " (:name event))
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
                        "VOID"))
                    "</" t ">"))))

(defn str-empty? [s]
  (or (nil? s)
      (= s "")))

(defn emit-cdata [^String cdata-str ^javax.xml.stream.XMLStreamWriter writer]
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
            ;; (if (.endsWith nm "%")
            ;;   (str "{{" ns (if ns ".") (subs nm 0 (- (count nm) 1))  "}}")
              (str "[[" (namespace kw) (if (namespace kw) ".") (name kw) "]]"))))
  (next-events [_ next-items]
    next-items)

  clojure.lang.Symbol
  (gen-event [sym]
    (let [nm (name sym)
          ns (namespace sym)]
      ;; (log/trace "gen-event Symbol: " sym)
      (Event. :sym nil nil
              (str "{{" ns (if ns ".") nm "}}"))))
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

(defn element [tag & [attrs & content]]
  ;;(println "ELEMENT: " tag attrs content)
  (let [e (if (= (type attrs) miraj.markup.Element)
            (Element. tag {} (remove nil? (apply list attrs content)))
            (if (map? attrs)
              (Element. tag (or attrs {}) (flatten (remove nil? content)))
              (Element. tag {} (remove nil? (apply list attrs)))))]
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
  (let [fac (javax.xml.stream.XMLInputFactory/newInstance)]
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
        sreader (.createXMLStreamReader fac s)]
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

(defn emit-str
  "Emits the Element to String and returns it.
   Options:
    :encoding <str>          Character encoding to use
    :with-xml-declaration <bool>, default false"
  [e & opts]
  (let [^java.io.StringWriter sw (java.io.StringWriter.)]
    (apply emit e sw opts)
    (.toString sw)))

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
    (cond
      (= @mode :html)
      (do ;(log/trace "emitting HTML: " args)
        (if (= :html (:tag (first args)))
          (.write string-writer "<!doctype html>"))
        (emit args string-writer :html true :with-xml-declaration false))

      (= @mode :xml)
      (do ;(log/trace "emiting XML")
          (if (= :with-xml-declaration (first args))
            (do ;(log/trace "emitting with xml decl: " args)
                (emit (rest args) string-writer :with-xml-declaration true))
            (do ;(log/trace "emitting w/o xml decl: " args)
                (emit args string-writer :with-xml-declaration false))))
      :else
      (throw (Exception. "invalid mode: " @mode)))
    (str (if (= @mode :html)
           (let [s (str/replace (.toString string-writer) #"VOID<[^>]+>" "")
                 regx (re-pattern (str "=\"" miraj-boolean-tag "\""))]
                 (str/replace s regx ""))
           (.toString string-writer)))))
;; (.toString string-writer)))

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

(defn make-meta-tag-fn-x
  [tag+validator]
  ;; (log/trace "make-tag-fn " tag+validator)
    (let [fn-tag (symbol (subs (str (first tag+validator)) 1))
          fn-validator (fnext tag+validator)
          elt (keyword fn-tag)
          ;; log (println "make-meta-tag-fn fn-tag: " fn-tag)
          ;; log (println "make-meta-tag-fn elt-kw: " elt)
          func `(defn ~fn-tag ;; (symbol (str tag))
                  [& fn-args#]
                  ;; (println "POLYMER FN: " ~elt (pr-str fn-args#))
                  (if (empty? fn-args#)
                    (element ~elt)
                    (let [first# (first fn-args#)
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
                                       fn-args#
                                       (rest fn-args#))
                                     fn-args#)
                          func# (apply element ~elt attrs# content#)]
                      ;; (log/trace "fn-args: " fn-args#)
                      ;; (log/trace "elt: " ~elt)
                      ;; (log/trace "tags: " attrs#)
                      ;; (log/trace "content: " content# " (" (type content#) ")")
                      ;; (log/trace "func: " func# (type func#))
                      func#)))]
      ;;(println (macroexpand func))))
      (eval func)))

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
            ;; #_(println (macroexpand func))
            ;; (eval func))))))

(defn make-tag-fns
  [pfx tags sfx]
  ;; (println "make-tag-fns " pfx tags sfx)
  (doseq [tag tags]
    (do ;(println "make-tag-fn " tag)
        (let [fn-tag (cond
                     (string? tag) (symbol tag)
                     (vector? tag) (symbol (last tag)))
              elt (keyword (str pfx (cond
                                      (string? tag) tag
                                      (vector? tag) (last tag))))
              ;; log (println "make-tag-fns fn-tag: " fn-tag " (" (type fn-tag) ")")
              func `(defn ~fn-tag ;; (symbol (str tag))
                      [& htags#]
                      ;; (println "POLYMER FN: " ~elt (pr-str htags#))
                      (if (empty? htags#)
                        (element ~elt)
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
                              func# (with-meta (apply element ~elt attrs# content#)
                                      {:co-fn true
                                       :elt-kw ~elt
                                       :elt-uri "foo/bar"})]
                          ;; (log/trace "htags: " htags#)
                          ;; (log/trace "elt: " ~elt)
                          ;; (log/trace "tags: " attrs#)
                          ;; (log/trace "content: " content# " (" (type content#) ")")
                          ;; (log/trace "func: " func# (type func#))
                          func#)))
              f (eval func)]))))

;;FIXME rename this to make-component-fns
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
                                        :co-type ~typ
                                        :doc ~docstring
                                        :elt-kw ~elt-kw
                                        :elt-uri ~elt-uri}})

                                    (let [attrib-args# (first args#)
                                          attrs# (if (map? attrib-args#)
                                                   (do ;(log/trace "map? first")
                                                     (if (instance? miraj.markup.Element attrib-args#)
                                                       (do ;(log/trace "Element instance")
                                                         {})
                                                       (do ;(log/trace "NOT Element instance")
                                                         attrib-args#)))
                                                   (do ;(log/trace "NOT map? attrib-args#")
                                                     {}))
                                          content# (if (map? attrib-args#)
                                                     (if (instance? miraj.markup.Element attrib-args#)
                                                       args#
                                                       (rest args#))
                                                     args#)]
                                      (with-meta (apply element ~elt-kw attrs# content#)
                                        {:miraj {:co-fn true
                                                 :co-type ~typ
                                                 :doc ~docstring
                                                 :elt-kw ~elt-kw
                                                 :elt-uri ~elt-uri}})))]
                         elt#)))
              (alter-meta! (find-var (symbol (str *ns*) (str fn-tag)))
                            (fn [old new]
                              (merge old new))
                            {:miraj {:co-fn true
                                     :co-type typ
                                     :doc docstring
                                     :elt-kw elt-kw
                                     :elt-uri elt-uri}})
              #_(println "var: " (find-var (symbol (str *ns*) (str fn-tag))))))))
;              )))))

(defn optimize-js
  [doc]
  ;; (println "JS optimizer")
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
  [strategy doc]
  (reset! mode :html)
  (case strategy
    :js (optimize-js doc)
    :css (optimize-css doc)
    (println "Unrecognized optimizer: " strategy)))

(defn co-compile
  [file doc & mode]
  (let [s (if (= (first mode) :pprint)
            (do ;;(println "pprint")
                (with-out-str (pprint doc)))
            (serialize doc))]
    (spit file s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;FIXME: do we really want polymer stuff in here?
(def polymer-nss #{"iron" "paper" "google" "gold" "neon" "platinum" "font" "molecules"})

(defn get-href
  ([pfx sfx]
   ;;(log/trace "get-href: " pfx " - " sfx (type sfx))
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
  ;; (log/trace "get-href: " pfx " - " sfx (type sfx))
  (let [pfx (str/split (namespace sym) #"\.")
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

(defmulti get-resource-elt
  (fn [typ nsp sym #_uri]
    (println (str "GET-RESOURCE-elt: " typ " " nsp " " sym))
    typ))

(defmethod get-resource-elt :default
  [typ nsp sym #_uri]
  (println
   (str "get-resource-elt :default: " typ " " nsp " " sym))
  (element :link
           {:rel "import" :href (get-href (ns-name nsp) ref)}))
  ;; (element :link {:rel "import" :href uri}))

(defmethod get-resource-elt :polymer
  [typ nsp sym #_uri]
  (println (str "get-resource-elt :polymer: " typ " " nsp " " sym (meta sym)))
  (let [pfx (:resource-pfx (meta nsp))
        path (:elt-uri (:miraj (meta (find-var sym))))
        uri (str pfx "/" path)]
    (print "meta: " (meta (find-var sym)))
    (print "miraj: " (keys (meta (find-var sym))))
    (print "uri: " uri)
    (element :link {:rel "import" :href uri})))

  ;; (get-href (ns-name nsp) ref)}))

(defmethod get-resource-elt :link
  [typ nsp sym #_uri]
  (println "get-resource-elt :link: " (str typ " " nsp " " sym))
  (element :link
           {:rel "import" :href (get-href (ns-name nsp) ref)}))

(defmethod get-resource-elt :css
;; FIXME: support all attribs
  [typ nsp sym #_uri]
  (println "get-resource-elt :css: " (str typ " " nsp " " sym))
  (let [uri (deref (find-var sym))]
  (element :link {:rel "stylesheet" :href (:uri uri)
                  :media (if (:media uri) (:media uri) "all")})))

(defmethod get-resource-elt :js
;;FIXME support all attribs
  [typ nsp sym #_uri]
  (println "get-resource-elt :js: " (str typ " " nsp " " sym))
  (element :script {:href (deref (find-var sym))}))

(defn get-requirement
  [comp]
  (println (str "get-requirement: " comp))
  (let [nsp (find-ns (first comp))
        options (apply hash-map (rest comp))
        as-opt (:as options)
        refer-opts (:refer options)
        _ (println "component ns: " nsp (meta nsp))
        ]
    (if (nil? refer-opts)
      (element :link
               {:rel "import" :href (get-href nsp nil)})
      (for [ref refer-opts]
        (let [ref-sym (symbol (str (ns-name nsp)) (str ref))
              _ (println "ref sym: " ref-sym)
              _ (println "ref meta: " (meta (find-var ref-sym)))
              ns-type (:resource-type (meta nsp))
              _ (println "ns-type: " ns-type)
              ]
          (get-resource-elt ns-type nsp ref-sym #_ref-uri))))))

(def xsl-normalize
  (str
   "<xsl:stylesheet version='1.0' xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>"
   "<xsl:strip-space elements='*' />"
   "<xsl:output method='xml' encoding='UTF-8' indent='yes'/>"

   "<xsl:template match='html' priority='99'>"
     "<xsl:copy>"
       "<head>"
         "<xsl:apply-templates select='link|style|meta' mode='head'/>"
       "</head>"
       "<xsl:apply-templates select='@*|node()'/>"
     "</xsl:copy>"
   "</xsl:template>"

   "<xsl:template match='@*|node()'>"
     "<xsl:copy>"
       "<xsl:apply-templates select='@*|node()'/>"
     "</xsl:copy>"
   "</xsl:template>"

   "<xsl:template match='link|style|meta'/>"
   "<xsl:template match='link|style|meta' mode='head'>"
     "<xsl:copy>"
       "<xsl:apply-templates select='@*|node()'/>"
     "</xsl:copy>"
   "</xsl:template>"

   ;; "<xsl:template match='script'/>"
   ;; "<xsl:template match='script' mode='head'>"
   ;;   "<xsl:copy>"
   ;;     "<xsl:apply-templates select='@*|node()'/>"
   ;;   "</xsl:copy>"
   ;; "</xsl:template>"

   "<xsl:template match='body//link' priority='99' mode='head'/>"
   "</xsl:stylesheet>"))

(defn normalize
  "inspect args, if necessary create <head> etc."
  [& args]
  (println "HTML args: " args)
  (println "HTML meta: " (meta args))
  (reset! mode :html)
  (let [meta-elts (for [[k v] (meta (first args))]
                    (element :meta {:name (kw->nm k)
                                    :content (str v)}))
        h (list
           (update (first args)
                   :content
                   (fn [content]
                     (if (meta args)
                       (concat meta-elts content)
                       content))))
        normh (apply xsl-xform xsl-normalize h)
        ]
    normh))

;; (require [[polymer.paper :as paper :refer [button card]]])
(defmacro require
  [& args]
  (doseq [arg args]
    (eval
     (macroexpand
      `(do (println "REQUIRING: " ~arg)
           (clojure.core/require ~arg)))))
  (println "FOO: ")
  `(do
     (println "REQUIRing: " [~@args])
     (for [arg# [~@args]]
       (do (println "GET-REQ: " arg#)
           ;; (list (element :FOO))))))
           (let [r# (get-requirement arg#)]
             (println "REQRES: " r#)
             r#)))))
           ;;   (element :foo))))))

;;             (first r#))))))

;    (element :foo)))

;;  `(clojure.core/require ~@args))

  ;; `(flatten
  ;;   (for [arg# ~args]
  ;;     (do
  ;;       (println REQUIRING: " arg#)
  ;;       (clojure.core/require arg#)
  ;;       (flatten (get-requirement arg#))))))

;; <!-- import the shared styles  -->
;; <link rel="import" href="../shared-styles/foo.html">
;; <!-- include the shared styles -->
;; <style is="custom-style" include="foo-style"></style>
;; <style is="custom-style" include="bar-style"></style>
;; etc.

(defn get-import
  [import]
  (println (str "get-import: " import))
  (println (str "ns: " (first import) " " (type (first import))))
  (let [nsp (first import)
        styles (rest import)]
    (clojure.core/require nsp)
    (let [import-ns (find-ns nsp)
          _ (println "import ns: " import-ns)
          _ (println "import ns meta: " (meta import-ns))

;; dispatch to impl based on type meta of config spec

          uri (deref (find-var
                      (symbol (str (ns-name import-ns)) "uri")))
          _ (println "uri: " uri)
        ]
      (concat
       (list (element :link {:rel "import" :href uri}))
       ;; SHARED STYLES!
       (for [style styles]
         (do #_(println "style name: " style)
             (let [style-sym (symbol
                              (str (ns-name import-ns)) (str style))
                   ;; _ (println "style-sym: " style-sym)
                   style-ref (deref (find-var style-sym))]
               ;; (println "style ref: " style-ref)
               (element :style {:is "custom-style"
                                :include style}))))))))

  ;;   (println "style ns: " nsp)))
  ;;   (doseq [style styles]
  ;;     (println "style: " style))))
  ;;   (if (nil? refer-opts)
  ;;     (element :link
  ;;              {:rel "import" :href (get-href nsp nil)})
  ;;     (for [ref refer-opts]
  ;;       (let [ref-sym (symbol (str (ns-name nsp)) (str ref))
  ;;             ;; _ (println "ref meta: " (meta (find-var ref-sym)))
  ;;             ns-type (:resource-type (meta nsp))
  ;;             ref-type (:resource-type (meta (find-var ref-sym)))
  ;;             ref-uri (deref (find-var ref-sym))
  ;;             ;; _ (println "ref uri: " ref-uri)
  ;;             ;; _ (println "ref type: " ns-type (type ns-type))
  ;;             ]
  ;;         (get-resource-elt ns-type nsp ref-sym ref-uri))))))

;; (import '(shared.styles.foo fooa-style foob-style)
;;         '(shared.styles.bar bara-style barb-style)))


(defmacro import
  [& args]
  (println "import: " args)
  (for [arg args]
    `(do (get-import ~arg))))
