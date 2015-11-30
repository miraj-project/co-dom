;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Functions to parse XML into lazy sequences and lazy trees and
  emit these as text."
  :author "Chris Houser"}
  clojure.data.xml
  (:require [clojure.string :as str])
  (:import [java.io ByteArrayInputStream StringReader StringWriter]
           [javax.xml.stream XMLInputFactory
                             XMLStreamReader
                             XMLStreamConstants]
           [javax.xml.parsers DocumentBuilder DocumentBuilderFactory]
           [javax.xml.transform.dom DOMSource]
           [javax.xml.transform OutputKeys TransformerFactory]
           [javax.xml.transform.stream StreamSource StreamResult]
           (java.nio.charset Charset)
           (java.io Reader)))

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

(defn write-attributes [attrs ^javax.xml.stream.XMLStreamWriter writer]
  (doseq [[k v] attrs]
    (let [[attr-ns attr-name] (qualified-name k)]
      ;; if v is keyword, convert it to polymer annotation, e.g. :items :items => "{{items}}"
      (if attr-ns
        (.writeAttribute writer attr-ns attr-name (str v))
        (.writeAttribute writer attr-name (str v))))))

(declare serialize)

; Represents a node of an XML tree
(defrecord Element [tag attrs content]
  java.lang.Object
  (toString [x]
    (do (print "Element toString: " x "\n")
        (let [sw (java.io.StringWriter.)]
          (serialize x)))))

(defrecord CData [content])
(defrecord Comment [content])

;;FIXME: support fragments

 ;; private static final String IDENTITY_XSLT =
 ;;    "<xsl:stylesheet xmlns:xsl='http://www.w3.org/1999/XSL/Transform'"
 ;;    + " version='1.0'>"
 ;;    + "<xsl:template match='/'><xsl:copy-of select='.'/>"
 ;;    + "</xsl:template></xsl:stylesheet>";
(def identity-transform
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
   "<xsl:template match=\"*[.='']\">"
     "<xsl:copy>"
       "<xsl:apply-templates select='@*|node()'/>"
       "VOID"
     "</xsl:copy>"
   "</xsl:template>"
   "</xsl:stylesheet>"))

(def indent-transform
;; http://www.xml.com/pub/a/2006/11/29/xslt-xml-pretty-printer.html?page=3#finalCode
(str
 "<?xml version='1.0' encoding='UTF-8'?>
<!--
Converts XML into a nice readable format.
Tested with Saxon 6.5.3.
As a test, this stylesheet should not change when run on itself.
But note that there are no guarantees about attribute order within an
element (see http://www.w3.org/TR/xpath#dt-document-order), or about
which characters are escaped (see
http://www.w3.org/TR/xslt#disable-output-escaping).
I did not test processing instructions, CDATA sections, or
namespaces.

Hew Wolff
Senior Engineer
Art & Logic, Inc.
www.artlogic.com
-->

<xsl:stylesheet xmlns:xsl='http://www.w3.org/1999/XSL/Transform' version='1.0'>
   <!-- Take control of the whitespace. -->

   <xsl:output method='html' indent='no' encoding='UTF-8'/>
   <xsl:strip-space elements='*'/>
   <xsl:preserve-space elements='xsl:text'/>

   <!-- Copy comments, and elements recursively. -->

   <xsl:template match='@*|node()'> <!-- match='*|comment()'> -->
      <xsl:param name='depth'>0</xsl:param>

      <!--
      Set off from the element above if one of the two has children.
      Also, set off a comment from an element.
      And set off from the XML declaration if necessary.
      -->

      <xsl:variable name='isFirstNode' select='count(../..) = 0 and position() = 1'/>
      <xsl:variable name='previous' select='preceding-sibling::node()[1]'/>
      <xsl:variable name='adjacentComplexElement' select='count($previous/*) &gt; 0 or count(*) &gt; 0'/>
      <xsl:variable name='adjacentDifferentType' select='not(($previous/self::comment() and self::comment()) or ($previous/self::* and self::*))'/>

      <xsl:if test='$isFirstNode or ($previous and ($adjacentComplexElement or $adjacentDifferentType))'>
         <xsl:text>&#xA;</xsl:text>
      </xsl:if>

      <!-- Start a new line. -->

      <xsl:text>&#xA;</xsl:text>

      <xsl:call-template name='indent'>
         <xsl:with-param name='depth' select='$depth'/>
      </xsl:call-template>

      <xsl:copy>
         <xsl:if test='self::*'>
            <xsl:copy-of select='@*'/>

            <xsl:apply-templates>
               <xsl:with-param name='depth' select='$depth + 1'/>
            </xsl:apply-templates>

            <xsl:if test='count(*) &gt; 0'>
               <xsl:text>&#xA;</xsl:text>

               <xsl:call-template name='indent'>
                  <xsl:with-param name='depth' select='$depth'/>
               </xsl:call-template>
            </xsl:if>
         </xsl:if>
      </xsl:copy>

      <xsl:variable name='isLastNode' select='count(../..) = 0 and position() = last()'/>

      <xsl:if test='$isLastNode'>
         <xsl:text>&#xA;</xsl:text>
      </xsl:if>
   </xsl:template>

   <xsl:template name='indent'>
      <xsl:param name='depth'/>

      <xsl:if test='$depth &gt; 0'>
         <xsl:text>   </xsl:text>

         <xsl:call-template name='indent'>
            <xsl:with-param name='depth' select='$depth - 1'/>
         </xsl:call-template>
      </xsl:if>
   </xsl:template>

   <!-- Escape newlines within text nodes, for readability. -->

   <xsl:template match='text()'>
      <xsl:call-template name='escapeNewlines'>
         <xsl:with-param name='text'>
            <xsl:value-of select='.'/>
         </xsl:with-param>
      </xsl:call-template>
   </xsl:template>

   <xsl:template name='escapeNewlines'>
      <xsl:param name='text'/>

      <xsl:if test='string-length($text) &gt; 0'>
         <xsl:choose>
            <xsl:when test='substring($text, 1, 1) = \"&#xA;\"'>
               <xsl:text disable-output-escaping='yes'>&amp;#xA;</xsl:text>
            </xsl:when>

            <xsl:otherwise>
               <xsl:value-of select='substring($text, 1, 1)'/>
            </xsl:otherwise>
         </xsl:choose>

         <xsl:call-template name='escapeNewlines'>
            <xsl:with-param name='text' select='substring($text, 2)'/>
         </xsl:call-template>
      </xsl:if>
   </xsl:template>
</xsl:stylesheet>"))

(defn pprint
  [fmt & ml]
  (let [s (if (or (= :html fmt) (= :xml fmt))
            (first ml)
            (if (keyword? fmt)
              (throw (Exception. "only :html and :xml supported"))
              fmt))
        fmt (if (keyword? fmt) fmt :xml)
        ml (if (string? s) s
               (serialize s))
        ;; log (println "ML: " ml)
        ;; see http://www.ling.helsinki.fi/kit/2004k/ctl257/JavaXSLT/Ch05.html
        ;; xml-string (StringReader. ml)
        xmlSource (StreamSource.  (StringReader. ml))

        ;; xslt-string (StringReader. identity-transform)
        ;; xsltSource (StreamSource. (StringReader. identity-transform))

        xmlOutput (StreamResult. (StringWriter.))

        factory (TransformerFactory/newInstance)
        transformer (if (= :html fmt)
                      (.newTransformer factory
                                       (StreamSource. (StringReader. identity-transform)))
                      (.newTransformer factory))]

    (.setOutputProperty transformer OutputKeys/INDENT "yes")
    (.setOutputProperty transformer "{http://xml.apache.org/xslt}indent-amount", "4")
    (if (.startsWith ml "<?xml")
      (.setOutputProperty transformer OutputKeys/OMIT_XML_DECLARATION "no")
      (.setOutputProperty transformer OutputKeys/OMIT_XML_DECLARATION "yes"))

    ;; (if doctype
    ;;   (do
    ;;     (.write os "<!DOCTYPE html>\n")))
    (.transform transformer xmlSource xmlOutput)
    (println (if (= :html fmt)
               (str/replace (.toString (.getWriter xmlOutput)) #"VOID<[^>]+>" "")
               (.toString (.getWriter xmlOutput))))))

(defn emit-start-tag [event ^javax.xml.stream.XMLStreamWriter writer]
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
  #_(println "EMIT-END-TAG: " (:name event))
  ;;(.writeEndElement writer)
  (.write writer (str "</" (name (:name event)) ">")))

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
    :void-element (do
                    #_(println "VOID ELT")
                    (emit-start-tag event stream-writer))
    :chars #_(if (:disable-escaping opts)
             (do ;; to prevent escaping of elts embedded in (str ...) constructs:
               (.writeCharacters stream-writer "") ; switches mode?
               (.write writer (:str event)))       ; writes without escaping < & etc.
             )
    (.writeCharacters stream-writer (:str event))
    :kw (.writeCharacters stream-writer (:str event))
    :symbol (.writeCharacters stream-writer (:str event))
    :cdata (emit-cdata (:str event) stream-writer)
    :comment (.writeComment stream-writer (:str event))))

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
    (if (= (:tag element) :link)
      (Event. :start-element (:tag element) (:attrs element) nil)
      (Event. :start-element (:tag element) (:attrs element) nil)))
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

  clojure.lang.Keyword
  (gen-event [kw]
    (let [nm (name kw)
          ns (namespace kw)]
    (Event. :kw nil nil
            (if (.endsWith nm "%")
              (str "{{" ns (if ns ".") (subs nm 0 (- (count nm) 1))  "}}")
              (str "[[" (namespace kw) (if (namespace kw) ".") (name kw) "]]")))))
  (next-events [_ next-items]
    next-items)

  ;; clojure.lang.Symbol
  ;; (gen-event [kw]
  ;;   (let [nm (name kw)
  ;;         ns (namespace kw)]
  ;;   (Event. :kw nil nil
  ;;           (if (.endsWith nm "%")
  ;;             (str "{{" ns (if ns ".") (subs nm 0 (- (count nm) 1))  "}}")
  ;;             (str "[[" (namespace kw) (if (namespace kw) ".") (name kw) "]]")))))
  ;; (next-events [_ next-items]
  ;;   next-items)

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
       (cons (gen-event e)
             (flatten-elements (next-events e (rest elements))))))))

(defn element [tag & [attrs & content]]
  (Element. tag (or attrs {}) (remove nil? content)))

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
      (when (= :start-element (.type event))
        (Element. (.name event) (.attrs event) contents)))
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
  (let [^javax.xml.stream.XMLStreamWriter stream-writer
        (-> (javax.xml.stream.XMLOutputFactory/newInstance)
            (.createXMLStreamWriter writer))]

    (when (instance? java.io.OutputStreamWriter writer)
      (check-stream-encoding writer (or (:encoding opts) "UTF-8")))

    (if (:html opts)
      (.writeDTD stream-writer "<!DOCTYPE html>"))
    (if (:with-xml-declaration opts)
      (.writeStartDocument stream-writer (or (:encoding opts) "UTF-8") "1.0"))
    ;; (println "flattening: " e)
    (doseq [event (flatten-elements [e])]
      (do #_(prn "event: " event)
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
    :encoding <str>          Character encoding to use
    :with-xml-declaration <bool>, default false"
  [& args]
  ;; (prn "serializing: " args)
  (let [^java.io.StringWriter sw (java.io.StringWriter.)]
    (if (= :html (first args))
      (emit (rest args) sw :html true :with-xml-declaration false)
      (if (= :with-xml-declaration (first args))
        (emit (rest args) sw :with-xml-declaration true)
        (emit args sw :with-xml-declaration false)))
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
