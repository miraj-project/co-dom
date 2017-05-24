<?xml version="1.0"?>
<xsl:stylesheet version='1.0' xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>
  <xsl:strip-space elements='*' />
  <xsl:output method='xml' encoding='UTF-8' indent='yes'/>

  <xsl:template match='html' priority='99'>
    <xsl:copy>
      <head>
	<xsl:choose>
	  <xsl:when test='meta[@name=\"charset\"]'>
	    <xsl:apply-templates select='meta[@name=\"charset\"]' mode='charset'/>
	  </xsl:when>
	  <xsl:otherwise>
	    <meta name='charset' content='utf-8'/>
	  </xsl:otherwise>
	</xsl:choose>
	<xsl:apply-templates select='link|meta|style' mode='head'/>
	<xsl:apply-templates select='head/link|head/meta|head/style' mode='head'/>
	<xsl:apply-templates select='script|head/script' mode='head'/>
      </head>
      <xsl:apply-templates select='@*|node()'/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match='head'/>
  <xsl:template match='head' mode='head'>
    <xsl:apply-templates select='@*|node()' mode='head'/>
  </xsl:template>

  <xsl:template match='meta[@name=\"charset\"]' priority='99'/>
  <xsl:template match='meta[@name=\"charset\"]' mode='head'/>
  <xsl:template match='meta[@name=\"charset\"]' mode='charset'>
    <xsl:copy>
      <xsl:apply-templates select='@*|node()'/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match='@*|node()'>
    <xsl:copy>
      <xsl:apply-templates select='@*|node()'/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match='link|meta|script|style'/>
  <xsl:template match='link|meta|script|style' mode='head'>
    <xsl:copy>
      <xsl:apply-templates select='@*|node()'/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match='body//style' priority='99'>
    <xsl:copy>
      <xsl:apply-templates select='@*|node()'/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match='body//script' priority='99'>
    <xsl:copy>
      <xsl:apply-templates select='@*|node()'/>
    </xsl:copy>
  </xsl:template>

  ;; "<xsl:template match='@" miraj-pseudo-kw "' priority='99'>
  ;;     "<xsl:element name='link'>
  ;;       "<xsl:attribute name='rel'>import</xsl:attribute>
  ;;       "<xsl:attribute name='href'>
  ;;         "<xsl:text>/miraj/polymer/assets/polymer/polymer.html</xsl:text>
  ;;       "</xsl:attribute>
  ;;     "</xsl:element>
  ;; "</xsl:template>


  ;; ;; "<xsl:template match='script'/>
  ;; "<xsl:template match='script/text()'>
  ;;   "<xsl:text disable-output-escaping='yes'>
  ;;   "FOO &amp; BAR"
  ;;   "<xsl:copy>
  ;;     "<xsl:apply-templates select='@*|node()'/>
  ;;   "</xsl:copy>
  ;;   "</xsl:text>
  ;; "</xsl:template>

  ;; "<xsl:template match='style/text()'>
  ;;   "<xsl:text disable-output-escaping='yes'>
  ;;   "FOO &lt; BAR"
  ;;     ;; "<xsl:copy>
  ;;     ;;   "<xsl:apply-templates select='@*|node()'/>
  ;;     ;; "</xsl:copy>
  ;;   "</xsl:text>
  ;; "</xsl:template>

  <xsl:template match='body//link' priority='99' mode='head'/>
  </xsl:stylesheet>

<!-- ;; (def xsl-normalize -->
<!-- ;;   (str -->
<!-- ;;    "<xsl:stylesheet version='1.0' xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>" -->
<!-- ;;    "<xsl:strip-space elements='*' />" -->
<!-- ;;    "<xsl:output method='xml' encoding='UTF-8' indent='yes'/>" -->

<!-- ;;    "<xsl:template match='html' priority='99'>" -->
<!-- ;;      "<xsl:copy>" -->
<!-- ;;        "<head>" -->
<!-- ;;          "<xsl:choose>" -->
<!-- ;;            "<xsl:when test='meta[@name=\"charset\"]'>" -->
<!-- ;;              "<xsl:apply-templates select='meta[@name=\"charset\"]' mode='charset'/>" -->
<!-- ;;            "</xsl:when>" -->
<!-- ;;            "<xsl:otherwise>" -->
<!-- ;;              "<meta name='charset' content='utf-8'/>" -->
<!-- ;;            "</xsl:otherwise>" -->
<!-- ;;          "</xsl:choose>" -->
<!-- ;;          "<xsl:apply-templates select='link|meta|style' mode='head'/>" -->
<!-- ;;          "<xsl:apply-templates select='head/link|head/meta|head/style' mode='head'/>" -->
<!-- ;;          "<xsl:apply-templates select='script|head/script' mode='head'/>" -->
<!-- ;;        "</head>" -->
<!-- ;;        "<xsl:apply-templates select='@*|node()'/>" -->
<!-- ;;      "</xsl:copy>" -->
<!-- ;;    "</xsl:template>" -->

<!-- ;;    "<xsl:template match='head'/>" -->
<!-- ;;    "<xsl:template match='head' mode='head'>" -->
<!-- ;;      "<xsl:apply-templates select='@*|node()' mode='head'/>" -->
<!-- ;;    "</xsl:template>" -->

<!-- ;;    "<xsl:template match='meta[@name=\"charset\"]' priority='99'/>" -->
<!-- ;;    "<xsl:template match='meta[@name=\"charset\"]' mode='head'/>" -->
<!-- ;;    "<xsl:template match='meta[@name=\"charset\"]' mode='charset'>" -->
<!-- ;;      "<xsl:copy>" -->
<!-- ;;        "<xsl:apply-templates select='@*|node()'/>" -->
<!-- ;;      "</xsl:copy>" -->
<!-- ;;    "</xsl:template>" -->

<!-- ;;    "<xsl:template match='@*|node()'>" -->
<!-- ;;      "<xsl:copy>" -->
<!-- ;;        "<xsl:apply-templates select='@*|node()'/>" -->
<!-- ;;      "</xsl:copy>" -->
<!-- ;;    "</xsl:template>" -->

<!-- ;;    "<xsl:template match='link|meta|script|style'/>" -->
<!-- ;;    "<xsl:template match='link|meta|script|style' mode='head'>" -->
<!-- ;;      "<xsl:copy>" -->
<!-- ;;        "<xsl:apply-templates select='@*|node()'/>" -->
<!-- ;;      "</xsl:copy>" -->
<!-- ;;    "</xsl:template>" -->

<!-- ;;    "<xsl:template match='body//style' priority='99'>" -->
<!-- ;;      "<xsl:copy>" -->
<!-- ;;        "<xsl:apply-templates select='@*|node()'/>" -->
<!-- ;;      "</xsl:copy>" -->
<!-- ;;    "</xsl:template>" -->

<!-- ;;    "<xsl:template match='body//script' priority='99'>" -->
<!-- ;;      "<xsl:copy>" -->
<!-- ;;        "<xsl:apply-templates select='@*|node()'/>" -->
<!-- ;;      "</xsl:copy>" -->
<!-- ;;    "</xsl:template>" -->

<!-- ;;    ;; "<xsl:template match='@" miraj-pseudo-kw "' priority='99'>" -->
<!-- ;;    ;;     "<xsl:element name='link'>" -->
<!-- ;;    ;;       "<xsl:attribute name='rel'>import</xsl:attribute>" -->
<!-- ;;    ;;       "<xsl:attribute name='href'>" -->
<!-- ;;    ;;         "<xsl:text>/bower_components/polymer/polymer.html</xsl:text>" -->
<!-- ;;    ;;       "</xsl:attribute>" -->
<!-- ;;    ;;     "</xsl:element>" -->
<!-- ;;    ;; "</xsl:template>" -->


<!-- ;;    ;; ;; "<xsl:template match='script'/>" -->
<!-- ;;    ;; "<xsl:template match='script/text()'>" -->
<!-- ;;    ;;   "<xsl:text disable-output-escaping='yes'>" -->
<!-- ;;    ;;   "FOO &amp; BAR" -->
<!-- ;;    ;;   "<xsl:copy>" -->
<!-- ;;    ;;     "<xsl:apply-templates select='@*|node()'/>" -->
<!-- ;;    ;;   "</xsl:copy>" -->
<!-- ;;    ;;   "</xsl:text>" -->
<!-- ;;    ;; "</xsl:template>" -->

<!-- ;;    ;; "<xsl:template match='style/text()'>" -->
<!-- ;;    ;;   "<xsl:text disable-output-escaping='yes'>" -->
<!-- ;;    ;;   "FOO &lt; BAR" -->
<!-- ;;    ;;     ;; "<xsl:copy>" -->
<!-- ;;    ;;     ;;   "<xsl:apply-templates select='@*|node()'/>" -->
<!-- ;;    ;;     ;; "</xsl:copy>" -->
<!-- ;;    ;;   "</xsl:text>" -->
<!-- ;;    ;; "</xsl:template>" -->

<!-- ;;    "<xsl:template match='body//link' priority='99' mode='head'/>" -->
<!-- ;;    "</xsl:stylesheet>")) -->

