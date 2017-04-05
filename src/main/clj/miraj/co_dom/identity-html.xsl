<?xml version="1.0"?>
<xsl:stylesheet version='1.0' xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>
  <xsl:strip-space elements='*' />
  <xsl:output method='xml' encoding='UTF-8' indent='yes' cdata-section-elements='script style'/>

  <xsl:template match='node()'>
    <xsl:copy>
      <xsl:apply-templates select='@*|node()'/>
    </xsl:copy>
  </xsl:template>

  <xsl:variable name="miraj-pseudo-attr">
    <xsl:text>__MIRAJ_PSEUDO_sfmWqa5HptMJ6ATR</xsl:text>
  </xsl:variable>

  <xsl:template match='//*[@__MIRAJ_PSEUDO_sfmWqa5HptMJ6ATR]' priority='999'>
    <xsl:copy>
      <xsl:apply-templates select='@*|node()'/>
    </xsl:copy>
    <xsl:element name='style'>
      <xsl:value-of select='@__MIRAJ_PSEUDO_sfmWqa5HptMJ6ATR'/>
    </xsl:element>
  </xsl:template>



  <xsl:template match='html'>
    <xsl:text disable-output-escaping='yes'>&lt;!doctype html&gt;</xsl:text>
    <xsl:text>&#x0A;</xsl:text>
    <xsl:copy>
      <xsl:apply-templates select='@*|node()'/>
    </xsl:copy>
  </xsl:template>

  <xsl:variable name="html5-void-elts">
    <xsl:text>area | base | br | col |  embed | hr | img | input | keygen | link | meta | param | source | track | wbr</xsl:text>
  </xsl:variable>

  <xsl:template priority="99" match='area | base | br | col |  embed | hr | img | input | keygen | link | meta | param | source | track | wbr'>
    <xsl:copy>
      <xsl:apply-templates select='@*|node()'/>
      <xsl:text>VOID_333109</xsl:text>
    </xsl:copy>
  </xsl:template>

  <!-- remove self-closing tags -->
  <xsl:template match='*[not(node()) and not(string(.))]'>
     <!-- <xsl:message>EMPTY TAG</xsl:message> -->
    <xsl:copy>
      <xsl:apply-templates select='@*|node()'/>
      <xsl:text>_EMPTY_333109</xsl:text>
    </xsl:copy>
  </xsl:template>

  <xsl:template match='script' priority='999'>
    <xsl:copy>
      <xsl:apply-templates select='@*|node()'/>
      <xsl:if test='not(node()) and not(string(.))'>
        <xsl:text>_EMPTY_333109</xsl:text>
      </xsl:if>
    </xsl:copy>
  </xsl:template>

  <xsl:template match='script/text()' priority='999'>
    <xsl:text disable-output-escaping='yes'>&#x0A;</xsl:text>
    <xsl:value-of select='.'/>
    <xsl:text disable-output-escaping='yes'>&#x0A;</xsl:text>
      <!-- <xsl:copy> -->
      <!-- <xsl:apply-templates select='@*|node()'/> -->
      <!-- </xsl:copy> -->
  </xsl:template>

  <xsl:template match="@*">
 <!-- Handle HTML boolean attributes -->
 <!-- If the attribute is present, its value must either be the empty -->
 <!-- string or a value that is an ASCII case-insensitive match for -->
 <!-- the attribute's canonical name, with no leading or trailing -->
 <!-- whitespace. The values "true" and "false" are not allowed on -->
 <!-- boolean attributes. -->
 <!-- <xsl:message>Attr: <xsl:value-of select='name()'/> = <xsl:value-of select='.'/></xsl:message> -->
    <xsl:choose>
       <!-- ONLY transform PSEUDO attributes -->
      <xsl:when test='name() = $miraj-pseudo-attr'>
	<!-- omit the attribute -->
      </xsl:when>
      <!-- ;; "<xsl:when test='name() = .'> -->
      <!-- ;; ;; FIXME: test for case-insensitive match to canonical name -->
      <!-- ;;   "<xsl:attribute name='{name()}'> -->
      <!-- ;;     miraj-boolean-tag -->
      <!-- ;;   "</xsl:attribute> -->
      <!-- ;; "</xsl:when> -->
      <!-- ;; "<xsl:when test='. = \"\"'> -->
      <!-- ;;   "<xsl:attribute name='{name()}'> -->
      <!-- ;;     miraj-boolean-tag -->
      <!-- ;;   "</xsl:attribute> -->
      <!-- ;; "</xsl:when> -->
      <!-- ;; "<xsl:when test='. = concat(\":\", name())'> -->
      <!-- ;;   "<xsl:attribute name='{name()}'> -->
      <!-- ;;     miraj-boolean-tag -->
      <!-- ;;   "</xsl:attribute> -->
      <!-- ;; "</xsl:when> -->
      <!-- ;; (str "<xsl:when test='. = \"" miraj-boolean-tag "\"'>") -->
      <!-- ;;   "<xsl:attribute name='{name()}'> -->
      <!-- ;;     miraj-boolean-tag -->
      <!-- ;;   "</xsl:attribute> -->
      <!-- ;; "</xsl:when> -->
      <!-- ;; {:foo ""} -->
      <xsl:otherwise>
	<xsl:copy/>
      </xsl:otherwise>
    </xsl:choose>


  </xsl:template>
  </xsl:stylesheet>
