<?xml version="1.0"?>
<!-- (def xsl-optimize-js -->
<!--   (str -->
<xsl:stylesheet version='1.0' xmlns:xsl='http://www.w3.org/1999/XSL/Transform' >
  <xsl:strip-space elements='*' />
  <xsl:output method='xml' encoding='UTF-8' indent='yes'/>

  <xsl:template match='html'>
    <xsl:if test='not(head)'>
      <xsl:message terminate='yes'>OPTIMIZE-JS ERROR: &lt;head> not found; did you forget to run normalize first?</xsl:message>
    </xsl:if>
    <xsl:copy>
      <xsl:apply-templates select='@*|node()'/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match='@*|node()'>
    <xsl:copy>
      <xsl:apply-templates select='@*|node()'/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match='head'>
    <xsl:copy>
      <xsl:apply-templates select='meta[@name="charset"]' mode='optimize'/>
      <xsl:apply-templates select='//script' mode='polyfill'/>
      <xsl:apply-templates select='@*|node()'/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match='meta[@name="charset"]'/>
  <xsl:template match='meta[@name="charset"]' mode='optimize'>
    <xsl:copy>
      <xsl:apply-templates select='@*|node()'/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match='head/script'/>
  <xsl:template match='body/script'>
    <xsl:copy>
      <xsl:apply-templates select='@*|node()'/>
    </xsl:copy>
  </xsl:template>
  <xsl:template match='script' mode='optimize' priority='99'>
    <xsl:copy>
      <xsl:apply-templates select='@*|node()'/>
    </xsl:copy>
  </xsl:template>

  <!-- FIXME - put webcomponentsjs after all <meta> elts? -->
  <!-- (h/script {:src "bower_components/webcomponentsjs/webcomponents-lite.js"}) -->
  <xsl:template match='script' mode='polyfill'/>
  ;; "<xsl:template match='script[contains(@src, "webcomponentsjs")]'/>
  <xsl:template match='script[contains(@src, "webcomponentsjs")]' mode='optimize' priority='99'/>
  <xsl:template match='script[contains(@src, "webcomponentsjs")]' mode='polyfill' priority='99'>
    <xsl:copy>
      <xsl:apply-templates select='@*|node()'/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match='body' priority='99'>
    <xsl:copy>
      <xsl:apply-templates select='@*|node()'/>
      <xsl:apply-templates select='//head/script' mode='optimize'/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>

