<?xml version="1.0"?>
<xsl:stylesheet version='1.0' xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>
  <xsl:strip-space elements='*' />
  <xsl:output method='xml' encoding='UTF-8' indent='yes'/>

  <xsl:template match='@*|node()'>
    <xsl:copy>
      <xsl:apply-templates select='@*|node()'/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>


<!-- (def xsl-identity-transform-xml -->
<!--    ;; see http://news.oreilly.com/2008/07/simple-pretty-printing-with-xs.html -->
<!--   (str -->
<!--    "<xsl:stylesheet version='1.0' xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>" -->
<!--    "<xsl:strip-space elements='*' />" -->
<!--    "<xsl:output method='xml' encoding='UTF-8' indent='yes'/>" -->

<!--    "<xsl:template match='@*|node()'>" -->
<!--      "<xsl:copy>" -->
<!--        "<xsl:apply-templates select='@*|node()'/>" -->
<!--      "</xsl:copy>" -->
<!--    "</xsl:template>" -->
<!--    "</xsl:stylesheet>")) -->
