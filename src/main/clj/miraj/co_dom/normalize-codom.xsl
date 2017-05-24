<?xml version="1.0"?>
<xsl:stylesheet version='1.0' xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>
  <xsl:strip-space elements='*' />
  <xsl:output method='xml' encoding='UTF-8' indent='yes'/>

  <xsl:template match='/' priority='99'>
    <xsl:apply-templates select='@*|node()'/>
  </xsl:template>

  <xsl:template match='CODOM_56477342333109' priority='99'>
    <xsl:copy>
      <xsl:element name='link'>
	<xsl:attribute name='rel'>import</xsl:attribute>
	<xsl:attribute name='href'>
	  <xsl:text>/miraj/polymer/assets/polymer/polymer.html</xsl:text>
	</xsl:attribute>
      </xsl:element>
      <xsl:apply-templates select='//link' mode='head'/>
      <xsl:element name='dom-module'>
	<xsl:attribute name='id'>
	  <xsl:value-of select='@id'/>
	</xsl:attribute>
	<template>
	  <xsl:apply-templates/>
	</template>
      </xsl:element>
    </xsl:copy>
  </xsl:template>

  <xsl:template match='@*|node()'>
    <xsl:copy>
      <xsl:apply-templates select='@*|node()'/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match='link'/>
  <xsl:template match='link' mode='head'>
    <xsl:copy>
      <xsl:apply-templates select='@*|node()'/>
    </xsl:copy>
  </xsl:template>

</xsl:stylesheet>

<!-- ;; (def xsl-normalize-codom -->
<!-- ;;   (str -->
<!-- ;;    "<xsl:stylesheet version='1.0' xmlns:xsl='http://www.w3.org/1999/XSL/Transform'>" -->
<!-- ;;    "<xsl:strip-space elements='*' />" -->
<!-- ;;    "<xsl:output method='xml' encoding='UTF-8' indent='yes'/>" -->

<!-- ;;    "<xsl:template match='/' priority='99'>" -->
<!-- ;;      "<xsl:apply-templates select='@*|node()'/>" -->
<!-- ;;    "</xsl:template>" -->

<!-- ;;    "<xsl:template match='CODOM_56477342333109' priority='99'>" -->
<!-- ;;      "<xsl:copy>" -->
<!-- ;;        "<xsl:element name='link'>" -->
<!-- ;;          "<xsl:attribute name='rel'>import</xsl:attribute>" -->
<!-- ;;          "<xsl:attribute name='href'>" -->
<!-- ;;            "<xsl:text>/bower_components/polymer/polymer.html</xsl:text>" -->
<!-- ;;          "</xsl:attribute>" -->
<!-- ;;        "</xsl:element>" -->
<!-- ;;        "<xsl:apply-templates select='//link' mode='head'/>" -->
<!-- ;;        "<xsl:element name='dom-module'>" -->
<!-- ;;          "<xsl:attribute name='id'>" -->
<!-- ;;            "<xsl:value-of select='@id'/>" -->
<!-- ;;          "</xsl:attribute>" -->
<!-- ;;          "<template>" -->
<!-- ;;            "<xsl:apply-templates/>" -->
<!-- ;;          "</template>" -->
<!-- ;;        "</xsl:element>" -->
<!-- ;;      "</xsl:copy>" -->
<!-- ;;    "</xsl:template>" -->

<!-- ;;    "<xsl:template match='@*|node()'>" -->
<!-- ;;      "<xsl:copy>" -->
<!-- ;;        "<xsl:apply-templates select='@*|node()'/>" -->
<!-- ;;      "</xsl:copy>" -->
<!-- ;;    "</xsl:template>" -->

<!-- ;;    "<xsl:template match='link'/>" -->
<!-- ;;    "<xsl:template match='link' mode='head'>" -->
<!-- ;;      "<xsl:copy>" -->
<!-- ;;        "<xsl:apply-templates select='@*|node()'/>" -->
<!-- ;;      "</xsl:copy>" -->
<!-- ;;    "</xsl:template>" -->

<!-- ;;    "</xsl:stylesheet>")) -->

