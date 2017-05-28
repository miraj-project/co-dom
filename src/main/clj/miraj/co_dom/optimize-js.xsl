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

      <!-- FIXME: the following should be in normalize or somewhere else other than optimize-js -->
      <xsl:element name="script">
        <xsl:text>(function() {
  if ('registerElement' in document
      &amp;&amp; 'import' in document.createElement('link')
      &amp;&amp; 'content' in document.createElement('template')) {
    // platform is good!
    // console.log('No Polyfill needed!');
   } else {
    // polyfill the platform!
    // console.log('Using Polyfill!');
    var e = document.createElement('script');
    e.src = '/miraj/polymer/assets/webcomponentsjs/webcomponents.js';
    document.head.insertBefore(e, document.head.firstChild);
   };

   if(document.head.createShadowRoot) {
     // console.log('Shadow DOM v0 supported!');
      if(document.head.attachShadow) {
      // console.log('(Shadow DOM v1 supported by browswer but not used with this version of Miraj/Polymer)');
      }
     // polymer v1 default is shady dom; to switch, run this script before Polymer is imported */
     // console.log('Switching from default Shady DOM to v0 Shadow DOM.');
     window.Polymer = {
      dom: 'shadow',
      lazyRegister: true
      };
    } else if(document.head.attachShadow) {
          // console.log('Shadow DOM v0 unsupported!');
          // console.log('Shadow DOM v1 supported!');
	  // currently Miraj only supports Polymer v1.x, which supports Webcomponents v0,
	  // so on browsers that support v1 Shadow DOM but not v0 (e.g. Safari 10),
	  // Polymer version 1.x will always use Shady DOM.
	  // console.log('Using v0 Shady DOM');
    } else {
    // console.log('Shadow DOM unsupported!');
    }
})();</xsl:text>
      </xsl:element>
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

      <!-- FIXME: the following should be in normalize, or somewhere other than optimize-js -->
      <xsl:element name="script">
	<xsl:text>if (!window.HTMLImports) {
	document.dispatchEvent(new CustomEvent('WebComponentsReady', {bubbles: true}));
	}</xsl:text>
      </xsl:element>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>

