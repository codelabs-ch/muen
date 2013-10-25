<?xml version="1.0" encoding="ISO-8859-1"?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="@*|node()">
  <xsl:copy>
    <xsl:apply-templates select="@*|node()"/>
  </xsl:copy>
</xsl:template>

<xsl:template match="div[@class='toc_block']">
<div class="toc_block">
   <ul class="toc">
      <xsl:apply-templates select="//h2" mode="toc"/>
   </ul>
</div>
<div class="clear"></div>
</xsl:template>

<xsl:template match="h2" mode="toc">
   <li class="toc">
      <a class="toc">
         <xsl:attribute name="href">
            <xsl:text>#sec</xsl:text>
            <xsl:value-of select="@id"/>
         </xsl:attribute>
         <xsl:value-of select="."/>
      </a>
   </li>
</xsl:template>

</xsl:stylesheet>
