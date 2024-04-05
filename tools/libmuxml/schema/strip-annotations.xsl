<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema">

  <xsl:output encoding="UTF-8" method="xml" indent="yes"/>

  <xsl:template match="@* | node()">
    <xsl:copy>
      <xsl:apply-templates select="@* | node()"/>
    </xsl:copy>
  </xsl:template>

  <!-- Remove spaces and empty lines -->
  <xsl:template match="*/text()[normalize-space()]">
    <xsl:value-of select="normalize-space()"/>
  </xsl:template>

  <xsl:template match="*/text()[not(normalize-space())]" />

  <!-- Remove annotation elements -->
  <xsl:template match="xs:annotation" />

</xsl:stylesheet>
