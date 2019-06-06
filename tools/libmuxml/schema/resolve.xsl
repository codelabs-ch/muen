<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema">

 <xsl:param name="format"/>

 <xsl:output method="xml" indent="yes"/>

 <xsl:strip-space elements="*"/>

 <xsl:template match="/">
  <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
   <xsl:apply-templates select="xs:schema/*"/>
  </xs:schema>
 </xsl:template>

 <xsl:template match="xs:include">
  <xsl:apply-templates select="document(@schemaLocation)/xs:schema/*"/>
 </xsl:template>

 <xsl:template match="xs:include_format">
  <xsl:apply-templates select="document(concat(concat($format,'/'), @schemaLocation))/xs:schema/*"/>
 </xsl:template>

 <xsl:template match="*">
  <xsl:copy-of select="."/>
 </xsl:template>

</xsl:stylesheet>
