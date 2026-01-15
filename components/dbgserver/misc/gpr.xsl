<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema">

 <xsl:output method="text" encoding="utf-8" indent="no"/>

 <xsl:include href="gpr_config-functions.xsl"/>

 <xsl:strip-space elements="*"/>
 <xsl:template match="system/*"/>

 <xsl:template match="/system/config">
  <xsl:call-template name="gprHeader"/>
  <xsl:call-template name="booleanType"/>
  <xsl:for-each select="boolean">
   <xsl:if test="starts-with(@name, 'dbgserver_sink')">
    <xsl:call-template name="configBoolean"/>
   </xsl:if>
   <xsl:if test="@name='hsuart_supported'">
    <xsl:call-template name="configBooleanWithParams">
     <xsl:with-param name="name" select="'dbgserver_hsuart'"/>
     <xsl:with-param name="value" select="@value"/>
    </xsl:call-template>
   </xsl:if>
  </xsl:for-each>
  <xsl:call-template name="configBooleanWithParams">
   <xsl:with-param name="name" select="'subject_consoles_enabled'"/>
   <xsl:with-param name="value" select="count(/system/subjects/subject/component[@ref=$COMPONENTNAME]/map[starts-with(@logical,'subject_console_')]) &gt; 0"/>
  </xsl:call-template>
  <xsl:call-template name="gprFooter"/>
 </xsl:template>

</xsl:stylesheet>
