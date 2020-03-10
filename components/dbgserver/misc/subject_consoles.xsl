<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema">

<xsl:output method="text" encoding="utf-8" indent="no"/>

<xsl:strip-space elements="*"/>
<xsl:template match="text()"/>
<xsl:template match="/">
 <xsl:if test="count(/system/subjects/subject/component[@ref=$COMPONENTNAME])='0'">
  <xsl:message terminate="yes">No <xsl:value-of select="$COMPONENTNAME"/> component reference in policy</xsl:message>
 </xsl:if>
 <xsl:if test="count(/system/subjects/subject/component[@ref=$COMPONENTNAME]/map[starts-with(@logical,'subject_console_')])='0'">
  <xsl:variable name="subjName" select="/system/subjects/subject[component/@ref=$COMPONENTNAME]/@name"/>
  <xsl:message terminate="no">No console mappings by subject '<xsl:value-of select="$subjName"/>'</xsl:message>
 </xsl:if>
 <xsl:apply-templates/>
</xsl:template>

<xsl:template match="/system/subjects/subject/component">
 <xsl:if test="@ref=$COMPONENTNAME">
  <xsl:text>&lt;include&gt;&#10;</xsl:text>
  <xsl:call-template name="extractConsoleInputlMappings"/>
  <xsl:call-template name="extractConsoleOutputMappings"/>
  <xsl:text>&lt;/include&gt;&#10;</xsl:text>
 </xsl:if>
</xsl:template>

<xsl:template name="extractConsoleInputlMappings">
 <xsl:text>&lt;array elementSize=&#34;16&#35;1000&#35;&#34; logical=&#34;subject_consoles_in&#34; eventBase=&#34;auto&#34;&gt;</xsl:text>
 <xsl:text>&#10;</xsl:text>
  <xsl:for-each select="map[starts-with(@logical,'subject_console_in_')]">
   <xsl:text>	&lt;writer logical=&#34;</xsl:text>
   <xsl:value-of select="@logical"/>
   <xsl:text>&#34;/&gt;&#10;</xsl:text>
  </xsl:for-each>
 <xsl:text>&lt;/array&gt;&#10;</xsl:text>
</xsl:template>

<xsl:template name="extractConsoleOutputMappings">
 <xsl:text>&lt;array elementSize=&#34;16&#35;1000&#35;&#34; logical=&#34;subject_consoles_out&#34;&gt;</xsl:text>
 <xsl:text>&#10;</xsl:text>
  <xsl:for-each select="map[starts-with(@logical,'subject_console_out_')]">
   <xsl:text>	&lt;reader logical=&#34;</xsl:text>
   <xsl:value-of select="@logical"/>
   <xsl:text>&#34;/&gt;&#10;</xsl:text>
  </xsl:for-each>
  <xsl:text>&lt;/array&gt;&#10;</xsl:text>
 </xsl:template>

</xsl:stylesheet>
