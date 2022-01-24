<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema">

	<xsl:template name="checkComponentReference">
		<xsl:if test="count(/system/subjects/subject/component[@ref=$COMPONENTNAME])='0'">
			<xsl:message terminate="yes">No <xsl:value-of select="$COMPONENTNAME"/> component reference in policy</xsl:message>
		</xsl:if>
	</xsl:template>

	<xsl:template name="configString">
		<xsl:param name="name"/>
		<xsl:param name="value"/>
		<xsl:text> &lt;string name=&#34;</xsl:text>
		<xsl:value-of select="$name"/>
		<xsl:text>&#34; value=&#34;</xsl:text>
		<xsl:value-of select="$value"/>
		<xsl:text>&#34;/&gt;</xsl:text>
	</xsl:template>

	<xsl:template name="configBoolean">
		<xsl:param name="name"/>
		<xsl:param name="value"/>
		<xsl:text> &lt;boolean name=&#34;</xsl:text>
		<xsl:value-of select="$name"/>
		<xsl:text>&#34; value=&#34;</xsl:text>
		<xsl:value-of select="$value"/>
		<xsl:text>&#34;/&gt;</xsl:text>
	</xsl:template>

	<xsl:template name="configHeader">
		<xsl:text>&lt;config&gt;</xsl:text>
		<xsl:text>&#10;</xsl:text>
	</xsl:template>

	<xsl:template name="configFooter">
		<xsl:text>&lt;/config&gt;</xsl:text>
		<xsl:text>&#10;</xsl:text>
	</xsl:template>

	<xsl:template name="extractConfigString">
		<xsl:param name="name"/>
		<xsl:variable name="value" select="/system/config/string[@name=$name]/@value"/>
		<xsl:choose>
			<xsl:when test="$value!=''">
				<xsl:call-template name="configString">
					<xsl:with-param name="name" select="$name"/>
					<xsl:with-param name="value" select="$value"/>
				</xsl:call-template>
			</xsl:when>
			<xsl:otherwise>
				<xsl:message terminate="yes">Unable to extract string, config variable <xsl:value-of select="$name"/> missing</xsl:message>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

 <xsl:template name="extractConfigBoolean">
  <xsl:param name="name"/>
  <xsl:param name="defaultValue"/>
  <xsl:variable name="value" select="/system/config/boolean[@name=$name]/@value"/>
  <xsl:choose>
   <xsl:when test="$value!=''">
    <xsl:call-template name="configBoolean">
     <xsl:with-param name="name" select="$name"/>
     <xsl:with-param name="value" select="$value"/>
    </xsl:call-template>
   </xsl:when>
   <xsl:otherwise>
    <xsl:call-template name="configBoolean">
     <xsl:with-param name="name" select="$name"/>
     <xsl:with-param name="value" select="$defaultValue"/>
    </xsl:call-template>
   </xsl:otherwise>
  </xsl:choose>
 </xsl:template>

</xsl:stylesheet>
