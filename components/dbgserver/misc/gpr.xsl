<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema">

	<xsl:output method="text" encoding="utf-8" indent="no"/>

	<xsl:strip-space elements="*"/>
	<xsl:template match="system/*"/>

	<xsl:template match="/system/config">
		<xsl:call-template name="gprHeader"/>
		<xsl:call-template name="booleanType"/>
		<xsl:for-each select="boolean">
			<xsl:if test="starts-with(@name, 'dbgserver_sink')">
				<xsl:call-template name="extractSink"/>
			</xsl:if>
		</xsl:for-each>
		<xsl:call-template name="gprFooter"/>
	</xsl:template>

	<xsl:template name="gprHeader">
		<xsl:text>abstract project </xsl:text>
		<xsl:value-of select="$GPRNAME"/>
		<xsl:text>&#10;</xsl:text>
		<xsl:text>is</xsl:text>
		<xsl:text>&#10;&#10;</xsl:text>
	</xsl:template>

	<xsl:template name="gprFooter">
		<xsl:text>&#10;&#10;</xsl:text>
		<xsl:text>end </xsl:text>
		<xsl:value-of select="$GPRNAME"/>
		<xsl:text>;&#10;</xsl:text>
	</xsl:template>

	<xsl:template name="booleanType">
		<xsl:text>   type Boolean is ("true", "false");</xsl:text>
		<xsl:text>&#10;&#10;</xsl:text>
	</xsl:template>

	<xsl:template name="extractSink">
		<xsl:variable name="sinkName"  select="@name"/>
		<xsl:variable name="sinkValue" select="@value"/>
		<xsl:text>   </xsl:text>
		<xsl:value-of select="$sinkName"/>
		<xsl:text> : Boolean := "</xsl:text>
		<xsl:value-of select="$sinkValue"/>
		<xsl:text>";&#10;</xsl:text>
	</xsl:template>

</xsl:stylesheet>
