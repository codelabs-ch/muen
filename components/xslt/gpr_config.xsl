<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema">

	<xsl:template name="gprHeader">
		<xsl:text>abstract project </xsl:text>
		<xsl:value-of select="$GPRNAME"/>
		<xsl:text>&#10;</xsl:text>
		<xsl:text>is</xsl:text>
		<xsl:text>&#10;&#10;</xsl:text>
	</xsl:template>

	<xsl:template name="gprFooter">
		<xsl:text>&#10;</xsl:text>
		<xsl:text>end </xsl:text>
		<xsl:value-of select="$GPRNAME"/>
		<xsl:text>;&#10;</xsl:text>
	</xsl:template>

	<xsl:template name="booleanType">
		<xsl:text>   type Boolean is ("true", "false");</xsl:text>
		<xsl:text>&#10;&#10;</xsl:text>
	</xsl:template>

	<xsl:template name="configBoolean">
		<xsl:variable name="name"  select="@name"/>
		<xsl:variable name="value" select="@value"/>
		<xsl:text>   </xsl:text>
		<xsl:value-of select="$name"/>
		<xsl:text> : Boolean := "</xsl:text>
		<xsl:value-of select="$value"/>
		<xsl:text>";&#10;</xsl:text>
	</xsl:template>

</xsl:stylesheet>
