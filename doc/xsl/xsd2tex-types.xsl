<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema">

	<xsl:import href="common-tex.xsl"/>
	<xsl:output method="text" encoding="utf-8" indent="no"/>

	<xsl:strip-space elements="*"/>
	<xsl:template match="xs:schema">
		<xsl:text>\begin{description}</xsl:text>
		<xsl:for-each select="xs:simpleType|xs:complexType">
			<xsl:call-template name="typeDescription"/>
		</xsl:for-each>
		<xsl:text>\end{description}</xsl:text>
	</xsl:template>

	<xsl:template name="typeDescription">
		<xsl:text>\item[</xsl:text>
		<xsl:call-template name="texifyText">
			<xsl:with-param name="text" select="@name"/>
		</xsl:call-template>
		<xsl:text>] \hfill \\&#10;</xsl:text>
		<xsl:call-template name="texifyText">
			<xsl:with-param name="text" select="xs:annotation/xs:documentation/text()"/>
		</xsl:call-template>
		<xsl:text>&#10;</xsl:text>
	</xsl:template>

</xsl:stylesheet>
