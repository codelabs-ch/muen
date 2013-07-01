<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema">

	<xsl:import href="common-tex.xsl"/>
	<xsl:output method="text" encoding="utf-8" indent="no"/>

	<xsl:strip-space elements="*"/>
	<xsl:template match="xs:schema/*"/>

	<xsl:template match="xs:schema/xs:include">
		<xsl:if test="not(@schemaLocation='./types.xsd')">
			<xsl:apply-templates select="document(concat($SCHEMADIR,'/',@schemaLocation))"/>
		</xsl:if>
	</xsl:template>

	<xsl:template match="xs:schema/xs:simpleType|xs:schema/xs:complexType">
		<xsl:text>&#10;\subsubsection{</xsl:text>
		<xsl:call-template name="texifyText">
			<xsl:with-param name="text" select="@name"/>
		</xsl:call-template>
		<xsl:text>}\label{subsec:</xsl:text>
		<xsl:value-of select="@name"/>
		<xsl:text>}&#10;\begin{</xsl:text>
		<xsl:choose>
			<xsl:when test="contains($LARGEGRAPHS,@name)">
				<xsl:text>sidewaysfigure}[hp]\centering&#10;\includegraphics[width=\textwidth]</xsl:text>
			</xsl:when>
			<xsl:otherwise>
				<xsl:text>figure}[!h]\centering&#10;\includegraphics[scale=0.7]</xsl:text>
			</xsl:otherwise>
		</xsl:choose>
		<xsl:text>{images/xml_</xsl:text>
		<xsl:value-of select="@name"/>
		<xsl:text>}&#10;</xsl:text>
		<xsl:text>\end{</xsl:text>
		<xsl:if test="contains($LARGEGRAPHS,@name)">
			<xsl:text>sideways</xsl:text>
		</xsl:if>
		<xsl:text>figure}&#10;</xsl:text>
		<xsl:call-template name="texifyText">
			<xsl:with-param name="text" select="xs:annotation/xs:documentation/text()"/>
		</xsl:call-template>
		<xsl:text>&#10;</xsl:text>
	</xsl:template>

</xsl:stylesheet>
