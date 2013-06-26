<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema">

	<xsl:import href="common-tex.xsl"/>
	<xsl:output method="text" encoding="utf-8" indent="no"/>

	<xsl:strip-space elements="*"/>
	<xsl:template match="xs:schema/*"/>

	<xsl:template match="xs:schema/xs:simpleType|xs:schema/xs:complexType">
		<xsl:text>&#10;\subsubsection{</xsl:text>
		<xsl:call-template name="texifyText">
			<xsl:with-param name="text" select="@name"/>
		</xsl:call-template>
		<xsl:text>}&#10;\begin{figure}[!h]\centering</xsl:text>
		<xsl:text>	\includegraphics[scale=0.48]{images/xml_</xsl:text>
		<xsl:value-of select="@name"/>
		<xsl:text>.png}&#10;</xsl:text>
		<xsl:text>	\caption{</xsl:text>
		<xsl:call-template name="texifyText">
			<xsl:with-param name="text" select="@name"/>
		</xsl:call-template>
		<xsl:text>}&#10;\end{figure}&#10;</xsl:text>
		<xsl:call-template name="texifyText">
			<xsl:with-param name="text" select="xs:annotation/xs:documentation/text()"/>
		</xsl:call-template>
		<xsl:text>&#10;</xsl:text>
	</xsl:template>

</xsl:stylesheet>
