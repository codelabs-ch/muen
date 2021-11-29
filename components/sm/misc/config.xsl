<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema">

	<xsl:output method="text" encoding="utf-8" indent="no"/>

	<xsl:include href="../../xslt/config.xsl"/>

	<xsl:strip-space elements="*"/>
	<xsl:template match="text()"/>
	<xsl:template match="/">
		<xsl:call-template name="checkComponentReference"/>
		<xsl:apply-templates/>
	</xsl:template>

	<xsl:template match="/">
		<xsl:text>&lt;include&gt;&#10;</xsl:text>
		<xsl:call-template name="extractGlobalConfig"/>
		<xsl:text>&lt;/include&gt;&#10;</xsl:text>
	</xsl:template>

	<xsl:template name="extractGlobalConfig">
		<xsl:for-each select="/system/config/boolean">
			<xsl:if test="starts-with(@name, 'pciconf_emulation_enabled')">
				<xsl:call-template name="configBoolean">
					<xsl:with-param name="name" select="@name"/>
					<xsl:with-param name="value" select="@value"/>
				</xsl:call-template>
				<xsl:text>&#10;</xsl:text>
			</xsl:if>
		</xsl:for-each>
		<xsl:call-template name="extractConfigBoolean">
			<xsl:with-param name="name" select="'sm_announce_xsave'"/>
			<xsl:with-param name="defaultValue" select="'false'"/>
		</xsl:call-template>
		<xsl:text>&#10;</xsl:text>
	</xsl:template>

</xsl:stylesheet>
