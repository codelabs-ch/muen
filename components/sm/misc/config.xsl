<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema">

	<xsl:output method="text" encoding="utf-8" indent="no"/>

	<xsl:strip-space elements="*"/>
	<xsl:template match="text()"/>
	<xsl:template match="/">
		<xsl:if test="count(/system/subjects/subject/component[@ref=$COMPONENTNAME])='0'">
			<xsl:message terminate="yes">No <xsl:value-of select="$COMPONENTNAME"/> component reference in policy</xsl:message>
		</xsl:if>
		<xsl:apply-templates/>
	</xsl:template>

	<xsl:template match="/">
		<xsl:call-template name="configHeader"/>
		<xsl:call-template name="extractGlobalConfig"/>
		<xsl:call-template name="configFooter"/>
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

</xsl:stylesheet>
