<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema">

	<xsl:output method="text" encoding="utf-8" indent="no"/>

	<xsl:include href="../../xslt/config.xsl"/>

	<xsl:strip-space elements="*"/>
	<xsl:template match="text()"/>
	<xsl:template match="/">
		<xsl:call-template name="configHeader"/>
		<xsl:call-template name="extractLogChannelSize"/>
		<xsl:call-template name="configFooter"/>
	</xsl:template>

	<xsl:template name="extractLogChannelSize">
		<xsl:variable name="configSize" select="/system/config/string[@name='logchannel_size']/@value"/>
		<xsl:variable name="logChannelSize">
			<xsl:choose>
				<xsl:when test="$configSize!=''">
					<xsl:value-of select="$configSize"/>
				</xsl:when>
				<xsl:otherwise>16&#35;0001_0000&#35;</xsl:otherwise>
			</xsl:choose>
		</xsl:variable>
		<xsl:text>&#10;</xsl:text>
		<xsl:call-template name="configString">
			<xsl:with-param name="name" select="'logchannel_size'"/>
			<xsl:with-param name="value" select="$logChannelSize"/>
		</xsl:call-template>
		<xsl:text>&#10;</xsl:text>
	</xsl:template>

</xsl:stylesheet>
