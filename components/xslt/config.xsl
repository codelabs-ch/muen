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

	<xsl:template name="extractLogChannelSize">
		<xsl:variable name="configSize" select="/system/config/string[@name='logchannel_size']/@value"/>
		<xsl:variable name="logChannelSize">
			<xsl:choose>
				<xsl:when test="$configSize!=''">
					<xsl:value-of select="$configSize"/>
				</xsl:when>
				<xsl:otherwise>
					<xsl:message terminate="yes">Unable to extract logchannel size, logchannel_size config variable missing</xsl:message>
				</xsl:otherwise>
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
