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

	<xsl:template match="/system/subjects/subject/component">
		<xsl:if test="@ref=$COMPONENTNAME">
			<xsl:text>&lt;include&gt;&#10;</xsl:text>
			<xsl:call-template name="extractLogSinks"/>
			<xsl:if test="/system/config/boolean[@name='dbgserver_sink_serial']/@value='true'">
				<xsl:call-template name="extractSerialPort"/>
			</xsl:if>
			<xsl:call-template name="extractLogChannelSize"/>
			<xsl:text>&lt;/include&gt;&#10;</xsl:text>
		</xsl:if>
	</xsl:template>

	<xsl:template name="resolveDeviceAlias">
		<xsl:param name="aliasName"/>
		<xsl:value-of select="/system/platform/mappings/aliases/alias[@name=$aliasName]/@physical"/>
	</xsl:template>

	<xsl:template name="resolveDeviceAliasResource">
		<xsl:param name="aliasDevName"/>
		<xsl:param name="aliasResourceName"/>
		<xsl:value-of select="/system/platform/mappings/aliases/alias[@name=$aliasDevName]/resource[@name=$aliasResourceName]/@physical"/>
	</xsl:template>

	<xsl:template name="extractSerialPort">
		<xsl:variable name="physDevName" select="map[@logical='debugconsole']/@physical"/>
		<xsl:variable name="physPortName" select="map[@logical='debugconsole']/map/@physical"/>
		<xsl:variable name="aliasPhysDevName">
			<xsl:call-template name="resolveDeviceAlias">
				<xsl:with-param name="aliasName" select="$physDevName"/>
			</xsl:call-template>
		</xsl:variable>
		<xsl:variable name="aliasPhysResourceName">
			<xsl:call-template name="resolveDeviceAliasResource">
				<xsl:with-param name="aliasDevName" select="$physDevName"/>
				<xsl:with-param name="aliasResourceName" select="$physPortName"/>
			</xsl:call-template>
		</xsl:variable>
		<xsl:variable name="physPortStart">
			<xsl:choose>
				<xsl:when test="$aliasPhysDevName!='' and $aliasPhysResourceName!=''">
					<xsl:value-of select="/system/hardware/devices/device[@name=$aliasPhysDevName]/ioPort[@name=$aliasPhysResourceName]/@start"/>
				</xsl:when>
				<xsl:otherwise>
					<xsl:value-of select="/system/hardware/devices/device[@name=$physDevName]/ioPort[@name=$physPortName]/@start"/>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:variable>
		<xsl:variable name="physPortEnd">
			<xsl:choose>
				<xsl:when test="$aliasPhysDevName!='' and $aliasPhysResourceName!=''">
					<xsl:value-of select="/system/hardware/devices/device[@name=$aliasPhysDevName]/ioPort[@name=$aliasPhysResourceName]/@end"/>
				</xsl:when>
				<xsl:otherwise>
					<xsl:value-of select="/system/hardware/devices/device[@name=$physDevName]/ioPort[@name=$physPortName]/@end"/>
				</xsl:otherwise>
			</xsl:choose>
		</xsl:variable>
		<xsl:choose>
			<xsl:when test="$physPortStart='' or $physPortEnd=''">
				<xsl:message terminate="yes">Unable to extract debug console information</xsl:message>
			</xsl:when>
			<xsl:otherwise>
				<xsl:call-template name="configString">
					<xsl:with-param name="name" select="'debugconsole_port_start'"/>
					<xsl:with-param name="value" select="$physPortStart"/>
				</xsl:call-template>
				<xsl:text>&#10;</xsl:text>
				<xsl:call-template name="configString">
					<xsl:with-param name="name" select="'debugconsole_port_end'"/>
					<xsl:with-param name="value" select="$physPortEnd"/>
				</xsl:call-template>
				<xsl:text>&#10;</xsl:text>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<xsl:template name="extractLogSinks">
		<xsl:for-each select="/system/config/boolean">
			<xsl:if test="starts-with(@name, 'dbgserver_sink')">
				<xsl:call-template name="configBoolean">
					<xsl:with-param name="name" select="substring(@name,11)"/>
					<xsl:with-param name="value" select="@value"/>
				</xsl:call-template>
				<xsl:text>&#10;</xsl:text>
			</xsl:if>
		</xsl:for-each>
	</xsl:template>

</xsl:stylesheet>
