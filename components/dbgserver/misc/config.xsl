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

	<xsl:template match="/system/subjects/subject/component">
		<xsl:if test="@ref=$COMPONENTNAME">
			<xsl:call-template name="configHeader"/>
			<xsl:call-template name="extractLogSinks"/>
			<xsl:if test="/system/config/boolean[@name='dbgserver_sink_serial']/@value='true'">
				<xsl:call-template name="extractSerialPort"/>
			</xsl:if>
			<xsl:call-template name="extractLogChannelSize"/>
			<xsl:call-template name="configFooter"/>
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

	<xsl:template name="extractLogChannelSize">
		<xsl:variable name="physName" select="/system/subjects/subject/component[@ref=$COMPONENTNAME]/map[starts-with(@logical,'log_')][1]/@physical"/>
		<xsl:variable name="physSize" select="/system/channels/channel[@name=$physName]/@size"/>
		<xsl:variable name="logChannelSize">
			<xsl:choose>
				<xsl:when test="$physSize!=''">
					<xsl:value-of select="$physSize"/>
				</xsl:when>
				<xsl:otherwise>16&#35;0000&#35;</xsl:otherwise>
			</xsl:choose>
		</xsl:variable>
		<xsl:call-template name="configString">
			<xsl:with-param name="name" select="'logchannel_size'"/>
			<xsl:with-param name="value" select="$logChannelSize"/>
		</xsl:call-template>
		<xsl:text>&#10;</xsl:text>
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
					<xsl:with-param name="name" select="@name"/>
					<xsl:with-param name="value" select="@value"/>
				</xsl:call-template>
				<xsl:text>&#10;</xsl:text>
			</xsl:if>
		</xsl:for-each>
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

</xsl:stylesheet>
