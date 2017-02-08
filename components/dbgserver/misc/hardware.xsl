<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema">

	<xsl:output method="text" encoding="utf-8" indent="no"/>

	<xsl:strip-space elements="*"/>
	<xsl:template match="system/*"/>

	<xsl:template match="/system/subjects">
		<xsl:for-each select="subject">
			<xsl:if test="@name = 'dbgserver'">
				<xsl:call-template name="extractSerialPort"/>
			</xsl:if>
		</xsl:for-each>
	</xsl:template>

	<xsl:template name="packageBegin">
		<xsl:text>package </xsl:text>
		<xsl:value-of select="$PACKAGENAME"/>
		<xsl:text>&#10;</xsl:text>
		<xsl:text>is</xsl:text>
		<xsl:text>&#10;&#10;</xsl:text>
	</xsl:template>

	<xsl:template name="packageEnd">
		<xsl:text>&#10;&#10;</xsl:text>
		<xsl:text>end </xsl:text>
		<xsl:value-of select="$PACKAGENAME"/>
		<xsl:text>;&#10;</xsl:text>
	</xsl:template>

	<xsl:template name="extractSerialPort">
		<xsl:variable name="physDevName" select="devices/device[@logical='debugconsole']/@physical"/>
		<xsl:variable name="physPortName" select="devices/device[@logical='debugconsole']/ioPort/@physical"/>
		<xsl:variable name="physPortStart" select="/system/hardware/devices/device[@name=$physDevName]/ioPort[@name=$physPortName]/@start"/>
		<xsl:choose>
			<xsl:when test="not($physDevName) or not($physPortName) or not($physPortStart)">
				<xsl:message terminate="yes">Unable to extract debug console information</xsl:message>
			</xsl:when>
			<xsl:otherwise>
				<xsl:call-template name="packageBegin"/>
				<xsl:call-template name="constantName"/>
				<xsl:text> : constant := </xsl:text>
				<xsl:value-of select="$physPortStart"/>
				<xsl:text>;</xsl:text>
				<xsl:call-template name="packageEnd"/>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<xsl:template name="constantName">
		<xsl:text>   Debugconsole_Port</xsl:text>
	</xsl:template>

</xsl:stylesheet>
