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

</xsl:stylesheet>
