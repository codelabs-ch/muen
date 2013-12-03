<?xml version="1.0" encoding="utf-8"?>

<xsl:stylesheet
	version="1.0"
	xmlns="http://www.w3.org/1999/xhtml"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns:html="http://www.w3.org/1999/xhtml"
	exclude-result-prefixes="html">

	<xsl:output
		method="xml"
		indent="yes"
		omit-xml-declaration="yes"
		doctype-public="-//W3C//DTD XHTML 1.1//EN"
		doctype-system="http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd"/>

	<xsl:template match="html:*">
		<xsl:element name="{local-name()}">
			<xsl:apply-templates select="@* | node()"/>
		</xsl:element>
	</xsl:template>

	<xsl:template match="node() | @*">
		<xsl:copy>
			<xsl:apply-templates select="node() | @*"/>
		</xsl:copy>
	</xsl:template>

	<xsl:template match="html:div[@class='toc_block']">
		<div class="toc_block">
			<xsl:call-template name="toc_entries"/>
		</div>
	</xsl:template>

	<xsl:template name="toc_entries">
		<div class="toc_entry">
			<a class="toc" href="index.html">Home</a>
		</div>
		<xsl:for-each select="//html:span[@class='anchor']">
			<div class="toc_entry">
				<a class="toc">
					<xsl:attribute name="href">
						<xsl:text>#</xsl:text>
						<xsl:value-of select="@id"/>
					</xsl:attribute>
					<xsl:value-of select="."/>
				</a>
			</div>
		</xsl:for-each>
	</xsl:template>

</xsl:stylesheet>
