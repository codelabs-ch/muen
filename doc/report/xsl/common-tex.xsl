<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

    <xsl:template name="escapeChars">
        <xsl:param name="text"/>
        <xsl:if test="string-length($text) > 0">
            <xsl:variable name="char">
                <xsl:value-of select="substring($text,1,1)"/>
            </xsl:variable>
            <xsl:variable name="rest">
                <xsl:value-of select="substring($text,2)"/>
            </xsl:variable>
            <xsl:if test="$char = '%' or $char = '$' or $char = '#' or $char = '_' or $char = '{' or $char = '}' or $char = '~'  or $char = '^'  or $char = '\'">
                <xsl:text>\</xsl:text>
            </xsl:if>
            <xsl:value-of select="$char"/>
            <xsl:call-template name="escapeChars">
                <xsl:with-param name="text" select="$rest"/>
            </xsl:call-template>
        </xsl:if>
    </xsl:template>

    <xsl:template name="texifyText">
        <xsl:param name="text"/>
        <xsl:variable name="output">
            <xsl:call-template name="escapeChars">
                <xsl:with-param name="text" select="$text"/>
            </xsl:call-template>
        </xsl:variable>
        <xsl:value-of select="normalize-space($output)"/>
    </xsl:template>

</xsl:stylesheet>
