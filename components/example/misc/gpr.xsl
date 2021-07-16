<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema">

 <xsl:output method="text" encoding="utf-8" indent="no"/>

 <xsl:include href="../../xslt/gpr_config.xsl"/>

 <xsl:strip-space elements="*"/>
 <xsl:template match="system/*"/>

 <xsl:template match="/system/config">
  <xsl:variable name="blkAHCIDrvEnabled" select="boolean[@name='ahci_drv_active']/@value"/>
  <xsl:variable name="blkWriteTestValue" select="boolean[@name='example_blk_write']/@value"/>
  <xsl:variable name="blkWriteTest">
   <xsl:choose>
    <xsl:when test="not($blkWriteTestValue)">
     <xsl:text>false</xsl:text>
    </xsl:when>
    <xsl:otherwise>
     <xsl:value-of select="$blkWriteTestValue"/>
    </xsl:otherwise>
   </xsl:choose>
  </xsl:variable>
  <xsl:call-template name="gprHeader"/>
  <xsl:call-template name="booleanType"/>
  <xsl:call-template name="configBooleanWithParams">
   <xsl:with-param name="name" select="'blk_write_test'"/>
   <xsl:with-param name="value" select="$blkWriteTest"/>
  </xsl:call-template>
  <xsl:call-template name="configBooleanWithParams">
   <xsl:with-param name="name" select="'ahci_drv_enabled'"/>
   <xsl:with-param name="value" select="$blkAHCIDrvEnabled"/>
  </xsl:call-template>
  <xsl:call-template name="gprFooter"/>
 </xsl:template>

</xsl:stylesheet>
