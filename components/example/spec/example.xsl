<?xml version="1.0" encoding="UTF-8"?>
 <xsl:transform
  version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="xml" encoding="UTF-8" indent="yes" omit-xml-declaration="yes"/>
  <xsl:template match="/system/config">
   <xsl:variable name="ahci_drv_enabled" select="boolean[@name='ahci_drv_active']/@value"/>

<component name="example" profile="native">
 <config>
  <boolean name="ahci_drv_enabled" value="{$ahci_drv_enabled}"/>
  <boolean name="print_serial" value="false"/>
  <boolean name="print_vcpu_speed" value="true"/>
  <integer name="serial" value="123456789"/>
  <string name="greeter" value="Subject running"/>
 </config>
 <depends>
  <library ref="libmudebuglog"/>
  <library ref="muinit"/>
 </depends>
 <requires>
  <memory>
   <if variable="ahci_drv_enabled" value="true">
    <memory executable="false" logical="blockdev_shm2" size="16#0100_0000#" virtualAddress="16#a100_0000#" writable="true"/>
   </if>
  </memory>
  <channels>
   <reader logical="example_request" size="16#1000#" virtualAddress="16#000A_0000#"/>
   <writer logical="example_response" size="16#1000#" virtualAddress="16#000B_0000#" event="1"/>
   <if variable="ahci_drv_enabled" value="true">
    <writer logical="blockdev_request2"  size="16#0000_8000#" virtualAddress="16#0030_0000#" event="51"/>
    <reader logical="blockdev_response2" size="16#0000_4000#" virtualAddress="16#0040_0000#" vector="38"/>
   </if>
  </channels>
  <events>
   <source>
    <event id="2" logical="yield"/>
    <event id="3" logical="timer"/>
   </source>
   <target>
    <event logical="inject_timer">
     <inject_interrupt vector="37"/>
    </event>
   </target>
  </events>
 </requires>
</component>
  </xsl:template>
  <xsl:template match="@*|node()">
   <xsl:apply-templates/>
  </xsl:template>
 </xsl:transform>
