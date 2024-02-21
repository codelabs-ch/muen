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
  <vcpu>
   <vmx>
    <masks>
     <exception>
      <Breakpoint>0</Breakpoint>
     </exception>
    </masks>
   </vmx>
   <registers>
    <cr4>
     <XSAVEEnable>1</XSAVEEnable>
    </cr4>
   </registers>
  </vcpu>
  <memory>
   <if variable="ahci_drv_enabled" value="true">
    <memory executable="false" logical="blockdev_shm2" size="16#0100_0000#" writable="true"/>
   </if>
   <memory executable="false" logical="filled_region" size="16#1000#" writable="true"/>
  </memory>
  <channels>
   <reader logical="example_request" size="16#1000#" vector="auto"/>
   <writer logical="example_response" size="16#1000#" event="auto"/>
   <if variable="ahci_drv_enabled" value="true">
    <writer logical="blockdev_request2"  size="16#0000_8000#" event="auto"/>
    <reader logical="blockdev_response2" size="16#0000_4000#" vector="auto"/>
   </if>
  </channels>
  <events>
   <source>
    <event id="2" logical="yield">
     <subject_yield/>
    </event>
    <event id="3" logical="timer"/>
    <event id="4" logical="sleep">
     <subject_sleep/>
    </event>
   </source>
   <target>
    <event logical="inject_timer">
     <inject_interrupt vector="37"/>
    </event>
   </target>
  </events>
 </requires>
 <provides>
  <memory executable="false" logical="interrupt_stack" size="16#2000#" type="subject_binary" virtualAddress="16#0001_0000#" writable="true">
   <fill pattern="16#00#"/>
  </memory>
 </provides>
</component>
  </xsl:template>
  <xsl:template match="@*|node()">
   <xsl:apply-templates/>
  </xsl:template>
 </xsl:transform>
