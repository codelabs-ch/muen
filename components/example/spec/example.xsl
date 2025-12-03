<?xml version="1.0" encoding="UTF-8"?>
 <xsl:transform
  version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="xml" encoding="UTF-8" indent="yes" omit-xml-declaration="yes"/>
  <xsl:template match="/system/config">
   <xsl:variable name="ahci_drv_enabled" select="boolean[@name='ahci_drv_active']/@value"/>
   <xsl:variable name="nvme_drv_enabled" select="boolean[@name='nvme_drv_active']/@value"/>

<component name="example" profile="native">
 <config>
  <boolean name="ahci_drv_enabled" value="{$ahci_drv_enabled}"/>
  <boolean name="nvme_drv_enabled" value="{$nvme_drv_enabled}"/>
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
   <!-- <if variable="ahci_drv_enabled" value="true">
    <memory executable="false" logical="blockdev_AHCI_shm2" size="16#0100_0000#" writable="true"/>
   </if>
   <if variable="nvme_drv_enabled" value="true">
    <memory executable="false" logical="blockdev_NVME_shm2" size="16#0100_0000#" writable="true"/>
   </if> -->
   <array logical="blockdev_shm2" virtualAddressBase="16#0100_0000#" elementSize="16#0100_0000#"  writable="true" executable="false">
    <if variable="ahci_drv_enabled" value="true">
     <memory logical="blockdev_AHCI_shm2"/>
    </if>
    <if variable="nvme_drv_enabled" value="true">
     <memory logical="blockdev_NVME_shm2"/>
    </if>
   </array>

   <memory executable="false" logical="filled_region" size="16#1000#" writable="true"/>
  </memory>
  <channels>
   <reader logical="example_request" size="16#1000#" vector="auto"/>
   <writer logical="example_response" size="16#1000#" event="auto"/>
   <!-- <if variable="ahci_drv_enabled" value="true">
    <writer logical="blockdev_request2_ahci"  size="16#0000_8000#" event="auto"/>
    <reader logical="blockdev_response2_ahci" size="16#0000_4000#" vector="auto"/>
   </if>
   <if variable="nvme_drv_enabled" value="true">
    <writer logical="blockdev_request2_nvme"  size="16#0000_8000#" event="auto"/>
    <reader logical="blockdev_response2_nvme" size="16#0000_4000#" vector="auto"/>
   </if> -->

   <array logical="blockdev_response2" virtualAddressBase="16#00a0_0000#" elementSize="16#4000#" vectorBase="42">
    <if variable="ahci_drv_enabled" value="true">
     <reader logical="blockdev_response2_ahci"/>
    </if>
    <if variable="nvme_drv_enabled" value="true">
     <reader logical="blockdev_response2_nvme"/>
    </if>
   </array>
   <array logical="blockdev_request2" virtualAddressBase="16#00b0_0000#" elementSize="16#8000#" eventBase="10">
    <if variable="ahci_drv_enabled" value="true">
     <writer logical="blockdev_request2_ahci"/>
    </if>
    <if variable="nvme_drv_enabled" value="true">
     <writer logical="blockdev_request2_nvme"/>
    </if>
   </array>

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
