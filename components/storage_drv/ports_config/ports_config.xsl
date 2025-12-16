<?xml version="1.0" encoding="UTF-8"?>
<xsl:transform
 version="1.0"
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
 <xsl:output method="text" encoding="UTF-8" indent="yes" omit-xml-declaration="yes"/>
 <xsl:template match="/system/config">
  <xsl:variable name="slots" select="integer[@name='SLOTS']/@value"/>
  <xsl:variable name="_ahci_port" select="integer[@name='AHCI_PORT']/@value"/>
   <xsl:variable name="ahci_port">
    <xsl:choose>
     <xsl:when test="not($_ahci_port)">
      <xsl:text>0</xsl:text>
     </xsl:when>
     <xsl:otherwise>
      <xsl:value-of select="$_ahci_port"/>
     </xsl:otherwise>
   </xsl:choose>
  </xsl:variable>
  <xsl:variable name="ports">
   <xsl:choose>
    <xsl:when test="$slots &gt; 2">
     <xsl:value-of select="3"/>
    </xsl:when>
    <xsl:otherwise>
     <xsl:value-of select="$slots+1"/>
    </xsl:otherwise>
   </xsl:choose>
  </xsl:variable>
with Ahci;
with Storage_Drv_Component.Channel_Arrays;

package Ports_Config is
   --  Use No_Partition if you want to export the whole Device attached
   --  to the given AHCI_Port. Smart_Only will not assign a partition to the
   --  device and allow only the Get_SMART request
   No_Partition   : Integer := Natural'Last;
   Null_Partition : Integer := 16#cafe#;
   Smart_Only     : Integer := 16#beef#;

   --  Maximum zero based device index of devices attached to a server port
   Devices_Max : constant := 2;

   --  Number of used Server Ports
   Ports_Max : constant := <xsl:value-of select="$ports"/>;

   package CSpecs renames Storage_Drv_Component.Channel_Arrays;
   --  FIXME: add a compile time check to make sure that all counts are equal!
   type Channel_Range is range 0 .. CSpecs.Blockdev_Request_Element_Count - 1;

   --  Defines a exported device by describing it's Ahci_Port and the
   --  zero based partition Number
   type Device_Type is record
      Ahci_Port : Ports_Config.Port_Range;
      Partition : Integer;
   end record;

   Null_Device : Device_Type :=
      (Ahci_Port => 0,
      Partition => Null_Partition);

   type Devices_Range is range 0 .. Devices_Max;
   type Devices_Array_Type is array (Devices_Range) of Device_Type;

   --  Defines a Server Port. Chan_Idx is used as offset in the
   --  Request/Response Channel Arrays
   type Port_Config_Type is record
      Chan_Idx : Channel_Range;
      Devices  : Devices_Array_Type;
   end record;

   type Ports_Array_Range is range 1 .. Ports_Max;
   type Port_Config_Array_Type is array (Ports_Array_Range)
      of Port_Config_Type;

   --  nuc: 0
   --  qemu: 1
   Device_ID : constant Ports_Config.Port_Range := <xsl:value-of select="$ahci_port"/>;

   --  configuration of the server ports
   Port_Config : constant Port_Config_Array_Type :=
      (1 => Port_Config_Type'(
          Chan_Idx => 0,
          Devices  => (
             0 => Device_Type'(Ahci_Port => Device_ID, Partition => 2),
             1 => Device_Type'(Ahci_Port => Device_ID, Partition => 3),
             2 => Device_Type'(Ahci_Port => Device_ID, Partition => 4))),
       2 => Port_Config_Type'(
          Chan_Idx => 1,
          Devices => (others => Device_Type'(Ahci_Port => Device_ID,
             Partition => Smart_Only)))<xsl:choose><xsl:when test="$ports='2'">);
</xsl:when>
<xsl:otherwise>,
       3 => Port_Config_Type'(
          Chan_Idx => 2,
          Devices  => (
             0 => Device_Type'(Ahci_Port => Device_ID, Partition => 2),
             1 => Device_Type'(Ahci_Port => Device_ID, Partition => 3),
             2 => Device_Type'(Ahci_Port => Device_ID, Partition => 4))));
</xsl:otherwise>
</xsl:choose>
end Ports_Config;
</xsl:template>
 <xsl:template match="@*|node()">
  <xsl:apply-templates/>
 </xsl:template>
</xsl:transform>
