--
--  Copyright (C) 2023 secunet Security Networks AG
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with Alloc.Map;
with Mutools;
use type Mutools.String_Vector.Vector;

package Alloc.Config
is
   --  This is the space all automatically assigned virtual addresses are
   --  contained in.
   --  TODO: Improvement: Use platform properties for these values and pass
   --        as parameter at runtime
   Default_Va_Space_Native : constant Alloc.Map.Memory_Interval_Type
     := (First_Address => 16#0000_0000_2000_0000#,
         Last_Address  => 16#0000_0007_FFFF_FFFF#);

   Default_Va_Space_Vm : constant Alloc.Map.Memory_Interval_Type
     := (First_Address => 16#0000_0010_0000_0000#,
         Last_Address  => 16#0000_001F_FFFF_FFFF#);

   --  These are the domains for the attributes 'vector' and 'event'
   --  of channel-readers and channel-writers respectively
   Vector_Numbers_Domain : constant Alloc.Map.Memory_Interval_Type
     := (First_Address => 0,
         Last_Address  => 255);
   Event_Numbers_Domain : constant Alloc.Map.Memory_Interval_Type
     := (First_Address => 0,
         Last_Address  => 63);

   --  This prefix is used for the xpaths of all targets of mucfgcresalloc.
   C_Res_Alloc_Target_Prefix : constant String
     := "/component";

   --  This is the list of XPaths where Comp_Va_alloc may assign virtual
   --  addresses.
   --  The paths are relative to the root of the respective component.
   C_Va_Alloc_Read_Write_Targets : constant Mutools.String_Vector.Vector
     := Mutools.String_Vector."&"
     ("requires/memory/memory",
      "requires/memory/array")
     & "requires/channels/reader"
     & "requires/channels/writer"
     & "requires/channels/array";

   --  This is the list of XPaths that Comp_Va_alloc additionally reads in order
   --  to block parts of the VA-space from beeing used.
   --  The paths are relative to the root of the respective component.
   C_Va_Alloc_Read_Only_Targets : constant Mutools.String_Vector.Vector
     := "devices/device/memory"
     & "provides/memory/memory";

   --  This is the list of XPaths where Comp_Res_Alloc may assign the 'vector'
   --  attribute.
   --  The paths are relative to the root of the respective component.
   C_Readers_Read_Write_Targets : constant Mutools.String_Vector.Vector
     := Mutools.String_Vector."&"
     ("requires/channels/reader[@vector]",
      "requires/channels/array[@vectorBase]");

   --  WICHTIG: die Elemente werden nur excluded, aber NIE benutzt um
   --  den channel die Adresse zu geben.
   --  Denn: wenn das hier per Hand geschrieben wird, muss auch der Channel
   --  per Hand geschrieben werden!
   C_Readers_Read_Only_Targets : constant Mutools.String_Vector.Vector
     := Mutools.String_Vector.To_Vector
     (New_Item => "requires/events/target/event",
      Length   => 1);

   --  This is the list of XPaths where Comp_Res_Alloc may assign the
   --  'event'/'id' attribute.
   --  The paths are relative to the root of the respective component.
   C_Writers_Read_Write_Targets : constant Mutools.String_Vector.Vector
     := Mutools.String_Vector."&"
     ("requires/channels/writer[@event]",
      "requires/channels/array[@eventBase]");
   C_Writers_Read_Only_Targets : constant Mutools.String_Vector.Vector
     := Mutools.String_Vector.To_Vector
     (New_Item => "requires/events/source/event",
      Length   => 1);

   --  This is like C_Va_Alloc_Read_Write_Targets but for Va_alloc and
   --  relative to the root node of the respective subject.
   --  Note that all targets of C_Va_Alloc_Read_Write_Targets and
   --  C_Va_Alloc_Read_Only_Targets will be read additionally (but not written),
   --  but relative to the matching "component"-node (not within the subject).
   Va_Alloc_Read_Write_Targets : constant Mutools.String_Vector.Vector
     := Mutools.String_Vector."&"
     ("memory/memory",
      "channels/reader")
     & "channels/writer";
   Va_Alloc_Read_Only_Targets : constant Mutools.String_Vector.Vector
     := Mutools.String_Vector.Empty_Vector;

   --  This is like  C_Event_Target_Read_Write_Targets but for Res_Alloc and
   --  relative to the root node of the respective subject.
   Readers_Read_Write_Targets : constant Mutools.String_Vector.Vector
     := Mutools.String_Vector.To_Vector
     (New_Item => "channels/reader[@vector]",
      Length   => 1);
   Readers_Read_Only_Targets : constant Mutools.String_Vector.Vector
     := Mutools.String_Vector.To_Vector
     (New_Item => "events/target/event",
      Length   => 1);

   --  This is like  C_Event_Source_Read_Write_Targets but for Res_Alloc and
   --  relative to the root node of the respective subject.
   Writers_Read_Write_Targets : constant Mutools.String_Vector.Vector
     := Mutools.String_Vector.To_Vector
     (New_Item => "channels/writer[@event]",
      Length   => 1);
   Writers_Read_Only_Targets : constant Mutools.String_Vector.Vector
     := Mutools.String_Vector.To_Vector
     (New_Item => "events/source/group[@name='vmcall']/event",
      Length   => 1);

end Alloc.Config;
