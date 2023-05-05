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

with Mutools.Intervals;
use type Mutools.String_Vector.Vector;

with Interfaces;
use type Interfaces.Unsigned_64;

package Mutools.Vres_Alloc.Config
is
   --  This is the space all automatically assigned virtual addresses of
   --  native subjects are contained in.
   --  TODO: Potential improvement: Use platform properties for these values and
   --        pass as parameter at runtime.
   Default_Va_Space_Native : constant Intervals.Interval_Type
     := (First_Element => 16#0000_0001_0000_0000#,
         Last_Element  => 16#0000_0007_FFFF_FFFF#);

   --  This is the space all automatically assigned virtual addresses of
   --  virtual machine subjects (like linuxes) are contained in.
   --  There is a maxium number of bits available for hardware-addresses in
   --  linuxes which is less than 64 bits. At the time of writing it is 36 bits
   --  for linuxes running in Muen.
   Default_Va_Space_Vm : constant Intervals.Interval_Type
     := (First_Element => 16#0000_0001_0000_0000#,
         Last_Element  => 16#0000_0007_FFFF_FFFF#);

   --  These are the domains for the attributes 'vector' and 'event'
   --  of channel-readers and channel-writers, respectively.
   --  These are also used for arrays of channels with events/vectors.
   --  Low numbers are reserved for libraries.
   Default_Vector_Numbers_Domain : constant Intervals.Interval_Type
     := (First_Element => 64,
         Last_Element  => 255);
   Default_Event_Numbers_Domain : constant Intervals.Interval_Type
     := (First_Element => 16,
         Last_Element  => 63);

   -------------------------------------------------------------------------
   --  ATTENTION: If you change the values below you may have to adapt the
   --  code of mucfgcvresalloc, mucfgvresalloc and their unittests.
   -------------------------------------------------------------------------
   --  All XPaths in this section are read by Mucfgcvresalloc and
   --  interpreted relative to the root of the respective component.

   --  XPaths in Cspec where VirtualAddresses is read and written.
   C_Va_Alloc_Read_Write_Targets : constant String_Vector.Vector
     := String_Vector."&"
     ("requires/memory/memory",
      "requires/memory/array")
     & "requires/channels/reader"
     & "requires/channels/writer"
     & "requires/channels/array";

   --  XPaths in Cspec where VirtualAddresses is read in order to block parts
   --  of the VA-space from being used.
   C_Va_Alloc_Read_Only_Targets : constant String_Vector.Vector
     := "devices/device/memory"
     & "provides/memory/memory";

   --  XPaths in Cspec where 'vector'/'vectorBase' is read and written.
   C_Readers_Read_Write_Targets : constant String_Vector.Vector
     := String_Vector."&"
     ("requires/channels/reader[@vector]",
      "requires/channels/array[@vectorBase]");

   --  XPaths in Cspec where 'vector' is read.
   --  Attention: The values found on these paths are NOT used to set missing
   --  'vector' attributes. They are only blocked in the domain for channel
   --  reader IDs.
   C_Readers_Read_Only_Targets : constant String_Vector.Vector
     := String_Vector.To_Vector
     (New_Item => "requires/events/target/event/inject_interrupt/..",
      Length   => 1);

   --  XPaths in Cspec where 'event'/'eventBase' is read and written.
   C_Writers_Read_Write_Targets : constant String_Vector.Vector
     := String_Vector."&"
     ("requires/channels/writer[@event]",
      "requires/channels/array[@eventBase]");

   --  XPaths in Cspec where 'event' is read.
   --  Attention: The values found on these paths are NOT used to set missing
   --  'event' attributes. They are only blocked in the domain for channel
   --  writer IDs.
   C_Writers_Read_Only_Targets : constant String_Vector.Vector
     := String_Vector.To_Vector
     (New_Item => "requires/events/source/event",
      Length   => 1);

   -------------------------------------------------------------------------
   --  All XPath in this section are read by Mucfgvresalloc and interpreted
   --  relative to the subject-node under investigation.

   --  Note that all targets in the above section for Mucfgcvresalloc
   --  will be read (but not written) additionally and, if existent,
   --  interpreted relative to the "component"-node referenced in the current
   --  subject.

   --  XPaths in subject where VirtualAddresses is read and written.
   Va_Alloc_Read_Write_Targets : constant String_Vector.Vector
     := String_Vector."&"
     ("memory/memory",
      "channels/reader")
     & "channels/writer";

   --  XPaths in subject where VirtualAddresses is read in order to block parts
   --  of the VA-space from being used.
   Va_Alloc_Read_Only_Targets : constant String_Vector.Vector
     := String_Vector.Empty_Vector;

   --  XPaths in subject where 'vector' is read and written.
   Readers_Read_Write_Targets : constant String_Vector.Vector
     := String_Vector.To_Vector
     (New_Item => "channels/reader[@vector]",
      Length   => 1);

   --  XPaths in subject where 'vector' is read.
   --  Attention: The values found on these paths are NOT used to set missing
   --  'vector' attributes. They are only blocked in the domain for channel
   --  reader IDs.
   Readers_Read_Only_Targets : constant String_Vector.Vector
     := String_Vector.To_Vector
     (New_Item => "events/target/event/inject_interrupt/..",
      Length   => 1);

   --  XPaths in subject where 'event' is read and written.
   Writers_Read_Write_Targets : constant String_Vector.Vector
     := String_Vector.To_Vector
     (New_Item => "channels/writer[@event]",
      Length   => 1);

   --  XPaths in subject where 'event' is read.
   --  Attention: The values found on these paths are NOT used to set missing
   --  'event' attributes. They are only blocked in the domain for channel
   --  writer event IDs.
   Writers_Read_Only_Targets : constant String_Vector.Vector
     := String_Vector.To_Vector
     (New_Item => "events/source/group[@name='vmcall']/event",
      Length   => 1);

end Mutools.Vres_Alloc.Config;
