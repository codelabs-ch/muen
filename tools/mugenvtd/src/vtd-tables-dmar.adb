--
--  Copyright (C) 2014, 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014, 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Unchecked_Conversion;

package body VTd.Tables.DMAR
is

   subtype Table_Stream is Ada.Streams.Stream_Element_Array
     (1 .. Mutools.Constants.Page_Size);

   -------------------------------------------------------------------------

   procedure Add_Entry
     (RT  : in out Root_Table_Type;
      Bus :        Table_Index_Type;
      CTP :        Table_Pointer_Type)
   is
   begin
      RT.Entries (Bus).Present := 1;
      RT.Entries (Bus).CTP     := Aligned_Pointer_Type (CTP / 2 ** 12);
   end Add_Entry;

   -------------------------------------------------------------------------

   procedure Add_Entry
     (CT        : in out Context_Table_Type;
      Device    :        Mutools.PCI.Device_Range;
      Func      :        Mutools.PCI.Function_Range;
      Domain    :        Domain_Range;
      PT_Levels :        Paging_Level;
      SLPTPTR   :        Table_Pointer_Type)
   is
      Idx : constant Table_Index_Type
        := Table_Index_Type (Device) * 8 + Table_Index_Type (Func);
   begin
      if PT_Levels = 4 then
         CT.Entries (Idx).AW := AGAW_48_Bit;
      elsif PT_Levels = 3 then
         CT.Entries (Idx).AW := AGAW_39_Bit;
      end if;

      CT.Entries (Idx).Present := 1;
      CT.Entries (Idx).DID     := Interfaces.Unsigned_16 (Domain);
      CT.Entries (Idx).SLPTPTR := Aligned_Pointer_Type (SLPTPTR / 2 ** 12);
   end Add_Entry;

   -------------------------------------------------------------------------

   procedure Serialize
     (RT       : Root_Table_Type;
      Filename : String)
   is
      --  Root_Table_Type'Write adds additional output so manual conversion to
      --  stream array is necessary.
      function Convert is new Ada.Unchecked_Conversion
        (Source => Root_Table_Type,
         Target => Table_Stream);
   begin
      Write (Stream   => Convert (S => RT),
             Filename => Filename);
   end Serialize;

   -------------------------------------------------------------------------

   procedure Serialize
     (CT       : Context_Table_Type;
      Filename : String)
   is
      --  Context_Table_Type'Write adds additional output so manual conversion
      --  to stream array is necessary.
      function Convert is new Ada.Unchecked_Conversion
        (Source => Context_Table_Type,
         Target => Table_Stream);
   begin
      Write (Stream   => Convert (S => CT),
             Filename => Filename);
   end Serialize;

end VTd.Tables.DMAR;
