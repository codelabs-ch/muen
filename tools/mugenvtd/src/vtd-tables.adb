--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Streams.Stream_IO;
with Ada.Unchecked_Conversion;

with Mutools.Constants;

package body VTd.Tables
is

   use type Ada.Streams.Stream_Element_Offset;

   subtype RT_Stream is Ada.Streams.Stream_Element_Array
     (1 .. Mutools.Constants.Page_Size);

   --  Root_Table_Type'Write adds additional output so manual conversion to
   --  stream array is necessary.
   function Convert is new Ada.Unchecked_Conversion
     (Source => Root_Table_Type,
      Target => RT_Stream);

   -------------------------------------------------------------------------

   procedure Add_Entry
     (RT  : in out Root_Table_Type;
      Bus :        Table_Range;
      CTP :        Table_Pointer_Type)
   is
   begin
      RT.Entries (Bus).Present := 1;
      RT.Entries (Bus).CTP     := CTP;
   end Add_Entry;

   -------------------------------------------------------------------------

   procedure Add_Entry
     (CT      : in out Context_Table_Type;
      Device  :        Device_Range;
      Func    :        Function_Range;
      Domain  :        Domain_Range;
      SLPTPTR :        Table_Pointer_Type)
   is
      Idx : constant Table_Range
        := Table_Range (Device) * 8 + Table_Range (Func);
   begin
      CT.Entries (Idx).Present := 1;
      CT.Entries (Idx).DID     := Interfaces.Unsigned_16 (Domain);
      CT.Entries (Idx).SLPTPTR := SLPTPTR;
   end Add_Entry;

   -------------------------------------------------------------------------

   procedure Serialize
     (RT       : Root_Table_Type;
      Filename : String)
   is
      File : Ada.Streams.Stream_IO.File_Type;
   begin
      Ada.Streams.Stream_IO.Create
        (File => File,
         Mode => Ada.Streams.Stream_IO.Out_File,
         Name => Filename);
      Ada.Streams.Stream_IO.Write (File => File,
                                   Item => Convert (S => RT));
      Ada.Streams.Stream_IO.Close (File => File);
   end Serialize;

end VTd.Tables;
