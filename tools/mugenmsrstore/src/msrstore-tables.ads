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

with Ada.Streams;

with Interfaces;

private with Mutools.Constants;

package Msrstore.Tables
is

   type MSR_Store_Size is range 1 .. 512;

   --  MSR store table as specified by Intel SDM Vol. 3C, "24.7.2 VM-Exit
   --  Controls for MSRs".
   type MSR_Store_Type (Size : MSR_Store_Size) is private;

   --  Returns True if the given MSR store is full.
   function Is_Full (Store : MSR_Store_Type) return Boolean;

   --  Returns True if the given MSR store is empty.
   function Is_Empty (Store : MSR_Store_Type) return Boolean;

   --  Append entry with specified MSR index and data to given store.
   procedure Append_Entry
     (Store : in out MSR_Store_Type;
      Index :        Interfaces.Unsigned_32;
      Data  :        Interfaces.Unsigned_64)
     with
       Pre => not Is_Full (Store => Store);

   --  Convert MSR store to binary stream.
   function To_Stream
     (Store : MSR_Store_Type)
      return Ada.Streams.Stream_Element_Array
     with
       Pre => not Is_Empty (Store => Store);

private

   --  MSR table entry format as specified by Intel SDM Vol. 3C, "24.7.2
   --  VM-Exit Controls for MSRs".
   type Store_Entry_Type is record
      Index    : Interfaces.Unsigned_32;
      Reserved : Interfaces.Unsigned_32;
      Data     : Interfaces.Unsigned_64;
   end record
     with Size => Mutools.Constants.MSR_Store_Entry_Size * 8;

   for Store_Entry_Type use record
      Index    at 0 range 0 .. 31;
      Reserved at 4 range 0 .. 31;
      Data     at 8 range 0 .. 63;
   end record;

   Null_Store_Entry : constant Store_Entry_Type
     := Store_Entry_Type'(Index    => 0,
                          Reserved => 0,
                          Data     => 0);

   type MSR_Table_Type is array (MSR_Store_Size range <>) of Store_Entry_Type;
   pragma Pack (MSR_Table_Type);

   type MSR_Store_Type (Size : MSR_Store_Size) is record
      Next_Idx : Positive := Positive (MSR_Store_Size'First);
      Data     : MSR_Table_Type (1 .. Size) := (others => Null_Store_Entry);
   end record;

   function Is_Empty
     (Store : MSR_Store_Type)
      return Boolean
   is (Store.Next_Idx = Positive (MSR_Store_Size'First));

   function Is_Full
     (Store : MSR_Store_Type)
      return Boolean
   is (Store.Next_Idx > Positive (Store.Size));

end Msrstore.Tables;
