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

package body Paging.Entries
is

   -------------------------------------------------------------------------

   function Create
     (Dst_Index   : Table_Range;
      Dst_Address : Interfaces.Unsigned_64;
      Present     : Boolean := True;
      Readable    : Boolean;
      Writable    : Boolean;
      Executable  : Boolean;
      Maps_Page   : Boolean;
      Global      : Boolean;
      Caching     : Paging.Caching_Type)
      return Table_Entry_Type
   is
   begin
      return Table_Entry_Type'
        (Dst_Table_Index => Dst_Index,
         Dst_Address     => Dst_Address,
         Present         => Present,
         Readable        => Readable,
         Writable        => Writable,
         Executable      => Executable,
         Maps_Page       => Maps_Page,
         Global          => Global,
         Caching         => Caching);
   end Create;

   -------------------------------------------------------------------------

   function Get_Caching (E : Table_Entry_Type) return Paging.Caching_Type
   is
   begin
      return E.Caching;
   end Get_Caching;

   -------------------------------------------------------------------------

   function Get_Dst_Address
     (E : Table_Entry_Type)
      return Interfaces.Unsigned_64
   is
   begin
      return E.Dst_Address;
   end Get_Dst_Address;

   -------------------------------------------------------------------------

   function Get_Dst_Table_Index (E : Table_Entry_Type) return Table_Range
   is
   begin
      return E.Dst_Table_Index;
   end Get_Dst_Table_Index;

   -------------------------------------------------------------------------

   function Is_Executable (E : Table_Entry_Type) return Boolean
   is
   begin
      return E.Executable;
   end Is_Executable;

   -------------------------------------------------------------------------

   function Is_Global (E : Table_Entry_Type) return Boolean
   is
   begin
      return E.Global;
   end Is_Global;

   -------------------------------------------------------------------------

   function Is_Present (E : Table_Entry_Type) return Boolean
   is
   begin
      return E.Present;
   end Is_Present;

   -------------------------------------------------------------------------

   function Is_Readable (E : Table_Entry_Type) return Boolean
   is
   begin
      return E.Readable;
   end Is_Readable;

   -------------------------------------------------------------------------

   function Is_Writable (E : Table_Entry_Type) return Boolean
   is
   begin
      return E.Writable;
   end Is_Writable;

   -------------------------------------------------------------------------

   function Maps_Page (E : Table_Entry_Type) return Boolean
   is
   begin
      return E.Maps_Page;
   end Maps_Page;

   -------------------------------------------------------------------------

   procedure Set_Dst_Address
     (E       : in out Table_Entry_Type;
      Address :        Interfaces.Unsigned_64)
   is
   begin
      E.Dst_Address := Address;
   end Set_Dst_Address;

end Paging.Entries;
