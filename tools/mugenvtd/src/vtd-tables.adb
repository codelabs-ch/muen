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

package body VTd.Tables
is

   -------------------------------------------------------------------------

   procedure Write
     (Stream   : Ada.Streams.Stream_Element_Array;
      Filename : String)
   is
      File : Ada.Streams.Stream_IO.File_Type;
   begin
      Ada.Streams.Stream_IO.Create
        (File => File,
         Mode => Ada.Streams.Stream_IO.Out_File,
         Name => Filename);
      Ada.Streams.Stream_IO.Write (File => File,
                                   Item => Stream);
      Ada.Streams.Stream_IO.Close (File => File);
   end Write;

end VTd.Tables;
