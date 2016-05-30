--
--  Copyright (C) 2013, 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Exceptions;
with Ada.Directories;
with Ada.Direct_IO;

package body Mutools.Templates
is

   use Ada.Strings.Unbounded;

   -------------------------------------------------------------------------

   function Create (Content : String) return Template_Type
   is
   begin
      return T : Template_Type do
         T.Data := To_Unbounded_String (Content);
      end return;
   end Create;

   -------------------------------------------------------------------------

   procedure Replace
     (Template : in out Template_Type;
      Pattern  :        String;
      Content  :        String)
   is
      First : Boolean := True;
      Idx   : Natural := 1;
   begin
      loop
         Idx := Index (Source  => Template.Data,
                       From    => Idx,
                       Pattern => Pattern);
         if First and then Idx = 0 then
            raise Pattern_Not_Found with "Pattern '" & Pattern
              & "' does not exist";
         end if;

         exit when Idx = 0;

         First := False;
         Replace_Slice (Source => Template.Data,
                        Low    => Idx,
                        High   => Idx + Pattern'Length - 1,
                        By     => Content);
      end loop;
   end Replace;

   -------------------------------------------------------------------------

   procedure Write
     (Template : Template_Type;
      Filename : String)
   is
      subtype Content_String is String (1 .. Length (Template.Data));
      package FIO is new Ada.Direct_IO (Content_String);

      Input_File : FIO.File_Type;
   begin
      if Ada.Directories.Exists (Name => Filename) then
         Ada.Directories.Delete_File (Name => Filename);
      end if;

      FIO.Create (File => Input_File,
                  Mode => FIO.Out_File,
                  Name => Filename);
      FIO.Write (File => Input_File,
                 Item => To_String (Template.Data));
      FIO.Close (File => Input_File);

   exception
      when E : others =>
         if FIO.Is_Open (File => Input_File) then
            FIO.Close (File => Input_File);
         end if;
         raise IO_Error with "Unable to write template to '"
           & Filename & "' - " & Ada.Exceptions.Exception_Message (X => E);
   end Write;

end Mutools.Templates;
