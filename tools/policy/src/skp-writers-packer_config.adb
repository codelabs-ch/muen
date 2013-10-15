--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Maps;

with SK.Utils;

with Skp.Templates;

package body Skp.Writers.Packer_Config
is

   use Ada.Strings.Unbounded;
   use type SK.Word64;

   type Entry_Type is record
      Filename : Unbounded_String;
      Kind     : File_Kind;
   end record;

   package File_Set_Package is new Ada.Containers.Ordered_Maps
     (Key_Type     => SK.Word64,
      Element_Type => Entry_Type);
   package FSP renames File_Set_Package;

   Files : FSP.Map;

   -------------------------------------------------------------------------

   procedure Add_File
     (Filename : String;
      Address  : SK.Word64;
      Kind     : File_Kind)
   is
   begin
      Files.Insert
        (Key      => Address,
         New_Item => (Filename => To_Unbounded_String (Filename),
                      Kind     => Kind));
   end Add_File;

   -------------------------------------------------------------------------

   procedure Write (Dir_Name : String)
   is
      Buffer : Unbounded_String;
      Tmpl   : Templates.Template_Type;

      --  Write file entry.
      procedure Write_Entry (Pos : FSP.Cursor);

      ----------------------------------------------------------------------

      procedure Write_Entry (Pos : FSP.Cursor)
      is
         use type FSP.Cursor;

         E : constant Entry_Type := FSP.Element (Position => Pos);
      begin
         Buffer := Buffer & Indent
           & "  (Path             => To_Unbounded_String ("""
           & E.Filename & """),"
           & ASCII.LF
           & Indent & "   Physical_Address => 16#"
           & SK.Utils.To_Hex (Item => FSP.Key (Position => Pos)) & "#,"
           & ASCII.LF
           & Indent & "   Kind             => "
           & Capitalize (Str => E.Kind'Img) & ")";

         if Pos /= Files.Last then
            Buffer := Buffer & "," & ASCII.LF;
         end if;
      end Write_Entry;
   begin
      Files.Iterate (Process => Write_Entry'Access);

      Tmpl := Templates.Load (Filename => "skp-packer_config.ads");
      Templates.Replace (Template => Tmpl,
                         Pattern  => "__files__",
                         Content  => To_String (Buffer));
      Templates.Replace (Template => Tmpl,
                         Pattern  => "__file_count__",
                         Content  => Files.Length'Img);
      Templates.Write (Template => Tmpl,
                       Filename => Dir_Name & "/skp-packer_config.ads");
   end Write;

end Skp.Writers.Packer_Config;
