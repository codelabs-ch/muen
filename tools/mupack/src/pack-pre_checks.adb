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

with Ada.Directories;

with Interfaces;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mutools.Utils;
with Mucfgcheck.Files;

package body Pack.Pre_Checks
is

   -------------------------------------------------------------------------

   procedure Clear renames Check_Procs.Clear;

   -------------------------------------------------------------------------

   procedure Files_Size (Data : Muxml.XML_Data_Type)
   is
      use type Interfaces.Unsigned_64;

      File_Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/memory/memory/file");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => File_Nodes) - 1 loop
         declare
            File       : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => File_Nodes,
                 Index => I);
            Memory     : constant DOM.Core.Node
              := DOM.Core.Nodes.Parent_Node (N => File);
            Path       : constant String
              := Mucfgcheck.Files.Get_Input_Directory & "/"
              & DOM.Core.Elements.Get_Attribute
              (Elem => File,
               Name => "filename");
            Mem_Name   : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Memory,
                 Name => "name");
            Offset_Str : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => File,
                 Name => "offset");
            Mem_Size   : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Memory,
                    Name => "size"));
            File_Size  : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64 (Ada.Directories.Size (Name => Path));
            Offset     : Interfaces.Unsigned_64 := 0;
         begin
            if Offset_Str = "none" then
               if File_Size > Mem_Size then
                  raise Check_Error with "File '" & Path & "' too large for "
                    & "physical memory region '" & Mem_Name & "': "
                    & Mutools.Utils.To_Hex (Number => File_Size) & " > "
                    & Mutools.Utils.To_Hex (Number => Mem_Size);
               end if;
            else
               Offset := Interfaces.Unsigned_64'Value (Offset_Str);
               if Offset > File_Size then
                  raise Check_Error with "Offset of file '" & Path
                    & "' referenced by physical memory region '" & Mem_Name
                    & "' larger than file size: "
                    & Mutools.Utils.To_Hex (Number => Offset) & " > "
                    & Mutools.Utils.To_Hex (Number => File_Size);
               end if;
            end if;
         end;
      end loop;
   end Files_Size;

   -------------------------------------------------------------------------

   function Get_Count return Natural renames Check_Procs.Get_Count;

   -------------------------------------------------------------------------

   procedure Register_All
   is
   begin
      Check_Procs.Register (Process => Mucfgcheck.Files.Files_Exist'Access);
      Check_Procs.Register (Process => Files_Size'Access);
      Check_Procs.Register (Process => Unresolved_Hash_References'Access);
   end Register_All;

   -------------------------------------------------------------------------

   procedure Run
     (Data      : Muxml.XML_Data_Type;
      Input_Dir : String)
   is
   begin
      Mucfgcheck.Files.Set_Input_Directory (Dir => Input_Dir);
      Check_Procs.Run (Data => Data);
   end Run;

   -------------------------------------------------------------------------

   procedure Unresolved_Hash_References (Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/memory/memory/hashRef");
      Count : constant Natural
        := DOM.Core.Nodes.Length (List => Nodes);
   begin
      if Count > 0 then
         raise Check_Error with "Policy contains" & Count'Img & " unresolved "
           & "hash reference(s)";
      end if;
   end Unresolved_Hash_References;

end Pack.Pre_Checks;
