--
--  Copyright (C) 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;

package body Mucfgcheck.Files
is

   -------------------------------------------------------------------------

   procedure Files_Exist (Data : Muxml.XML_Data_Type)
   is
      File_Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/memory/memory/file");
      Count : constant Natural := DOM.Core.Nodes.Length (List => File_Nodes);
   begin
      Mulog.Log (Msg => "Checking existence of" & Count'Img & " file(s)");

      for I in 0 .. Count - 1 loop
         declare
            File : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => File_Nodes,
                 Index => I);
            Path : constant String
              := Get_Input_Directory & "/" & DOM.Core.Elements.Get_Attribute
              (Elem => File,
               Name => "filename");
            Mem_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => DOM.Core.Nodes.Parent_Node (N => File),
                 Name => "name");
         begin
            if not Ada.Directories.Exists (Name => Path) then
               raise Validation_Error with "File '" & Path & "' referenced by "
                 & "physical memory region '" & Mem_Name & "' not found";
            end if;
         end;
      end loop;
   end Files_Exist;

   -------------------------------------------------------------------------

   function Get_Input_Directory return String is
     (Ada.Strings.Unbounded.To_String (Input_Dir));

   -------------------------------------------------------------------------

   procedure Set_Input_Directory (Dir : String)
   is
   begin
      Input_Dir := Ada.Strings.Unbounded.To_Unbounded_String (Dir);
   end Set_Input_Directory;

end Mucfgcheck.Files;
