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

with Ada.Strings.Unbounded;

with Interfaces;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;

with Pack.Command_Line;

package body Pack.Content_Providers
is

   use Ada.Strings.Unbounded;

   -------------------------------------------------------------------------

   procedure Process_Files (Data : in out Param_Type)
   is
      In_Dir     : constant String := Command_Line.Get_Input_Dir;
      File_Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
        (N     => Data.XML_Data.Doc,
         XPath => "/system/memory/memory/file");
      File_Count : constant Natural
        := DOM.Core.Nodes.Length (List => File_Nodes);
   begin
      Mulog.Log (Msg => "Found" & File_Count'Img & " file(s) to process");

      if File_Count = 0 then
         return;
      end if;

      for I in 0 .. File_Count - 1 loop
         declare
            File   : constant DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => File_Nodes,
               Index => I);
            Memory : constant DOM.Core.Node := DOM.Core.Nodes.Parent_Node
              (N => File);

            Filename : constant Unbounded_String := U
              (DOM.Core.Elements.Get_Attribute
                 (Elem => File,
                  Name => "filename"));
            Offset   : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => File,
                    Name => "offset"));
            Address  : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Memory,
                    Name => "physicalAddress"));
            Size     : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Memory,
                    Name => "size"));

         begin
            Image.Add_File (Image   => Data.Image,
                            Path    => In_Dir & "/" & S (Filename),
                            Address => Address,
                            Size    => Size,
                            Offset  => Offset);
         end;
      end loop;
   end Process_Files;

end Pack.Content_Providers;
