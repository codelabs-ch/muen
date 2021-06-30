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
with Mutools.Utils;

with Mucfgcheck.Validation_Errors;

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
               Validation_Errors.Insert
                 (Msg => "File '" & Path & "' referenced by "
                  & "physical memory region '" & Mem_Name & "' not found");
            end if;
         end;
      end loop;
   end Files_Exist;

   -------------------------------------------------------------------------

   procedure Files_Size (Data : Muxml.XML_Data_Type)
   is
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
              := Get_Input_Directory & "/"
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

            --D @Text(Section  => 'system_src.xsd:fileContentType',
            --D       Priority => 0).
            --D The following checks on the file content are performed.
            --D @UL(Id       => 'validators_file_content',
            --D     Section  => 'system_src.xsd:fileContentType',
            --D     Priority => 0).

            if Offset_Str = "none" then

               --D @Item(List     => 'validators_file_content',
               --D       Priority => 0).
               --D If \texttt{offset} is \texttt{none}, the size of the file
               --D must be less than the memory region size.

               if File_Size > Mem_Size then
                  Validation_Errors.Insert
                    (Msg => "File '" & Path
                     & "' too large for physical memory region '" & Mem_Name
                     & "': " & Mutools.Utils.To_Hex (Number => File_Size)
                     & " > " & Mutools.Utils.To_Hex (Number => Mem_Size));
               end if;
            else

               --D @Item(List     => 'validators_file_content',
               --D       Priority => 0).
               --D If \texttt{offset} is not \texttt{none}, the offset must be
               --D less than the file size. The file size is \emph{not} checked
               --D but the memory region size is used as upper bound.

               Offset := Interfaces.Unsigned_64'Value (Offset_Str);
               if Offset > File_Size then
                  Validation_Errors.Insert
                    (Msg => "Offset of file '" & Path
                     & "' referenced by physical memory region '" & Mem_Name
                     & "' larger than file size: "
                     & Mutools.Utils.To_Hex (Number => Offset) & " > "
                     & Mutools.Utils.To_Hex (Number => File_Size));
               end if;
            end if;
         end;
      end loop;
   end Files_Size;

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
