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

with Ada.Text_IO;
with Ada.Strings.Unbounded;

with Interfaces;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Mutools.Utils;
with Mutools.Processors;

with Pack.Command_Line;

pragma Elaborate_All (Mutools.Processors);

package body Pack.Content_Providers
is

   use Ada.Strings.Unbounded;

   package Content_Procs is new Mutools.Processors (Param_Type => Param_Type);

   --  Add entry with given parameters to memory map file.
   procedure Add_Mmap_Entry
     (File     : in out Ada.Text_IO.File_Type;
      Mem_Name :        String;
      Path     :        String;
      Format   :        String;
      Address  :        Interfaces.Unsigned_64;
      Size     :        Interfaces.Unsigned_64;
      Offset   :        Interfaces.Unsigned_64);

   -------------------------------------------------------------------------

   procedure Add_Mmap_Entry
     (File     : in out Ada.Text_IO.File_Type;
      Mem_Name :        String;
      Path     :        String;
      Format   :        String;
      Address  :        Interfaces.Unsigned_64;
      Size     :        Interfaces.Unsigned_64;
      Offset   :        Interfaces.Unsigned_64)
   is
   begin
      Ada.Text_IO.Set_Col (File => File,
                           To   => 1);
      Ada.Text_IO.Put (File => File,
                       Item => Mem_Name & ";");
      Ada.Text_IO.Put (File => File,
                       Item => Mutools.Utils.To_Hex (Number => Address) & ";");
      Ada.Text_IO.Put (File => File,
                       Item => Mutools.Utils.To_Hex (Number => Offset) & ";");
      Ada.Text_IO.Put (File => File,
                       Item => Mutools.Utils.To_Hex (Number => Size) & ";");
      Ada.Text_IO.Put (File => File,
                       Item => Format);
      Ada.Text_IO.Set_Col (File => File,
                           To   => 60);
      Ada.Text_IO.Put (File => File,
                       Item => Path);
      Ada.Text_IO.New_Line (File => File);
   end Add_Mmap_Entry;

   -------------------------------------------------------------------------

   function Get_Count return Natural renames Content_Procs.Get_Count;

   -------------------------------------------------------------------------

   procedure Process_Files (Data : in out Param_Type)
   is
      In_Dir     : constant String := Command_Line.Get_Input_Dir;
      File_Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
        (N     => Data.XML_Doc,
         XPath => "/system/memory/memory/file");
      File_Count : constant Natural
        := DOM.Core.Nodes.Length (List => File_Nodes);
      Mmap_File  : Ada.Text_IO.File_Type;
   begin
      Mulog.Log (Msg => "Found" & File_Count'Img & " file(s) to process");

      if File_Count = 0 then
         return;
      end if;

      Ada.Text_IO.Create (File => Mmap_File,
                          Mode => Ada.Text_IO.Out_File,
                          Name => S (Data.Mmap_File));
      begin
         Ada.Text_IO.Put_Line
           (File => Mmap_File,
            Item => "[Name;PhysicalAddress;Offset;Size;Type  Path]");

         for I in 0 .. File_Count - 1 loop
            declare
               File   : constant DOM.Core.Node := DOM.Core.Nodes.Item
                 (List  => File_Nodes,
                  Index => I);
               Memory : constant DOM.Core.Node := DOM.Core.Nodes.Parent_Node
                 (N => File);

               Filename   : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => File,
                    Name => "filename");
               Format     : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => File,
                    Name => "format");
               Offset_Str : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => File,
                    Name => "offset");
               Address    : constant Interfaces.Unsigned_64
                 := Interfaces.Unsigned_64'Value
                   (DOM.Core.Elements.Get_Attribute
                      (Elem => Memory,
                       Name => "physicalAddress"));
               Size       : constant Interfaces.Unsigned_64
                 := Interfaces.Unsigned_64'Value
                   (DOM.Core.Elements.Get_Attribute
                      (Elem => Memory,
                       Name => "size"));
               Mem_Name   : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Memory,
                    Name => "name");
               Offset     : Interfaces.Unsigned_64 := 0;
            begin
               if Offset_Str /= "none" then
                  Offset := Interfaces.Unsigned_64'Value (Offset_Str);
               end if;

               Image.Add_File (Image   => Data.Image,
                               Path    => In_Dir & "/" & Filename,
                               Address => Address,
                               Size    => Size,
                               Offset  => Offset);

               Add_Mmap_Entry (File     => Mmap_File,
                               Mem_Name => Mem_Name,
                               Path     => In_Dir & "/" & Filename,
                               Format   => Format,
                               Address  => Address,
                               Size     => Size,
                               Offset   => Offset);
            end;
         end loop;

         Ada.Text_IO.Close (File => Mmap_File);
         Mulog.Log (Msg => "Memory map written to file '"
                    & S (Data.Mmap_File) & "'");

      exception
         when others =>
            Ada.Text_IO.Close (File => Mmap_File);
            raise;
      end;
   end Process_Files;

   -------------------------------------------------------------------------

   procedure Register_All
   is
   begin
      Content_Procs.Register (Process => Process_Files'Access);
   end Register_All;

   -------------------------------------------------------------------------

   procedure Run (Data : in out Param_Type) renames Content_Procs.Run;

end Pack.Content_Providers;
