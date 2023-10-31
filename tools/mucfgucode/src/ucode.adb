--
--  Copyright (C) 2023  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2023  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Mulog;
with Muxml.Utils;
with Mutools.OS;
with Mutools.Utils;
with Mutools.Constants;
with Mutools.XML_Utils;

package body Ucode
is

   -------------------------------------------------------------------------

   procedure Run
     (Policy     : String;
      Ucode_Dir  : String;
      Output_Dir : String)
   is
      Data : Muxml.XML_Data_Type;
   begin
      Mulog.Log (Msg => "Processing policy '" & Policy & "'");

      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_Src,
                   File => Policy);

      declare
         Sig : constant String := Muxml.Utils.Get_Attribute
             (Doc   => Data.Doc,
              XPath => "/system/hardware/processor/cpuid"
              & "[@leaf='16#0000_0001#']",
              Name  => "eax");
         Sig_C : constant String := Mutools.Utils.To_Hex
             (Number    => Interfaces.Unsigned_64'Value (Sig),
              Normalize => False);
         Filename : constant String := Sig_C & ".ucode";
         Path     : constant String := Output_Dir & "/" & Filename;
      begin
         Mulog.Log (Msg => "Processor signature is " & Sig);
         if Ada.Directories.Exists (Name => Path) then
            Mulog.Log (Msg => "Deleting existing file '" & Path & "'");
            Ada.Directories.Delete_File (Name => Path);
         end if;
         Mutools.OS.Execute
           (Command => "iucode_tool --strict-checks -v " & Ucode_Dir
            & " -s 0x" & Sig_C & " -w " & Path);

         if Ada.Directories.Exists (Name => Path) then
            Mutools.OS.Execute (Command => "iucode_tool -l " & Path);
            declare
               use type DOM.Core.Node;
               use type Ada.Directories.File_Size;

               Node : DOM.Core.Node :=
                 Muxml.Utils.Get_Element
                   (Doc   => Data.Doc,
                    XPath =>
                      "/system/memory/memory[@type='kernel_microcode']");
               Size : Ada.Directories.File_Size :=
                 Ada.Directories.Size (Name => Path);
            begin
               if Node /= null then
                  Mulog.Log
                    (Msg => "Removing existing microcode memory region");
                  Node := DOM.Core.Nodes.Remove_Child
                    (N         => Muxml.Utils.Get_Element
                       (Doc   => Data.Doc,
                        XPath => "/system/memory"),
                     Old_Child => Node);
               end if;
               Mulog.Log
                 (Msg => "Adding file-backed memory region for MCU, size"
                  & Size'Img & " bytes");
               Size := Size - (Size mod (-Mutools.Constants.Page_Size));
               Mutools.XML_Utils.Add_Memory_Region
                 (Policy      => Data,
                  Name        => "microcode",
                  Address     => "",
                  Size        =>
                    Mutools.Utils.To_Hex
                      (Number => Interfaces.Unsigned_64 (Size)),
                  Alignment   => "16#1000#",
                  Caching     => "WB",
                  Memory_Type => "kernel_microcode",
                  File_Name   => Filename,
                  File_Offset => "none");
            end;
            Muxml.Write
              (Data => Data,
               Kind => Muxml.Format_Src,
               File => Policy);
            Mulog.Log (Msg => "Policy '" & Policy & "' updated");
         else
            Mulog.Log (Msg => "No matching microcode update");
         end if;
      end;
   end Run;

end Ucode;
