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

with Mulog;
with Muxml.Utils;
with Mutools.OS;
with Mutools.Utils;

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
         Path : constant String := Output_Dir & "/" & Sig_C;
      begin
         Mulog.Log (Msg => "Processor signature is " & Sig);
         if Ada.Directories.Exists (Name => Path) then
            Mulog.Log (Msg => "Deleting existing file '" & Path & "'");
            Ada.Directories.Delete_File (Name => Path);
         end if;
         Mutools.OS.Execute
           (Command => "/usr/sbin/iucode-tool --strict-checks -v " & Ucode_Dir
            & " -s 0x" & Sig_C & " -w " & Path);
         Mutools.OS.Execute (Command => "/usr/sbin/iucode-tool -l " & Path);
      end;
   end Run;

end Ucode;
