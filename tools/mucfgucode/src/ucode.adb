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

with Muxml.Utils;
with Mulog;

package body Ucode
is

   -------------------------------------------------------------------------

   procedure Run
     (Policy     : String;
      Ucode_Dir  : String;
      Output_Dir : String)
   is
      pragma Unreferenced (Ucode_Dir, Output_Dir);

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
      begin
         Mulog.Log (Msg => "Processor signature is " & Sig);
      end;
   end Run;

end Ucode;
