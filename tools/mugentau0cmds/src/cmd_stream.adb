--
--  Copyright (C) 2019  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2019  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Cmd_Stream.XML_Utils;
with Cmd_Stream.Roots.Devices;

package body Cmd_Stream
is

   -------------------------------------------------------------------------

   procedure Run
     (Policy      : in out Muxml.XML_Data_Type;
      Output_File :        String)
   is
      Stream_Doc : Muxml.XML_Data_Type;
   begin
      XML_Utils.Create_Stream_Boilerplate (Stream_Doc => Stream_Doc);
      Roots.Devices.Create_Physical_PCI_Devices (Policy     => Policy,
                                                 Stream_Doc => Stream_Doc);
      XML_Utils.Append_Command
        (Stream_Doc => Stream_Doc,
         Name       => "activateTau0");

      Muxml.Write (Data => Stream_Doc,
                   Kind => Muxml.None,   --  TODO: Use correct format here
                   File => Output_File);
   end Run;

end Cmd_Stream;
