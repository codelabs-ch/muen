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

with Mulog;
with Muxml;

with Validate.XML_Processors;
with Validate.Command_Line;
with Validators;

package body Validate
is

   -------------------------------------------------------------------------

   procedure Run
   is
      Data        : Muxml.XML_Data_Type;
      Policy_File : constant String := Command_Line.Get_Policy;
   begin
      Mulog.Log (Msg => "Validating policy '" & Policy_File & "'");

      Validators.Register_All;
      Mulog.Log
        (Msg => "Registered validators" & XML_Processors.Get_Count'Img);

      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => Policy_File);
      XML_Processors.Run (Data => Data);

      Mulog.Log (Msg => "Successfully validated policy '" & Policy_File & "'");
   end Run;

end Validate;
