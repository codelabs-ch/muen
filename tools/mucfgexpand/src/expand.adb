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

with Expanders;

package body Expand
is

   -------------------------------------------------------------------------

   procedure Run
     (Policy      : in out Muxml.XML_Data_Type;
      Output_File :        String)
   is
   begin
      Expanders.Register_All;
      Mulog.Log
        (Msg => "Registered expanders" & Expanders.Get_Count'Img);

      Expanders.Run (Data => Policy);

      Muxml.Write
        (File => Output_File,
         Kind => Muxml.Format_A,
         Data => Policy);
   end Run;

end Expand;
