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

with Mulog;
with Muxml;

package body Memhashes
is

   --  Generate hashes for memory content in given policy.
   procedure Generate_Hashes (Policy : in out Muxml.XML_Data_Type);

   -------------------------------------------------------------------------

   procedure Generate_Hashes (Policy : in out Muxml.XML_Data_Type)
   is
   begin
      null;
   end Generate_Hashes;

   -------------------------------------------------------------------------

   procedure Run (Policy_In, Policy_Out, Input_Dir : String)
   is
      Policy : Muxml.XML_Data_Type;
   begin
      Mulog.Log (Msg => "Looking for input files in '" & Input_Dir & "'");
      Mulog.Log (Msg => "Processing policy '" & Policy_In & "'");
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => Policy_In);

      Generate_Hashes (Policy => Policy);

      Mulog.Log (Msg => "Writing policy to '" & Policy_Out & "'");
      Muxml.Write (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => Policy_Out);
   end Run;

end Memhashes;
