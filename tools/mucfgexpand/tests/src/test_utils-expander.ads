--
--  Copyright (C) 2014, 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014, 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Muxml;

package Test_Utils.Expander
is

   type Process_Policy is not null access procedure
     (Data : in out Muxml.XML_Data_Type);

   procedure Process_Nil (Data : in out Muxml.XML_Data_Type) is null;

   --  Run expander test with given expander procedure. Verify result by
   --  comparing the difference to the original policy with a specified
   --  reference diff. The Pre procedure is executed prior to the expansion
   --  step.
   procedure Run_Test
     (Policy_Filename : String            := "data/test_policy.xml";
      Policy_Format   : Muxml.Schema_Kind := Muxml.Format_Src;
      Filename        : String;
      Ref_Filename    : String;
      Pre             : Process_Policy    := Process_Nil'Access;
      Expander        : Process_Policy);

end Test_Utils.Expander;
