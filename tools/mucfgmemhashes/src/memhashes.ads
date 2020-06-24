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

with Muxml;

package Memhashes
is

   --  Open and validate given input policy. Generate memory integrity hashes
   --  for all memory regions which contain content (file or fill). Extend
   --  policy with generated hashes and write result to specified output file.
   procedure Run (Policy_In, Policy_Out, Input_Dir : String);

   Hasher_Error    : exception;
   Reference_Error : exception;

private

   --  Generate hashes for memory content in given policy.
   procedure Generate_Hashes
     (Policy    : in out Muxml.XML_Data_Type;
      Input_Dir :        String);

   --  Resolve hash references.
   procedure Resolve_Refs (Policy : in out Muxml.XML_Data_Type);

end Memhashes;
