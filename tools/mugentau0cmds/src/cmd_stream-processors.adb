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

with Mutools.XML_Utils;

package body Cmd_Stream.Processors
is

   -------------------------------------------------------------------------

   procedure Create_Processors
     (Policy     : in out Muxml.XML_Data_Type;
      Stream_Doc : in out Utils.Stream_Document_Type)
   is
      Active_CPUs : constant Positive
        := Mutools.XML_Utils.Get_Active_CPU_Count (Data => Policy);
   begin
      for I in 0 .. Active_CPUs - 1 loop
         Utils.Append_Command
           (Stream_Doc => Stream_Doc,
            Name       => "addProcessor",
            Attrs      => ((Attr  => U ("id"),
                            Value => U (Trim (I'Img))),
                           (Attr  => U ("apicId"),
                            Value => U (Trim (Mutools.XML_Utils.To_APIC_ID
                              (Policy => Policy,
                               CPU_ID => I)'Img)))));
      end loop;
   end Create_Processors;

end Cmd_Stream.Processors;
