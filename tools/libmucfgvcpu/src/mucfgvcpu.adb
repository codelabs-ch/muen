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

with DOM.Core.Documents;

with Muxml.Utils;

with Mucfgvcpu.Profile_native;
with Mucfgvcpu.Profile_linux;

package body Mucfgvcpu
is

   type String_Access is not null access constant String;

   type VCPU_Info_Type is record
      XML : String_Access;
   end record;

   Profile_Map : constant array (Profile_Type) of VCPU_Info_Type
     := (Native => (XML => Profile_native.Data'Access),
         Linux  => (XML => Profile_linux.Data'Access));

   -------------------------------------------------------------------------

   procedure Set_VCPU_Profile
     (Profile : Profile_Type;
      Node    : DOM.Core.Node)
   is
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse_String (Data => Data,
                          Kind => Muxml.VCPU_Profile,
                          XML  => Profile_Map (Profile).XML.all);
      Muxml.Utils.Merge
        (Left     => Node,
         Right    => DOM.Core.Documents.Get_Element (Doc => Data.Doc),
         List_Tag => "msr");

      --  The profile's document must not be freed since some resources
      --  referenced by the merged DOM tree are not copied to the Node's
      --  document. This can be removed as soon as XML/Ada supports import of
      --  nodes into a document.

      Data.Doc := null;
      pragma Unreferenced (Data);
   end Set_VCPU_Profile;

end Mucfgvcpu;
