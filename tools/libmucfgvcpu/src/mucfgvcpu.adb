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

with Ada.Strings.Unbounded;

with DOM.Core.Nodes;
with DOM.Core.Documents.Local;

with Muxml.Utils;

with Mucfgvcpu.Profile_native;
with Mucfgvcpu.Profile_vm;

package body Mucfgvcpu
is

   function U
     (Source : String)
      return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   type String_Access is not null access constant String;

   type VCPU_Info_Type is record
      XML : String_Access;
   end record;

   Profile_Map : constant array (Profile_Type) of VCPU_Info_Type
     := (Native => (XML => Profile_native.Data'Access),
         VM     => (XML => Profile_vm.Data'Access));

   -------------------------------------------------------------------------

   procedure Set_VCPU_Profile
     (Profile :        Profile_Type;
      Node    : in out DOM.Core.Node)
   is
      Doc_Node  : constant DOM.Core.Document
        := DOM.Core.Nodes.Owner_Document (N => Node);
      Data      : Muxml.XML_Data_Type;
      VCPU_Node : DOM.Core.Node;
      Old_Node  : DOM.Core.Node;
   begin
      Muxml.Parse_String (Data => Data,
                          Kind => Muxml.VCPU_Profile,
                          XML  => Profile_Map (Profile).XML.all);
      VCPU_Node := DOM.Core.Documents.Local.Adopt_Node
        (Doc    => Doc_Node,
         Source => DOM.Core.Documents.Local.Clone_Node
           (N    => DOM.Core.Documents.Get_Element (Doc => Data.Doc),
            Deep => True));

      Muxml.Utils.Merge
        (Left      => VCPU_Node,
         Right     => Node,
         List_Tags => (1 => U ("msr")));

      Old_Node := DOM.Core.Nodes.Replace_Child
        (N         => DOM.Core.Nodes.Parent_Node (N => Node),
         New_Child => VCPU_Node,
         Old_Child => Node);
      DOM.Core.Nodes.Free (N => Old_Node);

      Node := VCPU_Node;
   end Set_VCPU_Profile;

end Mucfgvcpu;
