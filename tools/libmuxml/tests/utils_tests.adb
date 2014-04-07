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

with DOM.Core.Nodes;
with DOM.Core.Documents;

with Muxml.Utils;

package body Utils_Tests
is

   use Ahven;
   use Muxml;

   -------------------------------------------------------------------------

   procedure Append_Child
   is
      use type DOM.Core.Node;

      Impl : DOM.Core.DOM_Implementation;
      Data : XML_Data_Type;
      Node, Child : DOM.Core.Node;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element (Doc      => Data.Doc,
                                                 Tag_Name => "node");
      Child := DOM.Core.Documents.Create_Element (Doc     => Data.Doc,
                                                 Tag_Name => "child");
      Utils.Append_Child (Node      => Node,
                          New_Child => Child);

      Assert (Condition => DOM.Core.Nodes.Has_Child_Nodes (N => Node),
              Message   => "Error appending child");
      Assert (Condition => DOM.Core.Nodes.First_Child (N => Node) = Child,
              Message   => "Child mismatch");
   end Append_Child;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "XML utility tests");
      T.Add_Test_Routine
        (Routine => Append_Child'Access,
         Name    => "Append XML child node");
   end Initialize;

end Utils_Tests;
