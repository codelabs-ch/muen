--
--  Copyright (C) 2022 secunet Security Networks AG
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

with DOM.Core;
with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Hashed_Sets;
with Mutools.Xmldebuglog;
with Muxml;

package Mutools.XML_Templates
is

   -- Expand all useTemplate-nodes and delete template definitions
   procedure Expand (XML_Data     : in out Muxml.XML_Data_Type;
                     Debug_Active :        Boolean);

private
   use all type DOM.Core.Node;

   -- Creates a new XML-Docmunt containing only the given node
   procedure Create_XMLDocument_From_Node
      (New_Doc  : out DOM.Core.Document;
       Src_Node :     DOM.Core.Node);

   -- Create a new document containing the XML-code that substitutes
   -- the useTemplate-statement from Template_Call
   -- Running_Number is used to give the variables and expressions
   -- in the output unique names.
   -- The returned prefix is the final prefixed used generate unique names.
   procedure Compile_Template
      (Template       :     DOM.Core.Node;
       Template_Call  :     DOM.Core.Node;
       Running_Number :     Positive;
       Output         : out Muxml.XML_Data_Type;
       Used_Prefix    : out Ada.Strings.Unbounded.Unbounded_String);

   package Node_Set_Type is new Ada.Containers.Indefinite_Hashed_Sets
      (Element_Type        => DOM.Core.Node,
       Hash                => Mutools.Xmldebuglog.Hash,
       Equivalent_Elements => "=");

   -- Searches for definitions of 'variables' in the following form:
   --  (a) definitions of the form <boolean name="foo" .../>
   --      (also for integer and string)
   --  (b) <expression name="foo".. />
   -- All occurances of these variable-names (as variables) and occurances of
   --  <useTemplate namePrefix="foo" .../>
   -- are then prefixed with the given prefix except if the (attribute)-node
   -- with that reference is contained in Locked_Attr.
   procedure Prefix_Variables
      (Root_Node   : DOM.Core.Node;
       Config_Node : DOM.Core.Node;
       Prefix      : String;
       Locked_Attr : Node_Set_Type.Set);

   -- Adopt and insert deep clones of all child nodes of
   -- Parent_Of_Children before Target.
   -- If Append_Mode is true, the new children are appended
   -- to the list of children of Target.
   -- If Use_Target_As_Parent_For_Log is True, then
   -- entries in the xmldebuglog will be created for the new nodes and these
   -- will inherit the transaction-history from the target. Also, the last
   -- entry of the transaction log will be added to their history in this case.
   procedure Adopt_All_Children
      (Target             : DOM.Core.Node;
       Parent_Of_Children : DOM.Core.Node;
       Append_Mode        : Boolean       := False;
       Debug_Active       : Boolean       := False;
       Ancestor_For_Log   : DOM.Core.Node := null;
       Transaction_Index  : Mutools.Xmldebuglog.Transaction_Log_Index_Type
                          := Mutools.Xmldebuglog.Null_Ref_Index);

end Mutools.XML_Templates;
