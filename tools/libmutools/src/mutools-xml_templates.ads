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

with Muxml;
with DOM.Core;

package Mutools.XML_Templates
is

   -- Expand all useTemplate-nodes and delete template definitions
   procedure Expand (XML_Data : in out Muxml.XML_Data_Type);

private
   -- Creates a new XML-Docmunt containing only the given node
   procedure Create_XMLDocument_From_Node
      (New_Doc  : out DOM.Core.Document;
       Src_Node :     DOM.Core.Node);

   -- Create a new document containing the XML-code that substitutes
   -- the useTemplate-statement from Template_Call
   -- Running_Number is used to give the variables and expressions
   -- in the output unique names.
   procedure Compile_Template
      (Template       :     DOM.Core.Node;
       Template_Call  :     DOM.Core.Node;
       Running_Number :     Positive;
       Output         : out Muxml.XML_Data_Type);

   -- Searches for names of 'variables' in the following form:
   --  (a) definitions of the form <boolean name="..." .../>
   --      (also for integer and string)
   --  (b) <expression name="...".. />
   --  (c) <usetemplate namePrefix="..." .../>
   -- These variables are then renamed in all children of the given
   -- node
   procedure Prefix_Variables
      (Root_Node : DOM.Core.Node;
       Prefix    : String);

   -- Adopt and insert deep clones of all child nodes of
   -- Parent_Of_Children before Target
   -- If Append_Mode is true, the new children are appended
   -- to the list of children of Target
   procedure Adopt_All_Children
      (Target             : DOM.Core.Node;
       Parent_Of_Children : DOM.Core.Node;
       Append_Mode        : Boolean := False);

end Mutools.XML_Templates;
