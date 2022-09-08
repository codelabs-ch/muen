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
with Ada.Containers;
with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with DOM.Core;

package Mutools.Xmldebuglog
is
   subtype Transaction_Log_Index_Type is Natural;
   -- Transaction_Log_Index_Type'First is used as "no reference"
   subtype Transaction_Log_Legal_Index_Type is Transaction_Log_Index_Type range
      Transaction_Log_Index_Type'First + 1 .. Transaction_Log_Index_Type'Last;
   Null_Ref_Index : constant  Transaction_Log_Index_Type
      := Transaction_Log_Index_Type'First;

   type Conditional_Kind_Type is (IFCOND, CASECOND);
   type Transaction_Kind_Type is (USETEMPLATE, CONDITIONAL, AMEND);

   -- Traverse given XML-Tree and for each element-node N with an attribute A
   -- called "originOfNode", create a log-entry for N (if not existing).
   -- Then, A is removed from N and its log-entry gets the value of A as Origin_Of_Node.
   procedure Move_Origin_To_Log (Doc : DOM.Core.Document);

   -- For each element-node in Doc, generate the Node_Backtrace_Log-entry and
   -- insert a comment is Doc (right before the node) containing that information.
   procedure Add_Debug_Infos_As_Comments (Doc : DOM.Core.Document);

   -- Put a comment before the first element-node of Doc containing the current
   -- transaction log
   procedure Add_Transaction_Log_As_Comment (Doc : DOM.Core.Document);

   -- Return all entries of the transaction log, starting with the oldest transaction
   function Transaction_Log_To_String return String;

   -- Return log-information for the given node as it is
   -- WARNING: the information stored in the entry for Node may not be complete!
   -- Depending on the current state, it might be neccessary to generate
   -- the correct backtrace by examining the ancestors of Node, first.
   function Node_Backtrace_To_String (Node : DOM.Core.Node) return String;

   -- Gather Backtrace for Node and return a string meant for error messages.
   function Get_Log_For_Error_Message (Node : DOM.Core.Node) return String;

   -- return names of ancestors of Node (in the form of an xpath to node)
   -- Meant to work with element-nodes. Unspecified for other node-types.
   function Get_Xpath (Node : DOM.Core.Node) return String;

   -- Add an entry to the transaction log and return its index.
   -- The given node is assumed to be a valid useTemplate node.
   function Add_Usetemplate_Transaction
      (Usetemplate_Node : DOM.Core.Node;
       Prefix           : String)
      return Transaction_Log_Index_Type;

   -- Add an entry to the transaction log and return its index.
   function Add_Conditional_Transaction
      (Conditional_Node : DOM.Core.Node;
       Coditional_Kind  : Conditional_Kind_Type;
       Var_Name         : String;
       Var_Value        : String;
       Matched          : Boolean;
       Matched_Others   : Boolean)
      return Transaction_Log_Index_Type;

   -- Add an entry to the transaction log and return its index.
   function Add_Amend_Transaction
      (Amend_Node : DOM.Core.Node;
       Xpath      : String)
      return Transaction_Log_Index_Type;

   -- Add a Node_Backtrace entry for the given node (if not existent).
   -- Inherits backrace-information from Ancestor and
   -- adds TA_Number as last transaction that affected this node.
   procedure Add_Log_For_Node
      (Node      : DOM.Core.Node;
       Ancestor  : DOM.Core.Node;
       TA_Number : Transaction_Log_Index_Type);

   -- remove entry in Nodes_Backtrace_Log for Node, if it exists.
   -- Do nothing otherwise.
   procedure Remove_Log_Of_Node (Node : DOM.Core.Node);

   -- remove all entries in Nodes_Backtrace_Log for Node and its decendents
   -- If XPath is not empty, the root of the subtree will be determined
   -- by the first matching node.
   procedure Remove_Log_Of_Subtree (Node  : DOM.Core.Node;
                                    XPath : String := "");

   -- Create a new Nodes_Backtrace_Log for New_Node and fill it with the content
   -- of the entry for Old_Node.
   -- If Old_Node has no entry, a warning is given and an empty entry is created.
   -- If Deep is true, the same is done for each node of the subtree with root
   -- New_Node. In this case, the trees are assumed to have the same structure.
   procedure Copy_Log_Entry
      (Old_Node : DOM.Core.Node;
       New_Node : DOM.Core.Node;
       Deep     : Boolean);

   -- Go to ancestors of Node and use their backtrace-log to write complete backtrace
   -- information to the log-entry for Node.
   -- If Deep is True, the same is done for all nodes in the subtree spanned
   -- by Node.
   -- If Examine_Only_Parent is True, only the direct parent of Node is examined
   -- but no other ancestors.
   procedure Gather_Backtrace_Info
      (Node                : DOM.Core.Node;
       Examine_Only_Parent : Boolean := False;
       Deep                : Boolean := False);

private
   use all type DOM.Core.Node;

   -- There are 2 kinds of logs and one uses the other
   -- (1) There is a transaction log which contains information about
   --     successfully completed transactions, i.e.,
   --     instanciated templates, conditionals that evaluated to True and
   --     amend statements.
   -- (2) There is a node-backtrace for each node in the xml-tree.
   --     That contains the origin of that node (filename + line + column)
   --     and a list of pointers to transactions that created or moved that node.
   --     However, the latter information is not completely stored in the list
   --     of each node, but must be gathered by examining the nodes ancestors'
   --     backtrace as well.

   package String_Holder_Type is new Ada.Containers.Indefinite_Holders
      (Element_Type => String);

   type Origin_Info_Type is record
      File_Name : String_Holder_Type.Holder;
      Line      : Natural;
      Column    : Natural;
   end record;

   Null_Origin_Info : constant Origin_Info_Type
      := (File_Name => String_Holder_Type.To_Holder (""),
          Line      => 0,
          Column    => 0);

   type Call_Parameter_Type is record
      Key   : String_Holder_Type.Holder;
      Value : String_Holder_Type.Holder;
   end record;

   package Call_Parameter_List_Type is new Ada.Containers.Vectors
      (Element_Type => Call_Parameter_Type,
       Index_Type   => Positive);

   type Transaction_Type (Transaction_Kind : Transaction_Kind_Type) is record
      Origin_Of_Node   : Origin_Info_Type;
      case Transaction_Kind is
         when USETEMPLATE =>
            Template_Name   : String_Holder_Type.Holder;
            Parameters      : Call_Parameter_List_Type.Vector;
            Prefix          : String_Holder_Type.Holder;
         when CONDITIONAL =>
            Coditional_Kind : Conditional_Kind_Type;
            Var_Name        : String_Holder_Type.Holder;
            Var_Value       : String_Holder_Type.Holder;
            -- 'did the variable match something?'
            Matched         : Boolean;
            Matched_Others  : Boolean := False;
         when AMEND =>
            Xpath           : String_Holder_Type.Holder;
      end case;
   end record;

   -- data type of the transaction-log
   package Transaction_Log_Type is new Ada.Containers.Indefinite_Vectors
      (Element_Type => Transaction_Type,
       Index_Type   => Transaction_Log_Legal_Index_Type);

   subtype Actions_Index_Range is Natural range 1 .. 20;
   type Actions_Array_Type is array (Actions_Index_Range) of Transaction_Log_Index_Type
      with Default_Component_Value => Null_Ref_Index;

   type Actions_Ref_Type is record
      Length  : Natural := 0;
      Entries : Actions_Array_Type;
   end record;

   Null_Actions_Ref : constant Actions_Ref_Type
      := (Length  => 0,
          Entries => (others => Null_Ref_Index));

   type Node_Backtrace_Type is record
      Self                  : DOM.Core.Node; -- needed to determine ancestors
      Origin_Of_Node        : Origin_Info_Type;
      Template_Backtrace    : Actions_Ref_Type;
      Conditional_Backtrace : Actions_Ref_Type;
      -- Amend has at most one enty
      Amend_Backtrace       :  Transaction_Log_Index_Type := Null_Ref_Index;
   end record;

   -- Produce a hash for a DOM-Node.
   -- The hash is produced soly from the address that Node points to
   -- (which is justifed as equality of nodes is checked the same way).
   -- This hash-function does not claim any security properties.
   function Hash (Node : DOM.Core.Node) return Ada.Containers.Hash_Type;

   -- data type for node-backtrace log
   package Nodes_Backtrace_Log_Type is new Ada.Containers.Hashed_Maps
      (Key_Type        => DOM.Core.Node,
       Element_Type    => Node_Backtrace_Type,
       Hash            => Hash,
       Equivalent_Keys => "=");

   Transaction_Log     : Transaction_Log_Type.Vector;
   Nodes_Backtrace_Log : Nodes_Backtrace_Log_Type.Map;

   -- Methods to get a string representation of the logged data
   function Origin_To_String (Origin : Origin_Info_Type) return String;
   function Transaction_To_String (TA : Transaction_Type) return String;

   -- try to read "originOfNode" attribute of Node and parse it
   -- Null_Origin_Info is returned on error
   function Parse_Origin_Attribute
      (Node : DOM.Core.Node)
      return Origin_Info_Type;

end Mutools.Xmldebuglog;
