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
with Ada.Unchecked_Conversion;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;

with DOM.Core.Documents;
with DOM.Core.Elements;
with DOM.Core.Nodes;
with System;
with Interfaces;
with Mulog;
with Muxml.Utils;
with McKae.XML.XPath.XIA;

package body Mutools.Xmldebuglog
is

   function Node_To_Unsigned_64 is new Ada.Unchecked_Conversion
      (Source => DOM.Core.Node,
       Target => Interfaces.Unsigned_64);

   -------------------------------------------------------------------------

   --  Append New_Item and return True if and only if this is possible.
   function Append_To_Action_Ref
      (Log      : in out Actions_Ref_Type;
       New_Item :        Transaction_Log_Index_Type)
      return Boolean;

   -------------------------------------------------------------------------

   --  Amend backtrace-info of Node with the backtrace-info of Parent.
   --  If one of them does not have a log entry, nothing happens.
   procedure Merge_Parent_Backtrace_Info
      (Node   : DOM.Core.Node;
       Parent : DOM.Core.Node);

   -------------------------------------------------------------------------

   --  Returns the Origin_Info of node either from the log, or from the node
   --  attribute (if no log-entry is found), or Null_Origin_Info otherwise.
   function Get_Origin_Info
      (Node : DOM.Core.Node)
      return Origin_Info_Type;

   -------------------------------------------------------------------------

   function Add_Amend_Transaction
      (Amend_Node : DOM.Core.Node;
       Xpath      : String)
      return Transaction_Log_Index_Type
   is
   begin
      Transaction_Log_Type.Append
         (Container => Transaction_Log,
          New_Item  => (Transaction_Kind => AMEND,
                        Origin_Of_Node   => Get_Origin_Info (Node => Amend_Node),
                        Xpath            => String_Holder_Type.To_Holder (Xpath)));
      return Transaction_Log.Last_Index;

   end Add_Amend_Transaction;

   -------------------------------------------------------------------------

   function Add_Conditional_Transaction
      (Conditional_Node : DOM.Core.Node;
       Coditional_Kind  : Conditional_Kind_Type;
       Var_Name         : String;
       Var_Value        : String;
       Matched          : Boolean;
       Matched_Others   : Boolean)
      return  Transaction_Log_Index_Type
   is
   begin
      Transaction_Log_Type.Append
         (Container => Transaction_Log,
          New_Item  => (Transaction_Kind => CONDITIONAL,
                        Origin_Of_Node   => Get_Origin_Info (Node => Conditional_Node),
                        Coditional_Kind  => Coditional_Kind,
                        Var_Name         => String_Holder_Type.To_Holder (Var_Name),
                        Var_Value        => String_Holder_Type.To_Holder (Var_Value),
                        Matched          => Matched,
                        Matched_Others   => Matched_Others));
      return Transaction_Log.Last_Index;
   end Add_Conditional_Transaction;

   -------------------------------------------------------------------------

   procedure Add_Debug_Infos_As_Comments (Doc : DOM.Core.Document)
   is
      Node : DOM.Core.Node;
   begin
      --  Get the root node and go through the tree.
      Node := DOM.Core.Documents.Get_Element (Doc => Doc);
      while Node /= null loop
         --  Update debug-information of that node
         --  as we go through all nodes, we know that our parent is up-to-date
         --  information and can use Examine_Only_Parent.
         Gather_Backtrace_Info
            (Node                => Node,
             Examine_Only_Parent => True);

         declare
            Comment_Node : DOM.Core.Node
               :=  DOM.Core.Documents.Create_Comment
               (Doc  => Doc,
                Data => Node_Backtrace_To_String (Node));
            Parent_Node : constant DOM.Core.Node
               := DOM.Core.Nodes.Parent_Node (N => Node);
         begin
            if Parent_Node /= null then
               Comment_Node := DOM.Core.Nodes.Insert_Before
                  (N => Parent_Node,
                   New_Child => Comment_Node,
                   Ref_Child => Node);
            end if;
         end;
         Node := Muxml.Utils.Next_Node
            (Current_Node       => Node,
             Only_Element_Nodes => True);
      end loop;

   end Add_Debug_Infos_As_Comments;

   -------------------------------------------------------------------------

   procedure Add_Log_For_Node
      (Node      : DOM.Core.Node;
       Ancestor  : DOM.Core.Node;
       TA_Number : Transaction_Log_Index_Type)
   is
      Success : Boolean;
      TA_Kind : constant Transaction_Kind_Type
         := Transaction_Log (TA_Number).Transaction_Kind;
   begin
      if not Nodes_Backtrace_Log.Contains (Node) then
         Nodes_Backtrace_Log.Insert
            (Key      => Node,
             New_Item =>
                (Self                  => Node,
                 Origin_Of_Node        => Null_Origin_Info,
                 Template_Backtrace    => Null_Actions_Ref,
                 Conditional_Backtrace => Null_Actions_Ref,
                 Amend_Backtrace       => Null_Ref_Index));
      end if;
      Merge_Parent_Backtrace_Info
         (Node   => Node,
          Parent => Ancestor);

      case TA_Kind is
         when USETEMPLATE =>
            Success := Append_To_Action_Ref
               (Log      => Nodes_Backtrace_Log (Node).Template_Backtrace,
                New_Item => TA_Number);
         when CONDITIONAL =>
            Success := Append_To_Action_Ref
               (Log      => Nodes_Backtrace_Log (Node).Conditional_Backtrace,
                New_Item => TA_Number);
         when AMEND =>
            if Nodes_Backtrace_Log (Node).Amend_Backtrace = Null_Ref_Index then
               Success := True;
               Nodes_Backtrace_Log (Node).Amend_Backtrace := TA_Number;
            else
               Success := False;
            end if;
      end case;

      if not Success then
         Mulog.Log (Msg => "Debug-Warning: Could not append to xml-backtrace of node at '"
                       & Get_Xpath (Node => Node)
                       & "'");
      end if;
   end Add_Log_For_Node;

   -------------------------------------------------------------------------

   procedure Add_Transaction_Log_As_Comment (Doc : DOM.Core.Document)
   is
      --  First Element-Node of the document:
      Node : constant DOM.Core.Node
         := DOM.Core.Documents.Get_Element (Doc => Doc);
      Comment_Node : constant DOM.Core.Node
         := DOM.Core.Documents.Create_Comment
         (Doc  => Doc,
          Data => Transaction_Log_To_String);
      Unused : DOM.Core.Node;
   begin
      Unused := DOM.Core.Nodes.Insert_Before
         (N => DOM.Core.Nodes.Parent_Node (N => Node),
          New_Child => Comment_Node,
          Ref_Child => Node);
   end Add_Transaction_Log_As_Comment;

   -------------------------------------------------------------------------

   function Add_Usetemplate_Transaction
      (Usetemplate_Node : DOM.Core.Node;
       Prefix           : String)
      return Transaction_Log_Index_Type
   is
      Template_Name : constant String
         := DOM.Core.Elements.Get_Attribute
         (Elem => Usetemplate_Node,
          Name => "name");
      Parameters : Call_Parameter_List_Type.Vector;

   begin
      --  Add parameters one by one.
      declare
         Call_Parameter_List : constant DOM.Core.Node_List
            := McKae.XML.XPath.XIA.XPath_Query
            (N     => Usetemplate_Node,
             XPath => ".//parameter");
      begin
         for I in 0 .. DOM.Core.Nodes.Length
            (List => Call_Parameter_List) - 1  loop
            declare
               Param_Node : constant DOM.Core.Node
                  := DOM.Core.Nodes.Item
                  (List  => Call_Parameter_List,
                   Index => I);
               Key : constant String
                  := DOM.Core.Elements.Get_Attribute
                  (Elem => Param_Node,
                   Name => "name");
               Value : constant String
                  := DOM.Core.Elements.Get_Attribute
                  (Elem => Param_Node,
                   Name => "value");
            begin
               Call_Parameter_List_Type.Append
                  (Container => Parameters,
                   New_Item => (Key   => String_Holder_Type.To_Holder (Key),
                                Value => String_Holder_Type.To_Holder (Value)));
            end;
         end loop;
      end;

      Transaction_Log_Type.Append
         (Container => Transaction_Log,
          New_Item  => (Transaction_Kind => USETEMPLATE,
                        Origin_Of_Node   => Get_Origin_Info (Node => Usetemplate_Node),
                        Template_Name    => String_Holder_Type.To_Holder (Template_Name),
                        Parameters       => Parameters,
                        Prefix           => String_Holder_Type.To_Holder (Prefix)));
      return Transaction_Log.Last_Index;

   end Add_Usetemplate_Transaction;

   -------------------------------------------------------------------------

   function Append_To_Action_Ref
      (Log      : in out Actions_Ref_Type;
       New_Item :        Transaction_Log_Index_Type)
      return Boolean
   is
   begin
      if Log.Length < Actions_Index_Range'Last then
         Log.Entries (Actions_Index_Range'First + Log.Length) := New_Item;
         Log.Length := Log.Length + 1;
         return True;
      else
         return False;
      end if;
   end Append_To_Action_Ref;

   -------------------------------------------------------------------------

   --  Return a string representation of the key-value pairs in Params.
   function Call_Parameter_List_To_String
      (Params : Call_Parameter_List_Type.Vector)
      return String;

   -------------------------------------------------------------------------

   function Call_Parameter_List_To_String
      (Params : Call_Parameter_List_Type.Vector)
      return String
   is
      use Ada.Strings.Unbounded;
      use all type Ada.Containers.Count_Type;

      Output : Unbounded_String
         := To_Unbounded_String ("Call_Parameters(");
   begin
      for I in Params.First_Index .. Params.Last_Index - 1 loop
         Append (Source   => Output,
                 New_Item => Params (I).Key.Element
                    & "='"
                    & Params (I).Value.Element
                    & "', ");
      end loop;
      if Params.Length > 0 then
         Append (Source   => Output,
                 New_Item => Params.Last_Element.Key.Element
                    & "='"
                    & Params.Last_Element.Value.Element
                    & "'");
      end if;

      Append (Source   => Output,
              New_Item => ")");

      return To_String (Output);
   end Call_Parameter_List_To_String;

   -------------------------------------------------------------------------

   procedure Copy_Log_Entry
      (Old_Node : DOM.Core.Node;
       New_Node : DOM.Core.Node;
       Deep     : Boolean)
   is
      ----------------------------------------------------------------------

      procedure Copy_Log_Entry
         (Old_Node : DOM.Core.Node;
          New_Node : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Copy_Log_Entry
         (Old_Node : DOM.Core.Node;
          New_Node : DOM.Core.Node)
      is
      begin
         if not Nodes_Backtrace_Log.Contains (New_Node) then
            Nodes_Backtrace_Log.Insert
               (Key      => New_Node,
                New_Item =>
                   (Self                  => New_Node,
                    Origin_Of_Node        => Null_Origin_Info,
                    Template_Backtrace    => Null_Actions_Ref,
                    Conditional_Backtrace => Null_Actions_Ref,
                    Amend_Backtrace       => Null_Ref_Index));
         end if;
         if Nodes_Backtrace_Log.Contains (Old_Node) then
            Nodes_Backtrace_Log (New_Node) := Nodes_Backtrace_Log (Old_Node);
         else
            Mulog.Log (Msg => "Debug-Waring: Tried to copy non-existing log-entry.");
         end if;
      end Copy_Log_Entry;

   begin
      Copy_Log_Entry
         (Old_Node => Old_Node,
          New_Node => New_Node);

      if Deep then
         declare
            Current_New : DOM.Core.Node
               := Muxml.Utils.Next_Node_In_Subtree
               (Root_Node          => New_Node,
                Current_Node       => New_Node,
                Only_Element_Nodes => True);
            Current_Old : DOM.Core.Node
               := Muxml.Utils.Next_Node_In_Subtree
               (Root_Node          => Old_Node,
                Current_Node       => Old_Node,
                Only_Element_Nodes => True);
         begin
            while Current_New /= null loop
               Copy_Log_Entry
                  (Old_Node => Current_Old,
                   New_Node => Current_New);
               Current_New := Muxml.Utils.Next_Node_In_Subtree
                  (Root_Node          => New_Node,
                   Current_Node       => Current_New,
                   Only_Element_Nodes => True);
               Current_Old := Muxml.Utils.Next_Node_In_Subtree
                  (Root_Node          => Old_Node,
                   Current_Node       => Current_Old,
                   Only_Element_Nodes => True);
            end loop;
         end;
      end if;
   end Copy_Log_Entry;

   -------------------------------------------------------------------------

   procedure Gather_Backtrace_Info
      (Node                : DOM.Core.Node;
       Examine_Only_Parent : Boolean := False;
       Deep                : Boolean := False)
   is
      Parent : constant DOM.Core.Node
         := DOM.Core.Nodes.Parent_Node (N => Node);

   begin

      if Parent /= null then
         if not Examine_Only_Parent then
            Gather_Backtrace_Info (Node                => Parent,
                                   Examine_Only_Parent => Examine_Only_Parent,
                                   Deep                => False);
         end if;
         Merge_Parent_Backtrace_Info (Node => Node, Parent => Parent);
      end if;

      if Deep then
         declare
            Current_Node : DOM.Core.Node
               := Muxml.Utils.Next_Node_In_Subtree
               (Root_Node          => Node,
                Current_Node       => Node,
                Only_Element_Nodes => True);
         begin
            while Current_Node /= null loop
               Gather_Backtrace_Info (Node                => Current_Node,
                                      Examine_Only_Parent => True,
                                      Deep                => False);
               Current_Node := Muxml.Utils.Next_Node_In_Subtree
                  (Root_Node          => Node,
                   Current_Node       => Current_Node,
                   Only_Element_Nodes => True);
            end loop;
         end;
      end if;

   end Gather_Backtrace_Info;

   -------------------------------------------------------------------------

   function Get_Log_For_Error_Message (Node : DOM.Core.Node) return String
   is
   begin
      if not Nodes_Backtrace_Log.Contains (Node) then
         Nodes_Backtrace_Log.Insert
            (Key      => Node,
             New_Item =>
                (Self                  => Node,
                 Origin_Of_Node        => Get_Origin_Info (Node => Node),
                 Template_Backtrace    => Null_Actions_Ref,
                 Conditional_Backtrace => Null_Actions_Ref,
                 Amend_Backtrace       => Null_Ref_Index));
      else
         Nodes_Backtrace_Log (Node).Origin_Of_Node := Get_Origin_Info (Node => Node);
      end if;

      Gather_Backtrace_Info (Node => Node);
      return "Log information for debugging: " & Node_Backtrace_To_String (Node => Node);
   end Get_Log_For_Error_Message;

   -------------------------------------------------------------------------

   function Get_Origin_Info
      (Node : DOM.Core.Node)
      return Origin_Info_Type
   is
   begin
      -- decide which Origin-Info to use
      if Nodes_Backtrace_Log.Contains (Node) and then
         Nodes_Backtrace_Log (Node).Origin_Of_Node /= Null_Origin_Info
      then
         return Nodes_Backtrace_Log (Node).Origin_Of_Node;
      elsif Muxml.Utils.Has_Attribute (Node => Node, Attr_Name => "originOfNode")
      then
         return Parse_Origin_Attribute (Node => Node);
      else
         return Null_Origin_Info;
      end if;
   end Get_Origin_Info;

   -------------------------------------------------------------------------

   function Get_Xpath (Node : DOM.Core.Node) return String
   is
      use Ada.Strings.Unbounded;

      Current_Node : DOM.Core.Node
         := Node;
      Output : Unbounded_String
         := "/" & To_Unbounded_String (DOM.Core.Nodes.Node_Name (N => Current_Node));
   begin
      while DOM.Core.Nodes.Parent_Node (Current_Node) /= null loop
         Current_Node := DOM.Core.Nodes.Parent_Node (Current_Node);
         Output := "/" & DOM.Core.Nodes.Node_Name (N => Current_Node) & Output;
      end loop;
      return To_String (Output);
   end Get_Xpath;

   -------------------------------------------------------------------------

   function Hash (Node : DOM.Core.Node) return Ada.Containers.Hash_Type
   is
      use all type Interfaces.Unsigned_64;

      pragma Compile_Time_Error
         (not (System.Address'Size = 64 and Ada.Containers.Hash_Type'Size = 32),
          "Error: Implementation of hash assumes "
             & "address size of 64 bit and hash size of 32 bit.");

      Add_64 : constant Interfaces.Unsigned_64
         := Node_To_Unsigned_64 (Node);
   begin
      -- Add_Lower_Bits = Add_64 mod 2**32
      -- Add_Higher_Bits = Add_64 / 2**32
      -- return (Add_Low xor Add_High)
      return Ada.Containers.Hash_Type ((Add_64 mod 2**32) xor (Add_64 / 2**32));

   end Hash;

   -------------------------------------------------------------------------

   procedure Merge_Parent_Backtrace_Info
      (Node   : DOM.Core.Node;
       Parent : DOM.Core.Node)
   is

      ----------------------------------------------------------------------

      procedure Amend_Array
         (Ref      : in out Actions_Ref_Type;
          New_Data :        Actions_Ref_Type);

      ----------------------------------------------------------------------

      procedure Amend_Array
         (Ref      : in out Actions_Ref_Type;
          New_Data :        Actions_Ref_Type)
      is
         Remaining_New : Integer
            := New_Data.Length;
         Next_Element : Transaction_Log_Index_Type;
         Found : Boolean;
      begin
         while Remaining_New > 0 and Ref.Length < Actions_Index_Range'Last loop
            Next_Element := New_Data.Entries
               (Actions_Index_Range'First + Remaining_New - 1);
            Found := False;

            --  Ref may have some but not all array elements of New_Data already
            --  e.g. if Merge_Parent_Backtrace_Info was called in the past
            --  and new actions happend in between.
            --  Hence, if the new element is already present, we omit it.
            for I in Actions_Index_Range'First .. Actions_Index_Range'First + Ref.Length - 1 loop
               if Ref.Entries (I) = Next_Element then
                  Found := True;
               end if;
            end loop;

            if not Found then
               --  Actions on ancestors happend before actions specific to their
               --  children.
               --- Hence, these need to be at the beginning of the array.
               --  Hence, we shift array entries first.
               for I in reverse Actions_Index_Range'First ..
                  Actions_Index_Range'First + Ref.Length - 1
               loop
                  Ref.Entries (I + 1) := Ref.Entries (I);
               end loop;
               Ref.Entries (Actions_Index_Range'First) := Next_Element;
               Ref.Length := Ref.Length + 1;
            end if;

            Remaining_New := Remaining_New - 1;
         end loop;

         if Remaining_New > 0 then
            Mulog.Log (Msg => "Warning: Not enough space to store full xml-backtrace."
                          & "Added partial backtrace.");
         end if;
      end Amend_Array;

   begin
      --  Check if both have a log-entry (abort otherwise).
      if not Nodes_Backtrace_Log.Contains (Key => Node) or
         not Nodes_Backtrace_Log.Contains (Key => Parent)
      then
         return;
      end if;

      declare
         Node_Backtrace  : Node_Backtrace_Type
            renames Nodes_Backtrace_Log (Node);
         Parent_Backtrace : Node_Backtrace_Type
            renames Nodes_Backtrace_Log (Parent);
      begin

         --  If Node has some amend transaction in its history:
         --  Abort, because the history of Node must be written before amend
         --  is evaluated (no transaction can happen afterwards).
         if Node_Backtrace.Amend_Backtrace /= Null_Ref_Index then
            return;
         end if;

         Amend_Array (Ref      => Node_Backtrace.Template_Backtrace,
                      New_Data => Parent_Backtrace.Template_Backtrace);
         Amend_Array (Ref      => Node_Backtrace.Conditional_Backtrace,
                      New_Data => Parent_Backtrace.Conditional_Backtrace);

         Node_Backtrace.Amend_Backtrace := Parent_Backtrace.Amend_Backtrace;
      end;
   end Merge_Parent_Backtrace_Info;

   -------------------------------------------------------------------------

   procedure Move_Origin_To_Log (Doc : DOM.Core.Document)
   is
      Node : DOM.Core.Node;
   begin
      --  Get the root node.
      Node := DOM.Core.Documents.Get_Element (Doc => Doc);
      while Node /= null loop
         if not Muxml.Utils.Has_Attribute (Node => Node, Attr_Name => "originOfNode") then
            Mulog.Log (Msg => "Info: Found node at '"
                          & Get_Xpath (Node)
                          & "' without 'originOfNode' attribute.");
         else
            if not Nodes_Backtrace_Log.Contains (Node) then
               Nodes_Backtrace_Log.Insert
                  (Key      => Node,
                   New_Item =>
                      (Self                  => Node,
                       Origin_Of_Node        => Parse_Origin_Attribute (Node => Node),
                       Template_Backtrace    => Null_Actions_Ref,
                       Conditional_Backtrace => Null_Actions_Ref,
                       Amend_Backtrace       => Null_Ref_Index));
            else
               Nodes_Backtrace_Log (Node).Origin_Of_Node
                  := Parse_Origin_Attribute (Node => Node);
            end if;

            DOM.Core.Elements.Remove_Attribute
            (Elem => Node,
             Name => "originOfNode");
         end if;

         Node := Muxml.Utils.Next_Node
            (Current_Node       => Node,
             Only_Element_Nodes => True);
      end loop;
   end Move_Origin_To_Log;

   -------------------------------------------------------------------------

   function Node_Backtrace_To_String (Node : DOM.Core.Node) return String
   is
      use Ada.Strings.Unbounded;
      Output : Unbounded_String;
      NB : Node_Backtrace_Type;

   begin
      if not Nodes_Backtrace_Log.Contains (Node) then
         return "";
      end if;
      NB := Nodes_Backtrace_Log (Node);

      Output := To_Unbounded_String
         ("Node_Name='"
             & DOM.Core.Nodes.Node_Name (Node)
             & "', "
             & Origin_To_String (NB.Origin_Of_Node));

      if NB.Amend_Backtrace /= Null_Ref_Index then
         Append (Source   => Output,
                 New_Item => ", " & Transaction_To_String
                    (Transaction_Log (NB.Amend_Backtrace)));
      end if;

      --  Append conditional-transactions
      --  (in reverse order, so the last transaction comes first).
      for I in reverse Actions_Index_Range'First ..
         Actions_Index_Range'First + NB.Conditional_Backtrace.Length - 1
      loop
         Append (Source   => Output,
                 New_Item => ", ");
         Append (Source   => Output,
                 New_Item => Transaction_To_String
                           (Transaction_Log
                               (NB.Conditional_Backtrace.Entries (I))));
      end loop;

      --  Append useTemplate-transactions.
      for I in reverse Actions_Index_Range'First ..
         Actions_Index_Range'First + NB.Template_Backtrace.Length - 1
      loop
         Append (Source   => Output,
                 New_Item => ", ");
         Append (Source   => Output,
                 New_Item => Transaction_To_String
                           (Transaction_Log
                               (NB.Template_Backtrace.Entries (I))));
      end loop;

      return To_String (Output);
   end Node_Backtrace_To_String;

   -------------------------------------------------------------------------

   function Origin_To_String (Origin : Origin_Info_Type) return String
   is
   begin
      return "Node_Origin=(Filename='"
         & Origin.File_Name.Element
         & "', Line="
         & Ada.Strings.Fixed.Trim (Source => Origin.Line'Img, Side => Ada.Strings.Both)
         & ", Column="
         & Ada.Strings.Fixed.Trim (Source => Origin.Column'Img, Side => Ada.Strings.Both)
         & ")";
   end Origin_To_String;

   -------------------------------------------------------------------------

   function Parse_Origin_Attribute
      (Node : DOM.Core.Node)
      return Origin_Info_Type
   is
   begin
      if not (2 = Ada.Strings.Fixed.Count
                 (Source  => DOM.Core.Elements.Get_Attribute
                     (Elem => Node,
                      Name => "originOfNode"),
                  Pattern => ":"))
      then
         Mulog.Log (Msg => "Warning: Found node at '"
                       & Get_Xpath (Node)
                       & "' with malformed 'originOfNode' attribute."
                       & " Attribute value was '"
                       & DOM.Core.Elements.Get_Attribute
                       (Elem => Node,
                        Name => "originOfNode")
                       & "'.");
         return Null_Origin_Info;
      else
         declare
            Origin       : constant String
                         := DOM.Core.Elements.Get_Attribute
                              (Elem    => Node,
                               Name    => "originOfNode");
            First_Index  : constant Natural
                         := Ada.Strings.Fixed.Index
                              (Source  => Origin,
                               Pattern => ":",
                               From    => Origin'First);
            Second_Index : constant Natural
                         := Ada.Strings.Fixed.Index
                              (Source  => Origin,
                               Pattern => ":",
                               From    => First_Index + 1);
            Filename     : constant String
                         := Origin (Origin'First .. First_Index - 1);
            Line         : constant Natural
                         := Natural'Value
                              (Origin (First_Index + 1 .. Second_Index - 1));
            Column       : constant Natural
                         := Natural'Value
                              (Origin (Second_Index + 1 .. Origin'Last));
         begin
            return (File_Name => String_Holder_Type.To_Holder (Filename),
                    Line      => Line,
                    Column    => Column);
         end;
      end if;
   end Parse_Origin_Attribute;

   -------------------------------------------------------------------------

   procedure Remove_Log_Of_Node (Node : DOM.Core.Node)
   is
   begin
      if Nodes_Backtrace_Log.Contains (Node) then
         Nodes_Backtrace_Log.Delete (Key => Node);
      end if;
   end Remove_Log_Of_Node;

   -------------------------------------------------------------------------

   procedure Remove_Log_Of_Subtree
      (Node  : DOM.Core.Node;
       XPath : String := "")
   is
      Root_Node    : DOM.Core.Node
         := Node;
      Current_Node : DOM.Core.Node;
   begin
      if XPath /= "" then
         declare
            Exprs : constant DOM.Core.Node_List
               := McKae.XML.XPath.XIA.XPath_Query
               (N     => Node,
                XPath => XPath);

         begin
            if DOM.Core.Nodes.Length (List => Exprs) > 0 then
               Root_Node := DOM.Core.Nodes.Item (List  => Exprs, Index => 0);
            else
               raise Muxml.Validation_Error with
                  "Remove_Log_Of_Subtree called with XPath that does not match"
                  & "any nodes";
            end if;
         end;
      end if;
      Current_Node := Root_Node;

      while Current_Node /= null loop
         Remove_Log_Of_Node (Node => Current_Node);

         --  To look for non-element nodes is a precaution in case some got
         --  a log-entry by mistake.
         Current_Node := Muxml.Utils.Next_Node_In_Subtree
            (Root_Node          => Root_Node,
             Current_Node       => Current_Node,
             Only_Element_Nodes => False);
      end loop;
   end Remove_Log_Of_Subtree;

   -------------------------------------------------------------------------

   function Transaction_Log_To_String return String
   is
      use Ada.Strings.Unbounded;
      Output : Unbounded_String
         := To_Unbounded_String ("Transaction_Log:");
   begin
      if Transaction_Log.Is_Empty then
         Append (Source   => Output,
                 New_Item =>  "-- empty --");
         return To_String (Output);
      else
         for T of Transaction_Log loop
            Append (Source   => Output,
                    New_Item => ASCII.LF & Transaction_To_String (T));
         end loop;
         return To_String (Output);
      end if;
   end Transaction_Log_To_String;

   -------------------------------------------------------------------------

   function Transaction_To_String (TA : Transaction_Type) return String
   is
      Kind : constant  Transaction_Kind_Type
         := TA.Transaction_Kind;

   begin
      case Kind is
         when USETEMPLATE =>
            return "Transaction(Kind='useTemplate', "
               & Origin_To_String (Origin => TA.Origin_Of_Node)
               & ",  Template_Name='"
               & TA.Template_Name.Element
               & "', "
               & Call_Parameter_List_To_String (TA.Parameters)
               & ", Prefix='"
               & TA.Prefix.Element
               & "')";
         when CONDITIONAL =>
            return "Transaction(Kind='"
               & (if TA.Coditional_Kind = IFCOND then "if" else "case")
               & "', "
               & Origin_To_String (Origin => TA.Origin_Of_Node)
               & ", Var_Name='"
               & TA.Var_Name.Element
               & "', Var_Value='"
               & TA.Var_Value.Element
               & "', Matched="
               & TA.Matched'Img
               & ", Matched_Others="
               & TA.Matched_Others'Img
               & ")";
         when AMEND =>
            return "Transaction(Kind='amend', "
               & Origin_To_String (Origin => TA.Origin_Of_Node)
               & ", Xpath='"
               & TA.Xpath.Element
               & "')";
      end case;
   end Transaction_To_String;

end Mutools.Xmldebuglog;
