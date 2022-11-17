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
with DOM.Core;
with DOM.Core.Elements;
with DOM.Core.Nodes;

with McKae.XML.XPath.XIA;

with Mulog;
with Mutools.Expressions;
with Mutools.Expressions.Case_Expression;
with Mutools.Xmldebuglog;

package body Mutools.Conditionals
is
   --  Recursively evaluate all conditionals of given parent node.
   procedure Evaluate
      (Policy       :        Muxml.XML_Data_Type;
       Config       :        DOM.Core.Node_List;
       Parent       :        DOM.Core.Node;
       Node_Access  : in out Expressions.Access_Hashmaps_Type;
       Debug_Active :        Boolean);

   -------------------------------------------------------------------------

   --  Transfer all children from specified node to parent.
   procedure Transfer_Children
      (Old_Parent        : DOM.Core.Node;
       New_Parent        : DOM.Core.Node;
       Ref_In_New_Parent : DOM.Core.Node;
       Debug_Active      : Boolean       := False;
       Ancestor_For_Log  : DOM.Core.Node := null;
       Transaction_Index : Xmldebuglog.Transaction_Log_Index_Type
                         := Xmldebuglog.Null_Ref_Index);

   -------------------------------------------------------------------------

   procedure Evaluate
      (Policy       :        Muxml.XML_Data_Type;
       Config       :        DOM.Core.Node_List;
       Parent       :        DOM.Core.Node;
       Node_Access  : in out Expressions.Access_Hashmaps_Type;
       Debug_Active :        Boolean)
   is
      use type DOM.Core.Node;

      Next_Child : DOM.Core.Node;
      Cur_Child  : DOM.Core.Node
         := DOM.Core.Nodes.First_Child (N => Parent);

      ----------------------------------------------------------------------

      --  Get the type of the given Config_Var, try to cast Value to that type
      --  and return 'true' if and only if the result is equal to the value of
      --  Config_Var.
      function Is_Value_Equal_After_Cast
         (Config_Var_Name : String;
          Value           : String;
          Node_Access     : Expressions.Access_Hashmaps_Type)
         return Boolean;

      ----------------------------------------------------------------------

      function Is_Value_Equal_After_Cast
         (Config_Var_Name : String;
          Value           : String;
          Node_Access     : Expressions.Access_Hashmaps_Type)
         return Boolean
      is
      begin
         if Node_Access.Output_Boolean.Contains (Config_Var_Name) then
            declare
               Input_Value : Boolean;
            begin
               Input_Value := Boolean'Value (Value);
               return Input_Value = Node_Access.Output_Boolean (Config_Var_Name);
            exception
               when Constraint_Error =>
                  raise Expressions.Invalid_Expression with
                     "Cannot compare value '"
                     & Value
                     & "' to variable '"
                     & Config_Var_Name
                     & "' which is a Boolean (cast failed)";
            end;
         elsif Node_Access.Output_Integer.Contains (Config_Var_Name) then
            declare
               Input_Value : Integer;
            begin
               Input_Value := Integer'Value (Value);
               return Input_Value = Node_Access.Output_Integer (Config_Var_Name);
            exception
               when Constraint_Error =>
                  raise Expressions.Invalid_Expression with
                     "Cannot compare value '"
                     & Value
                     & "' to variable '"
                     & Config_Var_Name
                     & "' which is an Integer (cast failed)";
            end;
         elsif Node_Access.Output_String.Contains (Config_Var_Name) then
            return Value = Node_Access.Output_String (Config_Var_Name);
         else
            raise Expressions.Invalid_Expression with
               "Cannot find variable with name '"
               & Config_Var_Name
               & "' in configuration";
         end if;
      end Is_Value_Equal_After_Cast;

   begin
      while Cur_Child /= null loop

         --  Recursively evaluate children before processing conditional.

         Evaluate (Policy       => Policy,
                   Config       => Config,
                   Parent       => Cur_Child,
                   Node_Access  => Node_Access,
                   Debug_Active => Debug_Active);

         --  Get next child before potentially removing current child from
         --  parent.

         Next_Child := DOM.Core.Nodes.Next_Sibling (N => Cur_Child);
         begin
            if DOM.Core.Nodes.Node_Name (N => Cur_Child) = "if" then
               declare
                  Value     : constant String
                     := DOM.Core.Elements.Get_Attribute
                     (Elem => Cur_Child,
                      Name => "value");
                  Cfg_Name  : constant String
                     := DOM.Core.Elements.Get_Attribute
                     (Elem => Cur_Child,
                      Name => "variable");
                  Dummy     : DOM.Core.Node;
                  Log_Index : Xmldebuglog.Transaction_Log_Index_Type
                     := Xmldebuglog.Null_Ref_Index;
               begin
                  if Is_Value_Equal_After_Cast
                     (Config_Var_Name => Cfg_Name,
                      Value           => Value,
                      Node_Access     => Node_Access)
                  then
                     if Debug_Active then
                        Log_Index := Xmldebuglog.Add_Conditional_Transaction
                           (Conditional_Node => Cur_Child,
                            Coditional_Kind  => Xmldebuglog.IFCOND,
                            Var_Name         => Cfg_Name,
                            Var_Value        => Value,
                            Matched          => True,
                            Matched_Others   => False);
                     end if;

                     Transfer_Children
                        (Old_Parent        => Cur_Child,
                         New_Parent        => Parent,
                         Ref_In_New_Parent => Cur_Child,
                         Debug_Active      => Debug_Active,
                         Ancestor_For_Log  => Cur_Child,
                         Transaction_Index => Log_Index);
                  else
                     if Debug_Active then
                        Log_Index := Xmldebuglog.Add_Conditional_Transaction
                           (Conditional_Node => Cur_Child,
                            Coditional_Kind  => Xmldebuglog.IFCOND,
                            Var_Name         => Cfg_Name,
                            Var_Value        => Value,
                            Matched          => False,
                            Matched_Others   => False);
                     end if;
                  end if;

                  if Debug_Active then
                     Xmldebuglog.Remove_Log_Of_Subtree (Node => Cur_Child);
                  end if;

                  Dummy := DOM.Core.Nodes.Remove_Child
                     (N         => Parent,
                      Old_Child => Cur_Child);
                  DOM.Core.Nodes.Free (N => Dummy);
               end;
            elsif DOM.Core.Nodes.Node_Name (N => Cur_Child) = "case" then
               declare
                  Dummy, Matching_Option_Node : DOM.Core.Node;

                  --  We need the backtrace only for syntactial reasons.
                  Backtrace : String_Vector.Vector;
                  Log_Index : Xmldebuglog.Transaction_Log_Index_Type
                     := Xmldebuglog.Null_Ref_Index;
               begin
                  Expressions.Case_Expression.Evaluate_Case_Node_Frame
                     (Case_Node   => Cur_Child,
                      Return_Node => Matching_Option_Node,
                      Backtrace   => Backtrace,
                      Node_Access => Node_Access);

                  if Matching_Option_Node /= null then
                     if Debug_Active then
                        declare
                           Var_Name : constant String
                              := DOM.Core.Elements.Get_Attribute (Elem => Cur_Child,
                                                                  Name => "variable");
                           Var_Value : constant String
                              := Expressions.Case_Expression.Get_Value_Of_Reference_Debug
                              (Ref_Name    => Var_Name,
                               Node_Access => Node_Access);
                           Matched_Others : constant Boolean
                              := (if DOM.Core.Nodes.Node_Name
                                     (N => Matching_Option_Node) = "others"
                                     then True
                                     else False);
                        begin
                           Log_Index := Xmldebuglog.Add_Conditional_Transaction
                              (Conditional_Node => Cur_Child,
                               Coditional_Kind  => Xmldebuglog.CASECOND,
                               Var_Name         => Var_Name,
                               Var_Value        => Var_Value,
                               Matched          => True,
                               Matched_Others   => Matched_Others);
                        end;
                     end if;

                     Transfer_Children
                        (Old_Parent        => Matching_Option_Node,
                         New_Parent        => Parent,
                         Ref_In_New_Parent => Cur_Child,
                         Debug_Active      => Debug_Active,
                         Ancestor_For_Log  => Cur_Child,
                         Transaction_Index => Log_Index);
                  else
                     if Debug_Active then
                        declare
                           Var_Name : constant String
                              := DOM.Core.Elements.Get_Attribute (Elem => Cur_Child,
                                                                  Name => "variable");
                           Var_Value : constant String
                              := Expressions.Case_Expression.Get_Value_Of_Reference_Debug
                              (Ref_Name    => Var_Name,
                               Node_Access => Node_Access);
                        begin
                           Log_Index := Xmldebuglog.Add_Conditional_Transaction
                              (Conditional_Node => Cur_Child,
                               Coditional_Kind  => Xmldebuglog.CASECOND,
                               Var_Name         => Var_Name,
                               Var_Value        => Var_Value,
                               Matched          => False,
                               Matched_Others   => False);
                        end;
                     end if;
                  end if;

                  if Debug_Active then
                     Xmldebuglog.Remove_Log_Of_Subtree (Node => Cur_Child);
                  end if;

                  Dummy := DOM.Core.Nodes.Remove_Child
                     (N         => Parent,
                      Old_Child => Cur_Child);
                  DOM.Core.Nodes.Free (N => Dummy);
               end;
            end if;
         exception
            when others =>
               if Debug_Active then
                  Mulog.Log
                     (Msg => "Error when evaluating conditionals. "
                         & Xmldebuglog.Get_Log_For_Error_Message (Node => Cur_Child));
               end if;
               raise;
         end;
         Cur_Child := Next_Child;
      end loop;
   end Evaluate;

   -------------------------------------------------------------------------

   procedure Expand (Policy       : Muxml.XML_Data_Type;
                     Debug_Active : Boolean := False)
   is
      Config_Nodes : constant DOM.Core.Node_List
                   := McKae.XML.XPath.XIA.XPath_Query
                        (N     => Policy.Doc,
                         XPath => "/*/config/*");
      Sections     : constant DOM.Core.Node_List
                   := McKae.XML.XPath.XIA.XPath_Query
                        (N     => Policy.Doc,
                         XPath => "/*");
      Node_Access : Expressions.Access_Hashmaps_Type;

      ----------------------------------------------------------------------

      --  Populate Node_Access with elements from Config_Nodes
      --  for fast access to name-values pairs.
      procedure Initialize_Node_Access
         (Node_Access  : in out Expressions.Access_Hashmaps_Type;
          Config_Nodes :        DOM.Core.Node_List;
          Debug_Active :        Boolean);

      ----------------------------------------------------------------------

      procedure Initialize_Node_Access
         (Node_Access  : in out Expressions.Access_Hashmaps_Type;
          Config_Nodes :        DOM.Core.Node_List;
          Debug_Active :        Boolean)
      is
      begin
         for I in 0 .. DOM.Core.Nodes.Length (List => Config_Nodes) - 1 loop
            declare
               Node : constant DOM.Core.Node
                  := DOM.Core.Nodes.Item (List  => Config_Nodes, Index => I);
               Node_Type : constant String
                  := DOM.Core.Nodes.Node_Name (N => Node);
               Node_Name : constant String
                  := DOM.Core.Elements.Get_Attribute
                  (Elem => Node,
                   Name => "name");
               Node_Raw_Value : constant String
                  := DOM.Core.Elements.Get_Attribute
                  (Elem => Node,
                   Name => "value");
            begin
               if Node_Type = "boolean" then
                  Node_Access.Output_Boolean.Insert
                     (Key      => Node_Name,
                      New_Item => Boolean'Value (Node_Raw_Value));
               elsif Node_Type = "integer" then
                  Node_Access.Output_Integer.Insert
                     (Key      => Node_Name,
                      New_Item => Integer'Value (Node_Raw_Value));
               elsif Node_Type = "string" then
                  Node_Access.Output_String.Insert
                     (Key      => Node_Name,
                      New_Item => Node_Raw_Value);
               else
                  if Debug_Active then
                     Mulog.Log (Msg => Xmldebuglog.Get_Log_For_Error_Message
                                   (Node => Node));
                  end if;
                  raise Expressions.Invalid_Expression with
                     "Found invalid node with name '"
                     & Node_Name
                     & "' when loading config variables to expand conditionals";
               end if;
            end;
         end loop;
      end Initialize_Node_Access;

   begin
      --  Populate Node_Access for fast access to name-values pairs.
      Initialize_Node_Access
         (Node_Access  => Node_Access,
          Config_Nodes => Config_Nodes,
          Debug_Active => Debug_Active);

      for I in 0 .. DOM.Core.Nodes.Length (List => Sections) - 1 loop
         declare
            Cur_Section : constant DOM.Core.Node
                        := DOM.Core.Nodes.Item (List  => Sections,
                                                Index => I);
         begin
            Evaluate (Policy       => Policy,
                      Config       => Config_Nodes,
                      Parent       => Cur_Section,
                      Node_Access  => Node_Access,
                      Debug_Active => Debug_Active);
         end;
      end loop;
   end Expand;

   -------------------------------------------------------------------------

   procedure Transfer_Children
      (Old_Parent        : DOM.Core.Node;
       New_Parent        : DOM.Core.Node;
       Ref_In_New_Parent : DOM.Core.Node;
       Debug_Active      : Boolean       := False;
       Ancestor_For_Log  : DOM.Core.Node := null;
       Transaction_Index : Xmldebuglog.Transaction_Log_Index_Type
                         := Xmldebuglog.Null_Ref_Index)
   is
      use type DOM.Core.Node;

      Cur_Child : DOM.Core.Node;
   begin
      loop
         Cur_Child := DOM.Core.Nodes.First_Child (N => Old_Parent);
         exit when Cur_Child = null;

         --  Insert_Before can be used to move nodes (by specification).
         Cur_Child := DOM.Core.Nodes.Insert_Before
            (N         => New_Parent,
             New_Child => Cur_Child,
             Ref_Child => Ref_In_New_Parent);
         if Debug_Active then
            Xmldebuglog.Add_Log_For_Node
               (Node      => Cur_Child,
                Ancestor => Ancestor_For_Log,
                TA_Number => Transaction_Index);
         end if;
      end loop;
   end Transfer_Children;

end Mutools.Conditionals;
