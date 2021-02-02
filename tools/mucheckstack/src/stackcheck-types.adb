--
--  Copyright (C) 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Exceptions;

package body Stackcheck.Types
is

   -------------------------------------------------------------------------

   procedure Add_Call
     (Graph       : in out Control_Flow_Graph_Type;
      Source_Name : String;
      Target_Name : String)
   is
      use type MOSN.Cursor;

      Source_Node : constant MOSN.Cursor := Graph.Nodes.Find
        (Key => To_Unbounded_String (Source_Name));

      --  Add target call to source subprogram.
      procedure Add_Call_To_Source
        (Key     :        Ada.Strings.Unbounded.Unbounded_String;
         Element : in out Subprogram_Type);

      ----------------------------------------------------------------------

      procedure Add_Call_To_Source
        (Key     :        Ada.Strings.Unbounded.Unbounded_String;
         Element : in out Subprogram_Type)
      is
      begin
         pragma Assert (Key = Source_Name);

         Add_Call (Subprogram  => Element,
                   Callee_Name => Target_Name);
      end Add_Call_To_Source;
   begin
      if Source_Node = MOSN.No_Element then
         raise Missing_Subprogram with "No subprogram with name '"
           & Source_Name &  "' in control flow graph";
      end if;

      Graph.Nodes.Update_Element (Position => Source_Node,
                                  Process  => Add_Call_To_Source'Access);
   end Add_Call;

   -------------------------------------------------------------------------

   procedure Add_Call
     (Subprogram  : in out Subprogram_Type;
      Callee_Name :        String)
   is
   begin
      Subprogram.Calls.Append
        (New_Item => To_Unbounded_String (Source => Callee_Name));
   end Add_Call;

   -------------------------------------------------------------------------

   procedure Add_Node
     (Graph      : in out Control_Flow_Graph_Type;
      Subprogram :        Subprogram_Type)
   is
   begin
      if Graph.Nodes.Contains (Key => Subprogram.Name) then
         raise Duplicate_Subprogram with "Node with name '"
           & To_String (Subprogram.Name) & "' already in graph";
      end if;

      Graph.Nodes.Insert (Key      => Subprogram.Name,
                          New_Item => Subprogram);
   end Add_Node;

   -------------------------------------------------------------------------

   procedure Calculate_Stack_Usage (Graph : in out Control_Flow_Graph_Type)
   is

      --  Determine the maximumg stack usage of the given subprogram node
      --  by recursively calculating the worst-case stack usage of all called
      --  suprograms.
      procedure Calculate_Stack_Usage (Node : in out Subprogram_Type);

      ----------------------------------------------------------------------

      procedure Calculate_Stack_Usage (Node : in out Subprogram_Type)
      is
         Cur_Max_Stack_Usage : Natural := 0;

         --  Calculate worst-case stack usage of callee subprogram specified by
         --  name.
         procedure Process_Calls (Callee : String);

         -------------------------------------------------------------------

         procedure Process_Calls (Callee : String)
         is
         begin
            Update_Node (Graph   => Graph,
                         Name    => Callee,
                         Process => Calculate_Stack_Usage'Access);

            declare
               Callee_Stack_Usage : constant Natural
                 := Get_Max_Stack_Usage
                   (Graph     => Graph,
                    Node_Name => Callee);
            begin
               if Callee_Stack_Usage > Cur_Max_Stack_Usage then
                  Cur_Max_Stack_Usage := Callee_Stack_Usage;
               end if;
            end;
         end Process_Calls;
      begin
         if Is_Active (Node => Node) then
            raise Circular_Graph with Get_Name (Subprogram => Node)
              & ": Recursive call detected";
         end if;

         if Is_Done (Node => Node) then
            return;
         end if;

         Set_Active (Node  => Node,
                     State => True);
         begin
            Iterate_Calls (Subprogram => Node,
                           Process    => Process_Calls'Access);
         exception
            when E : Circular_Graph =>
               raise Circular_Graph with Get_Name (Subprogram => Node)
                 & "->" & Ada.Exceptions.Exception_Message (X => E);
         end;

         Set_Max_Stack_Usage
           (Subprogram => Node,
            Value      => Get_Own_Stack_Usage
              (Subprogram => Node) + Cur_Max_Stack_Usage);
         Set_Done (Node  => Node,
                   State => True);
         Set_Active (Node  => Node,
                     State => False);
      end Calculate_Stack_Usage;
   begin
      Iterate (Graph   => Graph,
               Process => Calculate_Stack_Usage'Access);
   end Calculate_Stack_Usage;

   -------------------------------------------------------------------------

   function Create
     (Name          : String;
      Stack_Usage   : Natural;
      Dynamic_Stack : Boolean;
      Bounded_Stack : Boolean)
      return Subprogram_Type
   is
   begin
      return Subprogram_Type'(Name            => To_Unbounded_String (Name),
                              Active_Flag     => False,
                              Done_Flag       => False,
                              Own_Stack_Usage => Stack_Usage,
                              Max_Stack_Usage => Stack_Usage,
                              Calls           => LOSC.Empty_List,
                              Dynamic_Flag    => Dynamic_Stack,
                              Bounded_Flag    => Bounded_Stack);
   end Create;

   -------------------------------------------------------------------------

   function Get_Call_Count (Subprogram : Subprogram_Type) return Natural
   is
   begin
      return Natural (Subprogram.Calls.Length);
   end Get_Call_Count;

   -------------------------------------------------------------------------

   function Get_Max_Stack_Usage (Subprogram : Subprogram_Type) return Natural
   is
   begin
      return Subprogram.Max_Stack_Usage;
   end Get_Max_Stack_Usage;

   -------------------------------------------------------------------------

   function Get_Max_Stack_Usage
     (Graph     : in out Control_Flow_Graph_Type;
      Node_Name :        String)
      return Natural
   is
      use type MOSN.Cursor;

      Pos : constant MOSN.Cursor := Graph.Nodes.Find
        (Key => To_Unbounded_String (Node_Name));
   begin
      if Pos = MOSN.No_Element then
         raise Missing_Subprogram with "No subprogram with name '"
           & Node_Name &  "' in control flow graph";
      end if;

      return Get_Max_Stack_Usage
        (Subprogram => MOSN.Element (Position => Pos));
   end Get_Max_Stack_Usage;

   -------------------------------------------------------------------------

   function Get_Name (Subprogram : Subprogram_Type) return String
   is
   begin
      return To_String (Subprogram.Name);
   end Get_Name;

   -------------------------------------------------------------------------

   function Get_Own_Stack_Usage (Subprogram : Subprogram_Type) return Natural
   is
   begin
      return Subprogram.Own_Stack_Usage;
   end Get_Own_Stack_Usage;

   -------------------------------------------------------------------------

   function Has_Bounded_Stack (Node : Subprogram_Type) return Boolean
   is
   begin
      return Node.Bounded_Flag;
   end Has_Bounded_Stack;

   -------------------------------------------------------------------------

   function Has_Dynamic_Stack (Node : Subprogram_Type) return Boolean
   is
   begin
      return Node.Dynamic_Flag;
   end Has_Dynamic_Stack;

   -------------------------------------------------------------------------

   procedure Iterate
     (Graph   : in out Control_Flow_Graph_Type;
      Process : not null access procedure (Node : in out Subprogram_Type))
   is
      use type MOSN.Cursor;

      Pos : MOSN.Cursor := Graph.Nodes.First;

      --  Invoke process procedure for given element node.
      procedure Call_Process
        (Key     :        Unbounded_String;
         Element : in out Subprogram_Type);

      ----------------------------------------------------------------------

      procedure Call_Process
        (Key     :        Unbounded_String;
         Element : in out Subprogram_Type)
      is
         pragma Unreferenced (Key);
      begin
         Process (Node => Element);
      end Call_Process;
   begin
      while Pos /= MOSN.No_Element loop
         Graph.Nodes.Update_Element (Position => Pos,
                                     Process  => Call_Process'Access);
         Pos := MOSN.Next (Position => Pos);
      end loop;
   end Iterate;

   -------------------------------------------------------------------------

   procedure Iterate_Calls
     (Subprogram : Subprogram_Type;
      Process    : not null access procedure (Callee : String))
   is

      --  Call the process procedure for the element designated by the given
      --  cursor.
      procedure Call_Process (Cursor : LOSC.Cursor);

      ----------------------------------------------------------------------

      procedure Call_Process (Cursor : LOSC.Cursor)
      is
      begin
         Process (Callee => To_String (LOSC.Element (Position => Cursor)));
      end Call_Process;

   begin
      Subprogram.Calls.Iterate (Process => Call_Process'Access);
   end Iterate_Calls;

   -------------------------------------------------------------------------

   function Is_Active (Node : Subprogram_Type) return Boolean
   is (Node.Active_Flag);

   -------------------------------------------------------------------------

   function Is_Done (Node : Subprogram_Type) return Boolean
   is (Node.Done_Flag);

   -------------------------------------------------------------------------

   procedure Set_Active
     (Node  : in out Subprogram_Type;
      State :        Boolean)
   is
   begin
      Node.Active_Flag := State;
   end Set_Active;

   -------------------------------------------------------------------------

   procedure Set_Done
     (Node  : in out Subprogram_Type;
      State :        Boolean)
   is
   begin
      Node.Done_Flag := State;
   end Set_Done;

   -------------------------------------------------------------------------

   procedure Set_Max_Stack_Usage
     (Subprogram : in out Subprogram_Type;
      Value      :        Natural)
   is
   begin
      Subprogram.Max_Stack_Usage := Value;
   end Set_Max_Stack_Usage;

   -------------------------------------------------------------------------

   procedure Update_Node
     (Graph   : in out Control_Flow_Graph_Type;
      Name    :        String;
      Process : not null access procedure (Node : in out Subprogram_Type))
   is
      use type MOSN.Cursor;

      Pos : constant MOSN.Cursor := Graph.Nodes.Find
        (Key => To_Unbounded_String (Name));

      --  Invoke Process procedure for the given element.
      procedure Process_Node
        (Key     :        Unbounded_String;
         Element : in out Subprogram_Type);

      ----------------------------------------------------------------------

      procedure Process_Node
        (Key     :        Unbounded_String;
         Element : in out Subprogram_Type)
      is
         pragma Unreferenced (Key);
      begin
         Process (Node => Element);
      end Process_Node;
   begin
      if Pos = MOSN.No_Element then
         raise Missing_Subprogram with "No subprogram with name '" & Name
           &  "' in control flow graph";
      end if;

      Graph.Nodes.Update_Element (Position => Pos,
                                  Process  => Process_Node'Access);
   end Update_Node;

end Stackcheck.Types;
