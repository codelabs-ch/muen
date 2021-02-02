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

private with Ada.Strings.Unbounded.Hash;
private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Hashed_Maps;

package Stackcheck.Types
is

   --  Subprogram information related to stack usage.
   type Subprogram_Type is private;

   Null_Subprogram : constant Subprogram_Type;

   --  Create subprogram with given information.
   function Create
     (Name          : String;
      Stack_Usage   : Natural;
      Dynamic_Stack : Boolean;
      Bounded_Stack : Boolean)
      return Subprogram_Type;

   --  Returns the name of the given subprogram.
   function Get_Name (Subprogram : Subprogram_Type) return String;

   --  Returns the stack usage of the given subprogram.
   function Get_Own_Stack_Usage (Subprogram : Subprogram_Type) return Natural;

   --  Returns the worst-case stack usage of the subprogram by considering all
   --  calls made.
   function Get_Max_Stack_Usage (Subprogram : Subprogram_Type) return Natural;

   --  Set worst-case stack usage of given subprogram to specified value.
   procedure Set_Max_Stack_Usage
     (Subprogram : in out Subprogram_Type;
      Value      :        Natural);

   --  Add call with given name to subprogram.
   procedure Add_Call
     (Subprogram  : in out Subprogram_Type;
      Callee_Name :        String);

   --  Return the number of calls of the given subprogram.
   function Get_Call_Count (Subprogram : Subprogram_Type) return Natural;

   --  Invokes the specified Process procedure for each call of the given
   --  subprogram.
   procedure Iterate_Calls
     (Subprogram : Subprogram_Type;
      Process    : not null access procedure (Callee : String));

   --  Returns True if the subprogram node is currently active.
   function Is_Active (Node : Subprogram_Type) return Boolean;

   --  Set the active flag of the given suprogram node to the specified state.
   procedure Set_Active
     (Node  : in out Subprogram_Type;
      State :        Boolean);

   --  Returns True if the subprogram node has been processed.
   function Is_Done (Node : Subprogram_Type) return Boolean;

   --  Set the done flag of the given suprogram node to the specified state.
   procedure Set_Done
     (Node  : in out Subprogram_Type;
      State :        Boolean);

   --  Returns True if the subprogram node's stack is dynamic.
   function Has_Dynamic_Stack (Node : Subprogram_Type) return Boolean;

   --  Returns True if the subprogram node's stack is dynamic, but bounded.
   function Has_Bounded_Stack (Node : Subprogram_Type) return Boolean;

   --  Representation of a control-flow graph.
   type Control_Flow_Graph_Type is private;

   --  Add specified subprogram node to given control flow graph. If a
   --  subprogram with the same name already exists an exception is raised.
   procedure Add_Node
     (Graph      : in out Control_Flow_Graph_Type;
      Subprogram :        Subprogram_Type);

   --  Invokes the specified Process procedure for the subprogram specified by
   --  name of the given control flow graph. An exception is raised if no
   --  subprogram with the given name is present.
   procedure Update_Node
     (Graph   : in out Control_Flow_Graph_Type;
      Name    :        String;
      Process : not null access procedure (Node : in out Subprogram_Type));

   --  Invokes the specified Process procedure for each subprogram node in the
   --  given control flow graph.
   procedure Iterate
     (Graph   : in out Control_Flow_Graph_Type;
      Process : not null access procedure (Node : in out Subprogram_Type));

   --  Add a call from specified source to target in given control flow graph.
   --  An exception is raised if the given source subprogram is not present.
   procedure Add_Call
     (Graph       : in out Control_Flow_Graph_Type;
      Source_Name :        String;
      Target_Name :        String);

   --  Return the maximum stack usage of the node specified by name in the
   --  given control flow graph. An exception is raised if no subprogram with
   --  given name is present.
   function Get_Max_Stack_Usage
     (Graph     : in out Control_Flow_Graph_Type;
      Node_Name :        String)
      return Natural;

   --  Determine the maximumg stack usage of all subprogram nodes present in
   --  the specified control flow graph.
   procedure Calculate_Stack_Usage (Graph : in out Control_Flow_Graph_Type);

   Duplicate_Subprogram : exception;
   Missing_Subprogram   : exception;
   Circular_Graph       : exception;

private

   use Ada.Strings.Unbounded;

   package List_of_Subprogram_Calls is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Unbounded_String);
   package LOSC renames List_of_Subprogram_Calls;

   type Subprogram_Type is record
      Name            : Unbounded_String;
      Active_Flag     : Boolean;
      Done_Flag       : Boolean;
      Own_Stack_Usage : Natural;
      Max_Stack_Usage : Natural;
      Calls           : LOSC.List;
      Dynamic_Flag    : Boolean;
      Bounded_Flag    : Boolean;
   end record;

   Null_Subprogram : constant Subprogram_Type
     := (Name            => Null_Unbounded_String,
         Active_Flag     => False,
         Done_Flag       => False,
         Own_Stack_Usage => 0,
         Max_Stack_Usage => 0,
         Calls           => LOSC.Empty_List,
         Dynamic_Flag    => False,
         Bounded_Flag    => False);

   function Equal_Name (Left, Right : Subprogram_Type) return Boolean
   is (Left.Name = Right.Name);

   package Map_Of_Subprogram_Nodes is new Ada.Containers.Hashed_Maps
     (Key_Type     => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type => Subprogram_Type,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => Ada.Strings.Unbounded."=",
      "="             => Equal_Name);
   package MOSN renames Map_Of_Subprogram_Nodes;

   type Control_Flow_Graph_Type is record
      Nodes : MOSN.Map;
   end record;

end Stackcheck.Types;
