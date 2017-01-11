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

   function Create
     (Name        : String;
      Stack_Usage : Natural)
      return Subprogram_Type
   is
   begin
      return Subprogram_Type'(Name            => To_Unbounded_String (Name),
                              Active_Flag     => False,
                              Done_Flag       => False,
                              Own_Stack_Usage => Stack_Usage,
                              Max_Stack_Usage => Stack_Usage,
                              Calls           => LOSC.Empty_List);
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

   function Get_Name (Subprogram : Subprogram_Type) return String
   is
   begin
      return To_String (Subprogram.Name);
   end Get_Name;

   -------------------------------------------------------------------------

   function Get_Stack_Usage (Subprogram : Subprogram_Type) return Natural
   is
   begin
      return Subprogram.Own_Stack_Usage;
   end Get_Stack_Usage;

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

end Stackcheck.Types;
