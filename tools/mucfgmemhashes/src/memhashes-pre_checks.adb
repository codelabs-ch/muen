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

with Ada.Strings.Unbounded;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with Mucfgcheck.Files;
with Mucfgcheck.Validation_Errors;

package body Memhashes.Pre_Checks
is

   -------------------------------------------------------------------------

   procedure Clear renames Check_Procs.Clear;

   -------------------------------------------------------------------------

   function Get_Count return Natural renames Check_Procs.Get_Count;

   -------------------------------------------------------------------------

   procedure Hash_References (Data : Muxml.XML_Data_Type)
   is
      --  Returns the error message for a given reference node.
      procedure Error_Msg
        (Node    :     DOM.Core.Node;
         Err_Str : out Ada.Strings.Unbounded.Unbounded_String;
         Fatal   : out Boolean);

      --  Returns True if the left node's 'memory' attribute matches the 'name'
      --  attribute of the right node.
      function Is_Valid_Hash_Ref (Left, Right : DOM.Core.Node) return Boolean;

      ----------------------------------------------------------------------

      procedure Error_Msg
        (Node    :     DOM.Core.Node;
         Err_Str : out Ada.Strings.Unbounded.Unbounded_String;
         Fatal   : out Boolean)
      is
         use type DOM.Core.Node;

         Name : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => DOM.Core.Nodes.Parent_Node (N => Node),
              Name => "name");
         Ref_Name : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Node,
              Name => "memory");
      begin
         Err_Str := Ada.Strings.Unbounded.To_Unbounded_String
           ("Physical memory region '" & Ref_Name & "' referenced by "
            & "hashRef of memory region '" & Name & "' does not exist");
         Fatal := False;
      end Error_Msg;

      ----------------------------------------------------------------------

      function Is_Valid_Hash_Ref (Left, Right : DOM.Core.Node) return Boolean
      is
         Ref_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Left,
            Name => "memory");
         Phy_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Right,
            Name => "name");
      begin
         return Ref_Name = Phy_Name;
      end Is_Valid_Hash_Ref;
   begin
      Mucfgcheck.For_Each_Match
        (XML_Data     => Data,
         Source_XPath => "/system/memory/memory/hashRef",
         Ref_XPath    => "/system/memory/memory",
         Log_Message  => "hash reference(s)",
         Error        => Error_Msg'Access,
         Match        => Is_Valid_Hash_Ref'Access);
   end Hash_References;

   -------------------------------------------------------------------------

   procedure Register_All
   is
   begin
      Check_Procs.Register (Process => Mucfgcheck.Files.Files_Exist'Access);
      Check_Procs.Register (Process => Mucfgcheck.Files.Files_Size'Access);
      Check_Procs.Register (Process => Hash_References'Access);
   end Register_All;

   -------------------------------------------------------------------------

   procedure Run
     (Data      : Muxml.XML_Data_Type;
      Input_Dir : String)
   is
   begin
      Mucfgcheck.Files.Set_Input_Directory (Dir => Input_Dir);
      Check_Procs.Run (Data => Data);
      Mucfgcheck.Validation_Errors.Check;
   end Run;

end Memhashes.Pre_Checks;
