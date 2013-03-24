------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Unicode.CES;   use Unicode.CES;
with Sax.Encodings; use Sax.Encodings;
with Unicode.Names.Basic_Latin; use Unicode.Names.Basic_Latin;
with Unicode;       use Unicode;
with Sax.Symbols;   use Sax.Symbols;
with Sax.Utils;     use Sax.Utils;

package body DOM.Core is
   use Nodes_Htable, Symbol_Table_Pointers;

   Node_List_Growth_Factor : Float := Default_Node_List_Growth_Factor;

   ---------------------------------
   -- Set_Node_List_Growth_Factor --
   ---------------------------------

   procedure Set_Node_List_Growth_Factor (Factor : Float) is
   begin
      Node_List_Growth_Factor := Factor;
   end Set_Node_List_Growth_Factor;

   ---------------------
   -- Create_Document --
   ---------------------

   function Create_Document
     (Implementation : DOM_Implementation;
      NameSpace_URI  : DOM_String := "";
      Qualified_Name : DOM_String := "";
      Doc_Type       : Node := null;
      Symbols        : Sax.Utils.Symbol_Table := Sax.Utils.No_Symbol_Table)
      return Node
   is
      pragma Warnings (Off, NameSpace_URI);
      pragma Warnings (Off, Qualified_Name);
      Sym : Sax.Utils.Symbol_Table := Symbols;
      Tmp : Symbol_Table_Access;
   begin
      pragma Assert
        (Doc_Type = null or else Doc_Type.Node_Type = Document_Type_Node);
      if Sym = No_Symbol_Table then
         Tmp := new Symbol_Table_Record;
         Sym := Symbol_Table_Pointers.Allocate (Tmp);
      end if;
      return new Node_Record'
        (Node_Type      => Document_Node,
         Parent         => null,
         Doc_Children   => Null_List,
         Doc_Type       => Doc_Type,
         Parent_Is_Owner => False,
         Implementation => Implementation,
         Ids            => null,
         Symbols        => Sym);
   end Create_Document;

   -----------------
   -- Has_Feature --
   -----------------

   function Has_Feature
     (Implementation : DOM_Implementation;
      Feature        : DOM_String;
      Version        : String := "2.0") return Boolean
   is
      pragma Warnings (Off, Implementation);
   begin
      return Feature = "XML" and then Version = "2.0";
   end Has_Feature;

   ------------
   -- Append --
   ------------

   procedure Append (List : in out Node_List; N : Node) is
      Old : Node_Array_Access := List.Items;
   begin
      if Old = null or else Old'Last = List.Last then
         List.Items := new Node_Array
           (0 .. List.Last + 1
              + (Integer'Max (0,
                   Integer (Float (List.Last) * Node_List_Growth_Factor))));
         if Old /= null then
            List.Items (0 .. List.Last) := Old.all;
            Free (Old);
         end if;
      end if;
      List.Last := List.Last + 1;
      List.Items (List.Last) := N;
   end Append;

   ------------
   -- Remove --
   ------------

   procedure Remove (List : in out Node_List; N : Node) is
   begin
      if List.Items = null or else List.Last = 0 then
         Free (List.Items);
         List.Last := -1;
      else
         for J in 0 .. List.Last loop
            if List.Items (J) = N then
               List.Items (J .. List.Last - 1) :=
                 List.Items (J + 1 .. List.Last);
               List.Last := List.Last - 1;
               return;
            end if;
         end loop;
      end if;
   end Remove;

   ----------
   -- Free --
   ----------

   procedure Free (List : in out Node_List) is
   begin
      Free (List.Items);
   end Free;

   --------------------
   -- Qualified_Name --
   --------------------

   function Qualified_Name (N : Node_Name_Def) return DOM_String is
   begin
      pragma Assert (N.Local_Name /= No_Symbol);
      if N.Prefix = No_Symbol or N.Prefix = Empty_String then
         return Get (N.Local_Name).all;
      else
         return Get (N.Prefix).all & Colon_Sequence & Get (N.Local_Name).all;
      end if;
   end Qualified_Name;

   ----------------
   -- Set_Prefix --
   ----------------

   procedure Set_Prefix (N : in out Node_Name_Def; Prefix : Symbol) is
   begin
      --  ??? We're supposed to check that Prefix is valid, and raise
      --  Invalid_Character_Err otherwise

      N.Prefix := Prefix;
   end Set_Prefix;

   -------------------------
   -- From_Qualified_Name --
   -------------------------

   function From_Qualified_Name
     (Doc       : Document;
      Symbols   : Sax.Utils.Symbol_Table;
      Name      : Sax.Symbols.Symbol;
      Namespace : Sax.Symbols.Symbol := Sax.Symbols.No_Symbol)
      return Node_Name_Def
   is
      N         : constant Cst_Byte_Sequence_Access := Get (Name);
      Index     : Natural := N'First;
      Colon_Pos : Natural;
      C         : Unicode_Char := 0;
   begin
      --  ??? Test for Invalid_Character_Err
      --  ??? Must convert Tag_Name to uppercase for HTML documents
      --  ??? Test for Namespace_Err

      while Index <= N'Last loop
         Colon_Pos := Index;
         Encoding.Read (N.all, Index, C);
         exit when C = Colon;
      end loop;

      if C = Colon then
         return
           (Prefix     => Find (Doc.Symbols, N (N'First .. Colon_Pos - 1)),
            Local_Name => Find (Doc.Symbols, N (Index .. N'Last)),
            Namespace  => Namespace);

      elsif Symbols /= Doc.Symbols then
         return
           (Prefix     => Find (Doc.Symbols, Get (Name).all),
            Local_Name => Find (Doc.Symbols, Get (Namespace).all),
            Namespace  => Namespace);
      else
         return
           (Prefix     => No_Symbol,
            Local_Name => Name,
            Namespace  => Namespace);
      end if;
   end From_Qualified_Name;

   ----------
   -- Free --
   ----------

   procedure Free (N : in out Node_String) is
      pragma Unreferenced (N);
   begin
      null;
   end Free;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (N : Node_String) return Symbol is
   begin
      return N.Key;
   end Get_Key;

   ---------------------
   -- Document_Add_Id --
   ---------------------

   procedure Document_Add_Id
     (Doc  : Document;
      Id   : Symbol;
      Elem : Element) is
   begin
      if Doc.Ids = null then
         Doc.Ids := new Nodes_Htable.HTable (203);
      end if;

      Set (Doc.Ids.all, (N => Node (Elem), Key => Id));
   end Document_Add_Id;

   ------------------------
   -- Document_Remove_Id --
   ------------------------

   procedure Document_Remove_Id
     (Doc : Document;
      Id  : Symbol) is
   begin
      if Doc.Ids /= null then
         Remove (Doc.Ids.all, Id);
      end if;
   end Document_Remove_Id;

end DOM.Core;
