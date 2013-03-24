------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
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

with Ada.Unchecked_Deallocation;
with Interfaces; use Interfaces;

package body Sax.HTable is

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Htable_Item, Item_Ptr);

   -----------
   -- Reset --
   -----------

   procedure Reset (Hash_Table : in out HTable) is
      Item, Tmp : Item_Ptr;
   begin
      for Index in Hash_Table.Table'Range loop
         if Hash_Table.Table (Index).Set then
            Free (Hash_Table.Table (Index).Elem);
            Item := Hash_Table.Table (Index).Next;

            while Item /= null loop
               Free (Item.Elem);
               Tmp := Item;
               Item := Item.Next;
               Unchecked_Free (Tmp);
            end loop;

            Hash_Table.Table (Index).Set := False;
         end if;
      end loop;
   end Reset;

   -------------------
   -- Set_With_Hash --
   -------------------

   procedure Set_With_Hash
     (Hash_Table : in out HTable;
      E          : Element;
      Hashed     : Interfaces.Unsigned_32)
   is
      Index : constant Unsigned_32 := Hashed mod Hash_Table.Size + 1;
      Item  : Item_Ptr;
   begin
      if Hash_Table.Table (Index).Set then
         --  Check whether we already have the item

         if Equal (Get_Key (Hash_Table.Table (Index).Elem), Get_Key (E)) then
            Free (Hash_Table.Table (Index).Elem);
            Hash_Table.Table (Index).Elem := E;
            return;
         else
            Item := Hash_Table.Table (Index).Next;
            while Item /= null loop
               if Equal (Get_Key (Item.Elem), Get_Key (E)) then
                  Free (Item.Elem);
                  Item.Elem := E;
                  return;
               end if;

               Item := Item.Next;
            end loop;
         end if;

         Hash_Table.Table (Index).Next := new Htable_Item'
           (Elem => E,
            Next => Hash_Table.Table (Index).Next);
      else
         Hash_Table.Table (Index) :=
           (Elem => E,
            Next => null,
            Set  => True);
      end if;
   end Set_With_Hash;

   ---------
   -- Set --
   ---------

   procedure Set (Hash_Table : in out HTable; E : Element) is
   begin
      Set_With_Hash (Hash_Table, E, Hash (Get_Key (E)));
   end Set;

   ---------
   -- Get --
   ---------

   function Get (Hash_Table : HTable; K : Key) return Element is
      Tmp : constant Element_Ptr := Get_Ptr (Hash_Table, K);
   begin
      if Tmp = null then
         return Empty_Element;
      else
         return Tmp.all;
      end if;
   end Get;

   -------------
   -- Get_Ptr --
   -------------

   function Get_Ptr (Hash_Table : HTable; K : Key) return Element_Ptr is
   begin
      return Get_Ptr_With_Hash (Hash_Table, K, Hash (K));
   end Get_Ptr;

   -----------------------
   -- Get_Ptr_With_Hash --
   -----------------------

   function Get_Ptr_With_Hash
     (Hash_Table : HTable;
      K          : Key;
      Hashed     : Interfaces.Unsigned_32) return Element_Ptr
   is
      H    : constant Unsigned_32 := Hashed mod Hash_Table.Size + 1;
      Elmt : Item_Ptr;
   begin
      if Hash_Table.Table (H).Set then
         if Equal (Get_Key (Hash_Table.Table (H).Elem), K) then
            return Hash_Table.Table (H).Elem'Unrestricted_Access;
         else
            Elmt := Hash_Table.Table (H).Next;

            while Elmt /= null loop
               if Equal (Get_Key (Elmt.Elem), K) then
                  return Elmt.Elem'Access;
               end if;

               Elmt := Elmt.Next;
            end loop;
         end if;
      end if;
      return null;
   end Get_Ptr_With_Hash;

   ------------
   -- Remove --
   ------------

   procedure Remove (Hash_Table : in out HTable; K : Key) is
      Index     : constant Unsigned_32 :=
                    Hash (K) mod Hash_Table.Size + 1;
      Elmt      : Item_Ptr;
      Next_Elmt : Item_Ptr;
   begin
      if not Hash_Table.Table (Index).Set then
         return;

      elsif Equal (Get_Key (Hash_Table.Table (Index).Elem), K) then
         Free (Hash_Table.Table (Index).Elem);
         Elmt := Hash_Table.Table (Index).Next;  --  second element in list
         if Elmt = null then
            Hash_Table.Table (Index).Set := False;
         else
            Hash_Table.Table (Index).Elem := Elmt.Elem;
            Hash_Table.Table (Index).Next := Elmt.Next;  --  to third element
            Unchecked_Free (Elmt); --  no longer needed, was copied to first
         end if;

      else
         Next_Elmt := Hash_Table.Table (Index).Next;
         loop
            if Next_Elmt = null then
               return;

            elsif Equal (Get_Key (Next_Elmt.Elem), K) then
               if Elmt = null then
                  Hash_Table.Table (Index).Next := Next_Elmt.Next;
               else
                  Elmt.Next := Next_Elmt.Next;
               end if;

               Free (Next_Elmt.Elem);
               Unchecked_Free (Next_Elmt);
               return;
            end if;

            Elmt := Next_Elmt;
            Next_Elmt :=  Elmt.Next;
         end loop;
      end if;
   end Remove;

   ----------------
   -- Remove_All --
   ----------------

   procedure Remove_All (Hash_Table : in out HTable) is
      Item, Item2 : Item_Ptr;
      Prev : Item_Ptr;
   begin
      for T in Hash_Table.Table'Range loop
         if Hash_Table.Table (T).Set then
            --  First examine the remaining of the list in that bucket
            Prev := null;
            Item := Hash_Table.Table (T).Next;
            while Item /= null loop
               if not Preserve (Item.Elem) then
                  if Prev = null then
                     Hash_Table.Table (T).Next := Item.Next;
                  else
                     Prev.Next := Item.Next;
                  end if;

                  Item2 := Item;
                  Item  := Item.Next;  --  Prev not changed
                  Free (Item2.Elem);
                  Unchecked_Free (Item2);
               else
                  Prev := Item;
                  Item := Item.Next;
               end if;
            end loop;

            --  Then examine the bucket itself
            if not Preserve (Hash_Table.Table (T).Elem) then
               Free (Hash_Table.Table (T).Elem);

               if Hash_Table.Table (T).Next = null then
                  Hash_Table.Table (T).Set := False;
               else
                  Item := Hash_Table.Table (T).Next;
                  Hash_Table.Table (T).Elem := Item.Elem;
                  Hash_Table.Table (T).Next := Item.Next;
                  Unchecked_Free (Item);
               end if;
            end if;
         end if;
      end loop;
   end Remove_All;

   -----------
   -- First --
   -----------

   function First (Hash_Table : HTable) return Iterator is
   begin
      for Index in Hash_Table.Table'Range loop
         if Hash_Table.Table (Index).Set then
            return (Index => Index,
                    Elem  => Hash_Table.Table (Index).Elem'Unrestricted_Access,
                    Item => null);
         end if;
      end loop;

      return No_Iterator;
   end First;

   ----------
   -- Next --
   ----------

   procedure Next (Hash_Table : HTable; Iter : in out Iterator) is
   begin
      pragma Assert (Iter /= No_Iterator);

      if Iter.Item = null then
         Iter.Item := Hash_Table.Table (Iter.Index).Next;
      else
         Iter.Item := Iter.Item.Next;
      end if;

      if Iter.Item /= null then
         Iter.Elem := Iter.Item.Elem'Unrestricted_Access;
         return;
      end if;

      loop
         Iter.Index := Unsigned_32'Succ (Iter.Index);
         exit when Iter.Index > Hash_Table.Table'Last
           or else Hash_Table.Table (Iter.Index).Set;
      end loop;

      if Iter.Index > Hash_Table.Table'Last then
         Iter := No_Iterator;
      else
         Iter.Item := null;
         Iter.Elem := Hash_Table.Table (Iter.Index).Elem'Unrestricted_Access;
      end if;
   end Next;

   -------------
   -- Current --
   -------------

   function Current (Iter : Iterator) return Element is
   begin
      return Iter.Elem.all;
   end Current;

end Sax.HTable;
