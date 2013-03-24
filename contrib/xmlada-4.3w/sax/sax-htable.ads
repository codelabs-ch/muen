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

pragma Ada_05;

with Interfaces;

generic
   type Element is private;
   --  The type of element to be stored

   Empty_Element : Element;

   with procedure Free (Elmt : in out Element) is null;
   --  Free the memory used by Elmt

   type Key (<>) is limited private;
   with function Get_Key (E : Element)  return Key;
   with function Hash    (F : Key)      return Interfaces.Unsigned_32;
   with function Equal   (F1, F2 : Key) return Boolean;

package Sax.HTable is

   type HTable (Size : Interfaces.Unsigned_32) is private;
   type Element_Ptr is access all Element;

   procedure Reset (Hash_Table : in out HTable);
   --  Resets the hash table by freeing all the elements

   procedure Set (Hash_Table : in out HTable; E : Element);
   procedure Set_With_Hash
     (Hash_Table : in out HTable;
      E          : Element;
      Hashed     : Interfaces.Unsigned_32);
   --  Insert the element pointer in the HTable.
   --  The second version is useful if you want to add an element only if it
   --  doesn't exist yet in the table (so a [Get] followed by a [Set], since
   --  you can then compute the hash only once).

   function Get (Hash_Table : HTable; K : Key) return Element;
   function Get_Ptr (Hash_Table : HTable; K : Key) return Element_Ptr;
   function Get_Ptr_With_Hash
     (Hash_Table : HTable;
      K          : Key;
      Hashed     : Interfaces.Unsigned_32) return Element_Ptr;
   --  Returns the latest inserted element pointer with the given Key
   --  or Empty_Element if none.

   procedure Remove (Hash_Table : in out HTable; K : Key);
   --  Removes the latest inserted element pointer associated with the
   --  given key if any, does nothing if none.

   generic
      with function Preserve (Elem : Element) return Boolean;
   procedure Remove_All (Hash_Table : in out HTable);
   --  Remove all elements for which [Preserve] returns False

   type Iterator is private;
   No_Iterator : constant Iterator;

   function First (Hash_Table : HTable) return Iterator;
   --  Return the first element in the table
   --  There is no guarantee that 2 calls to this function will return the same
   --  element.

   procedure Next
     (Hash_Table : HTable;
      Iter       : in out Iterator);
   --  Move to the next element in the htash table, that hasn't been returned
   --  yet. All the elements in the table will eventually be visited if there
   --  is no call to Set since the call to First.
   --  Iter is set to No_Iterator if there is no more element in the table.

   function Current (Iter : Iterator) return Element;
   --  Return the element pointed to by Iter

private

   type Htable_Item;
   type Item_Ptr is access Htable_Item;
   type Htable_Item is record
      Elem : aliased Element;
      Next : Item_Ptr;
   end record;

   type First_Item is record
      Elem : aliased Element;
      Next : Item_Ptr;
      Set  : Boolean := False;
   end record;

   type Item_Array is array (Interfaces.Unsigned_32 range <>) of First_Item;
   --  The first element is not an Item_Ptr to save one call to malloc for each
   --  first key in buckets.

   type HTable (Size : Interfaces.Unsigned_32) is record
      Table : Item_Array (1 .. Size);
   end record;

   type Iterator is record
      Index : Interfaces.Unsigned_32;
      Elem  : Element_Ptr;
      Item  : Item_Ptr;
   end record;

   No_Iterator : constant Iterator :=
                   (Interfaces.Unsigned_32'Last, null, null);
end Sax.HTable;
