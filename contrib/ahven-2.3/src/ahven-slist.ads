--
-- Copyright (c) 2008-2009 Tero Koskinen <tero.koskinen@iki.fi>
--
-- Permission to use, copy, modify, and distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--

with Ada.Finalization;

generic
   type Element_Type is private;
package Ahven.SList is
   type List is new Ada.Finalization.Controlled with private;
   type Cursor is private;

   subtype Count_Type is Natural;

   Invalid_Cursor : exception;

   List_Full : exception;
   -- Thrown when the size of the list exceeds Count_Type'Last.

   Empty_List : constant List;

   procedure Append (Target : in out List; Node_Data : Element_Type);
   -- Append an element at the end of the list.
   --
   -- Raises List_Full if the list has already Count_Type'Last items.

   procedure Clear (Target : in out List);
   -- Remove all elements from the list.

   function First (Target : List) return Cursor;
   -- Return a cursor to the first element of the list.

   function Next (Position : Cursor) return Cursor;
   -- Move the cursor to point to the next element on the list.

   function Data (Position : Cursor) return Element_Type;
   -- Return element pointed by the cursor.

   function Is_Valid (Position : Cursor) return Boolean;
   -- Tell the validity of the cursor. The cursor
   -- will become invalid when you iterate it over
   -- the last item.

   function Length (Target : List) return Count_Type;
   -- Return the length of the list.

   generic
      with procedure Action (T : in out Element_Type) is <>;
   procedure For_Each (Target : List);
   -- A generic procedure for walk through every item
   -- in the list and call Action procedure for them.

private
   type Node;
   type Node_Access is access Node;
   type Cursor is new Node_Access;

   procedure Remove (Ptr : Node_Access);
   -- A procedure to release memory pointed by Ptr.

   type Node is record
      Next : Node_Access := null;
      Data : Element_Type;
   end record;

   type List is new Ada.Finalization.Controlled with record
      First : Node_Access := null;
      Last  : Node_Access := null;
      Size  : Count_Type  := 0;
   end record;

   procedure Initialize (Target : in out List);
   procedure Finalize   (Target : in out List);
   procedure Adjust     (Target : in out List);

   Empty_List : constant List :=
     (Ada.Finalization.Controlled with First => null,
                                       Last  => null,
                                       Size  => 0);
end Ahven.SList;
