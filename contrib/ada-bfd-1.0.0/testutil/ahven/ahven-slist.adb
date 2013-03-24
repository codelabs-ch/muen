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
with Ada.Unchecked_Deallocation;

package body Ahven.SList is
   procedure Remove (Ptr : Node_Access) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Node,
                                        Name   => Node_Access);
      My_Ptr : Node_Access := Ptr;
   begin
      Ptr.Next := null;
      Free (My_Ptr);
   end Remove;

   procedure Append (Target : in out List;
                     Node_Data : Element_Type) is
      New_Node : Node_Access  := null;
   begin
      if Target.Size = Count_Type'Last then
         raise List_Full;
      end if;

      New_Node := new Node'(Data => Node_Data, Next => null);

      if Target.Last = null then
         Target.First := New_Node;
      else
         Target.Last.Next := New_Node;
      end if;
      Target.Last := New_Node;

      Target.Size := Target.Size + 1;
   end Append;

   procedure Clear (Target : in out List) is
      Current_Node : Node_Access := Target.First;
      Next_Node : Node_Access := null;
   begin
      while Current_Node /= null loop
         Next_Node := Current_Node.Next;
         Remove (Current_Node);
         Current_Node := Next_Node;
      end loop;

      Target.First := null;
      Target.Last := null;
      Target.Size := 0;
   end Clear;

   function First (Target : List) return Cursor is
   begin
      return Cursor (Target.First);
   end First;

   function Next (Position : Cursor) return Cursor is
   begin
      if Position = null then
         raise Invalid_Cursor;
      end if;
      return Cursor (Position.Next);
   end Next;

   function Data (Position : Cursor) return Element_Type is
   begin
      if Position = null then
         raise Invalid_Cursor;
      end if;

      return Position.Data;
   end Data;

   function Is_Valid (Position : Cursor) return Boolean is
   begin
      return Position /= null;
   end Is_Valid;

   function Length (Target : List) return Count_Type is
   begin
      return Target.Size;
   end Length;

   procedure For_Each (Target : List) is
      Current_Node : Node_Access := Target.First;
   begin
      while Current_Node /= null loop
         Action (Current_Node.Data);
         Current_Node := Current_Node.Next;
      end loop;
   end For_Each;

   procedure Initialize (Target : in out List) is
   begin
      Target.Last := null;
      Target.First := null;
      Target.Size := 0;
   end Initialize;

   procedure Finalize (Target : in out List) is
   begin
      Clear (Target);
   end Finalize;

   procedure Adjust (Target : in out List) is
      Target_Last  : Node_Access := null;
      Target_First : Node_Access := null;
      Current      : Node_Access := Target.First;
      New_Node     : Node_Access;
   begin
      -- Recreate the list using the same data
      while Current /= null loop
         New_Node := new Node'(Data => Current.Data, Next => null);

         if Target_Last = null then
            Target_First := New_Node;
         else
            Target_Last.Next := New_Node;
         end if;
         Target_Last := New_Node;

         Current := Current.Next;
      end loop;
      Target.First := Target_First;
      Target.Last := Target_Last;

      -- No need to adjust size, it is same as before copying
   end Adjust;
end Ahven.SList;
