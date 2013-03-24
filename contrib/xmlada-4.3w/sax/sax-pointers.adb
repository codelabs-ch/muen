------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2007-2012, AdaCore                     --
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
with GNAT.Task_Lock;
with Interfaces;   use Interfaces;

package body Sax.Pointers is

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Root_Encapsulated'Class, Root_Encapsulated_Access);

--     function Sync_Add_And_Fetch
--       (Ptr   : access Interfaces.Integer_32;
--        Value : Interfaces.Integer_32) return Interfaces.Integer_32;
--     pragma Import (Intrinsic, Sync_Add_And_Fetch, "__sync_add_and_fetch_4");
   --  Increment Ptr by Value. This is task safe (either using a lock or
   --  intrinsic atomic operations). Returns the new value (as set, it
   --  might already have been changed by another by the time this function
   --  returns.

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Root_Encapsulated) is
      pragma Unreferenced (Data);
   begin
      null;
   end Free;

   --------------------
   -- Smart_Pointers --
   --------------------

   package body Smart_Pointers is

      --------------
      -- Allocate --
      --------------

      function Allocate (Data : Encapsulated'Class) return Pointer is
         Tmp : constant Encapsulated_Access := new Encapsulated'Class'(Data);
      begin
         return Allocate (Tmp);
      end Allocate;

      function Allocate (Data : access Encapsulated'Class) return Pointer is
      begin
         return
           (Ada.Finalization.Controlled with Root_Encapsulated_Access (Data));
      end Allocate;

      ---------
      -- Get --
      ---------

      function Get (P : Pointer) return Encapsulated_Access is
      begin
         return Encapsulated_Access (P.Data);
      end Get;

      ---------
      -- "=" --
      ---------

      function "=" (P1, P2 : Pointer) return Boolean is
      begin
         return P1.Data = P2.Data;
      end "=";

      --------------
      -- Finalize --
      --------------

      procedure Finalize (P : in out Pointer) is
         Data : Root_Encapsulated_Access := P.Data;
      begin
         --  Make Finalize idempotent, since it could be called several
         --  times for the same instance (RM 7.6.1(24)

         P.Data := null;

         --  Test if refcount is > 0, in case we are already freeing this
         --  element. That shouldn't happen, though, since we are not in a
         --  multi-tasking environment.

         if Data /= null then
            --  GNATCOLL uses a more efficient implementation for platforms
            --  providing the gcc builtin. Here, we keep things simpler,
            --  although less efficient.
            GNAT.Task_Lock.Lock;
            Data.Refcount := Data.Refcount - 1;
            if Data.Refcount = 0 then
               GNAT.Task_Lock.Unlock;
               Free (Data.all);
               Unchecked_Free (Data);
            else
               GNAT.Task_Lock.Unlock;
            end if;
         end if;
      end Finalize;

      ------------
      -- Adjust --
      ------------

      procedure Adjust (P : in out Pointer) is
         Dummy : Integer_32;
         pragma Unreferenced (Dummy);
         Data : Root_Encapsulated_Access := P.Data;
      begin
         if Data /= null then
            GNAT.Task_Lock.Lock;
            Data.Refcount := Data.Refcount + 1;
            GNAT.Task_Lock.Unlock;
         end if;
      end Adjust;

   end Smart_Pointers;

end Sax.Pointers;
