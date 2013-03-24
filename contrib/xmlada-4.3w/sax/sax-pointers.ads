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

--  This package provides smart pointers, ie types that encapsulate a
--  standard pointer and provide automatic memory management for it.

with Ada.Finalization;
with Interfaces;

package Sax.Pointers is
   --  All types that can be encapsulated in a smart pointer must derive from
   --  a common ancestor, which adds a Refcount field to them. The advantage is
   --  that we require one less memory allocation *per pointer*, since
   --  encapsulating any type requires that the pointer is defined as
   --     type Pointed_Data is record
   --        Data     : access Encapsulated;
   --        Refcount : Natural := 1;
   --     end record;
   --     type Pointer is new Ada.Finalization.Controlled with record
   --        Data     : access Pointed_Data;
   --     end record;
   --  and the association with the refcount is less tight
   --
   --  On the other hand, the implementation chosen in this package requires
   --  the encapsulated type to be slightly bigger, since it includes the tag.
   --  But on the whole, memory usage is the same, with one less alloc/free, so
   --  more efficient.

   type Root_Encapsulated is abstract tagged private;
   type Root_Encapsulated_Access is access all Root_Encapsulated'Class;

   procedure Free (Data : in out Root_Encapsulated);
   --  Free the memory associated with Data.
   --  By default, it does nothing

   generic
      type Encapsulated is abstract new Root_Encapsulated with private;
   package Smart_Pointers is
      type Pointer is private;
      Null_Pointer : constant Pointer;

      type Encapsulated_Access is access all Encapsulated'Class;

      function Allocate (Data : Encapsulated'Class) return Pointer;
      function Allocate (Data : access Encapsulated'Class) return Pointer;
      pragma Inline (Allocate);
      --  Allocate a new pointer.
      --  Data is adopted by the smart pointer, and should no longer be
      --  referenced directly elsewhere. This is meant for efficiency in cases
      --  where copying the data would cost too much.
      --  Typical code looks like:
      --      Tmp := new Encapsulated;
      --      Ptr := Allocate (Tmp);
      --  (You can't do Allocate(new Encapsulated) for visibility reasons)

      function Get (P : Pointer) return Encapsulated_Access;
      pragma Inline (Get);
      --  Return the data pointed to by P

      function "=" (P1, P2 : Pointer) return Boolean;
      --  Whether the two pointers point to the same data

   private
      type Pointer is new Ada.Finalization.Controlled with record
         Data : Root_Encapsulated_Access;
      end record;

      procedure Finalize (P : in out Pointer);
      procedure Adjust   (P : in out Pointer);
      --  Take care of reference counting

      Null_Pointer : constant Pointer :=
                       (Ada.Finalization.Controlled with Data => null);
   end Smart_Pointers;

private
   type Root_Encapsulated is abstract tagged record
      Refcount : aliased Interfaces.Integer_32 := 1;
   end record;
end Sax.Pointers;
