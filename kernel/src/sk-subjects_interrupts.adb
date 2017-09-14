--
--  Copyright (C) 2013-2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013-2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with System;

with Skp.Kernel;

with SK.Bitops;

pragma Elaborate_All (SK.Bitops);

package body SK.Subjects_Interrupts
with
   Refined_State => (State => Pending_Interrupts)
is

   Interrupt_Count : constant := 256;
   Bits_In_Word    : constant := 64;
   Interrupt_Words : constant := Interrupt_Count / Bits_In_Word;

   type Interrupt_Word_Type is range 0 .. (Interrupt_Words - 1);

   type Interrupts_Array is array (Interrupt_Word_Type) of Word64;

   Null_Interrupts : constant Interrupts_Array := (others => 0);

   pragma Warnings (GNAT, Off, "*padded by * bits");
   type Pending_Interrupts_Array is
     array (Skp.Global_Subject_ID_Type) of Interrupts_Array
   with
      Independent_Components,
      Component_Size => Page_Size * 8,
      Alignment      => Page_Size;
   pragma Warnings (GNAT, On, "*padded by * bits");

   Pending_Interrupts : Pending_Interrupts_Array
   with
      Volatile,
      Async_Writers,
      Async_Readers,
      Address => System'To_Address (Skp.Kernel.Subj_Interrupts_Address);

   procedure Find_Highest_Bit_Set is new Bitops.Find_Highest_Bit_Set
     (Search_Range => Bitops.Word64_Pos);

   -------------------------------------------------------------------------

   --  Return word, pos for given vector.
   procedure To_Pos
     (Vector :     SK.Byte;
      Word   : out Interrupt_Word_Type;
      Pos    : out Bitops.Word64_Pos)
   with
      Inline_Always
   is
   begin
      Word := Interrupt_Word_Type (Vector / Bits_In_Word);
      Pos  := Bitops.Word64_Pos (Vector mod Bits_In_Word);
   end To_Pos;

   -------------------------------------------------------------------------

   --  Convert given word, pos to vector;
   function To_Vector
     (Word : Interrupt_Word_Type;
      Pos  : Bitops.Word64_Pos)
      return Byte
   is (Byte (Word) * Byte (Bits_In_Word) + Byte (Pos))
   with
      Inline_Always;

   -------------------------------------------------------------------------

   --  Clear interrupt vector for specified subject in global interrupts array.
   procedure Interrupt_Clear
     (Subject : Skp.Global_Subject_ID_Type;
      Vector  : SK.Byte)
   with
      Global  => (In_Out => Pending_Interrupts),
      Depends => (Pending_Interrupts =>+ (Subject, Vector))
   is
      Word : Interrupt_Word_Type;
      Pos  : Bitops.Word64_Pos;
   begin
      To_Pos (Vector => Vector,
              Word   => Word,
              Pos    => Pos);

      declare
         Val : constant Word64 := Pending_Interrupts (Subject) (Word);
      begin
         Pending_Interrupts (Subject) (Word)
           := Bitops.Bit_Clear (Value => Val,
                                Pos   => Pos);
      end;
   end Interrupt_Clear;

   -------------------------------------------------------------------------

   procedure Init_Interrupts (Subject : Skp.Global_Subject_ID_Type)
   with
      Refined_Global  => (In_Out => Pending_Interrupts),
      Refined_Depends => (Pending_Interrupts =>+ Subject)
   is
   begin
      Pending_Interrupts (Subject) := Null_Interrupts;
   end Init_Interrupts;

   -------------------------------------------------------------------------

   procedure Insert_Interrupt
     (Subject : Skp.Global_Subject_ID_Type;
      Vector  : SK.Byte)
   with
      Refined_Global  => (In_Out => Pending_Interrupts),
      Refined_Depends => (Pending_Interrupts =>+ (Vector, Subject))
   is
      Word : Interrupt_Word_Type;
      Pos  : Bitops.Word64_Pos;
   begin
      To_Pos (Vector => Vector,
              Word   => Word,
              Pos    => Pos);

      declare
         Val : constant Word64 := Pending_Interrupts (Subject) (Word);
      begin
         Pending_Interrupts (Subject) (Word)
           := Bitops.Bit_Set (Value => Val,
                              Pos   => Pos);
      end;
   end Insert_Interrupt;

   -------------------------------------------------------------------------

   procedure Has_Pending_Interrupt
     (Subject           :     Skp.Global_Subject_ID_Type;
      Interrupt_Pending : out Boolean)
   with
      Refined_Global  => Pending_Interrupts,
      Refined_Depends => (Interrupt_Pending => (Subject, Pending_Interrupts))
   is
      Field      : Word64;
      Unused_Pos : Bitops.Word64_Pos;
   begin
      Search_Interrupt_Words :
      for Interrupt_Word in reverse Interrupt_Word_Type loop
         Field := Pending_Interrupts (Subject) (Interrupt_Word);

         pragma Warnings
           (GNATprove, Off, "unused assignment to ""Unused_Pos""",
            Reason => "Only Interrupt_Pending is needed");
         Find_Highest_Bit_Set
           (Field => Field,
            Found => Interrupt_Pending,
            Pos   => Unused_Pos);
         pragma Warnings
           (GNATprove, On, "unused assignment to ""Unused_Pos""");
         exit Search_Interrupt_Words when Interrupt_Pending;
      end loop Search_Interrupt_Words;
   end Has_Pending_Interrupt;

   -------------------------------------------------------------------------

   procedure Consume_Interrupt
     (Subject :     Skp.Global_Subject_ID_Type;
      Found   : out Boolean;
      Vector  : out SK.Byte)
   with
      Refined_Global  => (In_Out => Pending_Interrupts),
      Refined_Depends => ((Vector, Found, Pending_Interrupts) =>
                              (Pending_Interrupts, Subject))
   is
      Field       : Word64;
      Bit_In_Word : Bitops.Word64_Pos;
   begin
      Vector := 0;

      Search_Interrupt_Words :
      for Interrupt_Word in reverse Interrupt_Word_Type loop
         Field := Pending_Interrupts (Subject) (Interrupt_Word);

         Find_Highest_Bit_Set
           (Field => Field,
            Found => Found,
            Pos   => Bit_In_Word);

         if Found then
            Vector := To_Vector
              (Word => Interrupt_Word,
               Pos  => Bit_In_Word);
            Interrupt_Clear
              (Subject => Subject,
               Vector  => Vector);
            exit Search_Interrupt_Words;
         end if;
      end loop Search_Interrupt_Words;
   end Consume_Interrupt;

end SK.Subjects_Interrupts;
