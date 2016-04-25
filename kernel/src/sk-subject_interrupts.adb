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

with System.Machine_Code;

package body SK.Subject_Interrupts
with
   Refined_State => (State => Global_Interrupts)

--  External modification by concurrent kernels is not modelled.
is

   Interrupt_Count : constant := 256;
   Bits_In_Word    : constant := 64;
   Interrupt_Words : constant := Interrupt_Count / Bits_In_Word;

   type Interrupt_Word_Type is range 0 .. (Interrupt_Words - 1);

   type Interrupt_Bit_Type is range 0 .. (Bits_In_Word - 1);

   type Interrupt_Pos_Type is range
     0 .. Interrupt_Count * (Skp.Subject_Id_Type'Last + 1) - 1;

   type Bitfield64_Type is mod 2 ** Bits_In_Word;

   type Atomic64_Type is record
      Bits : Bitfield64_Type with Atomic;
   end record
   with
       Atomic,
       Size      => 64,
       Alignment => 8;

   type Interrupts_Array is array (Interrupt_Word_Type) of Atomic64_Type;

   type Global_Interrupts_Array is
     array (Skp.Subject_Id_Type) of Interrupts_Array;

   Global_Interrupts : Global_Interrupts_Array := Global_Interrupts_Array'
     (others => Interrupts_Array'(others => Atomic64_Type'(Bits => 0)))
   with
      Volatile,
      Async_Writers,
      Async_Readers;

   -------------------------------------------------------------------------

   --  Clear interrupt at given bit position in global interrupts array.
   procedure Atomic_Interrupt_Clear (Interrupt_Bit_Pos : Interrupt_Pos_Type)
   with
      Global  => (In_Out => Global_Interrupts),
      Depends => (Global_Interrupts =>+ Interrupt_Bit_Pos);

   procedure Atomic_Interrupt_Clear (Interrupt_Bit_Pos : Interrupt_Pos_Type)
   with
      SPARK_Mode => Off
   is
   begin
      System.Machine_Code.Asm
        (Template => "lock btr %0, (%1)",
         Inputs   =>
           (Word64'Asm_Input ("r", Word64 (Interrupt_Bit_Pos)),
            System.Address'Asm_Input ("r", Global_Interrupts'Address)),
         Clobber  => "memory",
         Volatile => True);
   end Atomic_Interrupt_Clear;

   -------------------------------------------------------------------------

   --  Set interrupt at given bit position in global interrupts array.
   procedure Atomic_Event_Set (Event_Bit_Pos : Interrupt_Pos_Type)
   with
      Global  => (In_Out => Global_Interrupts),
      Depends => (Global_Interrupts =>+ Event_Bit_Pos);

   procedure Atomic_Event_Set (Event_Bit_Pos : Interrupt_Pos_Type)
   with
      SPARK_Mode => Off
   is
   begin
      System.Machine_Code.Asm
        (Template => "lock bts %0, (%1)",
         Inputs   =>
           (Word64'Asm_Input ("r", Word64 (Event_Bit_Pos)),
            System.Address'Asm_Input ("r", Global_Interrupts'Address)),
         Clobber  => "memory",
         Volatile => True);
   end Atomic_Event_Set;

   -------------------------------------------------------------------------

   --  Find highest bit set in given bitfield. If no bit is set, return False.
   procedure Find_Highest_Bit_Set
     (Field :     SK.Word64;
      Found : out Boolean;
      Pos   : out Interrupt_Bit_Type)
   with
      Depends => ((Found, Pos) => Field);

   procedure Find_Highest_Bit_Set
     (Field :     SK.Word64;
      Found : out Boolean;
      Pos   : out Interrupt_Bit_Type)
   with
      SPARK_Mode => Off
   is
      Tmp_Pos : SK.Word64;
   begin
      Found := Field /= 0;

      if Found then
         System.Machine_Code.Asm
           (Template => "bsrq %1, %0",
            Inputs   => (SK.Word64'Asm_Input ("g", Field)),
            Outputs  => (SK.Word64'Asm_Output ("=r", Tmp_Pos)));

         Pos := Interrupt_Bit_Type (Tmp_Pos); -- Position: 0 .. 63
      end if;
   end Find_Highest_Bit_Set;

   -------------------------------------------------------------------------

   procedure Insert_Event
     (Subject : Skp.Subject_Id_Type;
      Event   : SK.Byte)
   with
      Refined_Global  => (In_Out => Global_Interrupts),
      Refined_Depends => (Global_Interrupts =>+ (Event, Subject))
   is
      Pos : Interrupt_Pos_Type;
   begin
      Pos := Interrupt_Count * Interrupt_Pos_Type (Subject)
        + Interrupt_Pos_Type (Event);
      pragma Assert
        (Natural (Pos) >= Interrupt_Count * Subject and then
         Natural (Pos) < Interrupt_Count * Subject + Interrupt_Count,
         "Events of unrelated subject changed");
      Atomic_Event_Set (Event_Bit_Pos => Pos);
   end Insert_Event;

   -------------------------------------------------------------------------

   procedure Has_Pending_Events
     (Subject       :     Skp.Subject_Id_Type;
      Event_Pending : out Boolean)
   with
      Refined_Global  => Global_Interrupts,
      Refined_Depends => (Event_Pending => (Subject, Global_Interrupts))
   is
      Bits       : Bitfield64_Type;
      Unused_Pos : Interrupt_Bit_Type;
   begin
      Search_Interrupt_Words :
      for Interrupt_Word in reverse Interrupt_Word_Type loop
         Bits := Global_Interrupts (Subject) (Interrupt_Word).Bits;

         pragma Warnings
           (GNATprove, Off, "unused assignment to ""Unused_Pos""",
            Reason => "Only Event_Pending is needed");
         Find_Highest_Bit_Set
           (Field => SK.Word64 (Bits),
            Found => Event_Pending,
            Pos   => Unused_Pos);
         pragma Warnings
           (GNATprove, On, "unused assignment to ""Unused_Pos""");
         exit Search_Interrupt_Words when Event_Pending;
      end loop Search_Interrupt_Words;

   end Has_Pending_Events;

   -------------------------------------------------------------------------

   procedure Consume_Event
     (Subject :     Skp.Subject_Id_Type;
      Found   : out Boolean;
      Event   : out SK.Byte)
   with
      Refined_Global  => (In_Out => Global_Interrupts),
      Refined_Depends => ((Event, Found, Global_Interrupts) =>
                              (Global_Interrupts, Subject))
   is
      Bits        : Bitfield64_Type;
      Bit_In_Word : Interrupt_Bit_Type;
      Pos         : Interrupt_Pos_Type;
   begin
      Event := 0;

      Search_Interrupt_Words :
      for Interrupt_Word in reverse Interrupt_Word_Type loop
         Bits := Global_Interrupts (Subject) (Interrupt_Word).Bits;

         Find_Highest_Bit_Set
           (Field => SK.Word64 (Bits),
            Found => Found,
            Pos   => Bit_In_Word);

         if Found then
            Event := SK.Byte (Interrupt_Word) * SK.Byte (Bits_In_Word)
              + SK.Byte (Bit_In_Word);
            Pos := Interrupt_Count * Interrupt_Pos_Type
              (Subject) + Interrupt_Pos_Type (Event);
            pragma Assert
              (Natural (Pos) >= Interrupt_Count * Subject and then
               Natural (Pos) <  Interrupt_Count * Subject + Interrupt_Count,
               "Events of unrelated subject consumed");
            Atomic_Interrupt_Clear (Interrupt_Bit_Pos => Pos);
            exit Search_Interrupt_Words;
         end if;
      end loop Search_Interrupt_Words;
   end Consume_Event;

end SK.Subject_Interrupts;
