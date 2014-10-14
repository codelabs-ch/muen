--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package body SK.Events
with
   Refined_State => (State => Global_Events)

--  External modification by concurrent kernels is not modelled.
is

   Event_Count  : constant := 256;
   Bits_In_Word : constant := 64;
   Event_Words  : constant := Event_Count / Bits_In_Word;

   type Event_Word_Type is range 0 .. (Event_Words - 1);

   type Event_Bit_Type is range 0 .. (Bits_In_Word - 1);

   type Event_Pos_Type is range
     0 .. Event_Count * (Skp.Subject_Id_Type'Last + 1) - 1;

   type Bitfield64_Type is mod 2 ** Bits_In_Word;

   type Atomic64_Type is record
      Bits : Bitfield64_Type with Atomic;
   end record
   with
       Atomic,
       Size      => 64,
       Alignment => 8;

   type Event_Array is array (Event_Word_Type) of Atomic64_Type;

   type Global_Event_Array is array (Skp.Subject_Id_Type) of Event_Array;

   Global_Events : Global_Event_Array := Global_Event_Array'
     (others => Event_Array'(others => Atomic64_Type'(Bits => 0)))
   with
      Volatile,
      Async_Writers,
      Async_Readers;

   -------------------------------------------------------------------------

   --  Clear event at given bit position in global events array.
   procedure Atomic_Event_Clear (Event_Bit_Pos : Event_Pos_Type)
   with
      Global  => (In_Out => Global_Events),
      Depends => (Global_Events =>+ Event_Bit_Pos);

   procedure Atomic_Event_Clear (Event_Bit_Pos : Event_Pos_Type)
   with
      SPARK_Mode => Off
   is
   begin
      System.Machine_Code.Asm
        (Template => "lock btr %0, (%1)",
         Inputs   =>
           (Word64'Asm_Input ("r", Word64 (Event_Bit_Pos)),
            System.Address'Asm_Input ("r", Global_Events'Address)),
         Clobber  => "memory",
         Volatile => True);
   end Atomic_Event_Clear;

   -------------------------------------------------------------------------

   --  Set event at given bit position in global events array.
   procedure Atomic_Event_Set (Event_Bit_Pos : Event_Pos_Type)
   with
      Global  => (In_Out => Global_Events),
      Depends => (Global_Events =>+ Event_Bit_Pos);

   procedure Atomic_Event_Set (Event_Bit_Pos : Event_Pos_Type)
   with
      SPARK_Mode => Off
   is
   begin
      System.Machine_Code.Asm
        (Template => "lock bts %0, (%1)",
         Inputs   =>
           (Word64'Asm_Input ("r", Word64 (Event_Bit_Pos)),
            System.Address'Asm_Input ("r", Global_Events'Address)),
         Clobber  => "memory",
         Volatile => True);
   end Atomic_Event_Set;

   -------------------------------------------------------------------------

   --  Find highest bit set in given bitfield. If no bit is set, return False.
   procedure Find_Highest_Bit_Set
     (Field :     SK.Word64;
      Found : out Boolean;
      Pos   : out Event_Bit_Type)
   with
      Depends => ((Found, Pos) => Field);

   procedure Find_Highest_Bit_Set
     (Field :     SK.Word64;
      Found : out Boolean;
      Pos   : out Event_Bit_Type)
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

         Pos := Event_Bit_Type (Tmp_Pos); -- Position: 0 .. 63
      end if;
   end Find_Highest_Bit_Set;

   -------------------------------------------------------------------------

   procedure Insert_Event
     (Subject : Skp.Subject_Id_Type;
      Event   : SK.Byte)
   with
      Refined_Global  => (In_Out => Global_Events),
      Refined_Depends => (Global_Events =>+ (Event, Subject))
   is
      Pos : Event_Pos_Type;
   begin
      Pos := Event_Count * Event_Pos_Type (Subject) + Event_Pos_Type (Event);
      Atomic_Event_Set (Event_Bit_Pos => Pos);
   end Insert_Event;

   -------------------------------------------------------------------------

   procedure Has_Pending_Events
     (Subject       :     Skp.Subject_Id_Type;
      Event_Pending : out Boolean)
   with
      SPARK_Mode      => $Complete_Proofs,
      --  Workaround for [N306-030] "Accessing parts of volatile objects"
      Refined_Global  => Global_Events,
      Refined_Depends => (Event_Pending => (Subject, Global_Events))
   is
      Bits       : SK.Word64;
      Unused_Pos : Event_Bit_Type;
   begin
      Search_Event_Words :
      for Event_Word in reverse Event_Word_Type loop
         Bits := SK.Word64 (Global_Events (Subject) (Event_Word).Bits);

         pragma $Prove_Warnings
           (Off, "unused assignment to ""Unused_Pos""",
            Reason => "Only Event_Pending is needed");
         Find_Highest_Bit_Set
           (Field => Bits,
            Found => Event_Pending,
            Pos   => Unused_Pos);
         pragma $Prove_Warnings (On, "unused assignment to ""Unused_Pos""");
         exit Search_Event_Words when Event_Pending;
      end loop Search_Event_Words;

   end Has_Pending_Events;

   -------------------------------------------------------------------------

   procedure Consume_Event
     (Subject :     Skp.Subject_Id_Type;
      Found   : out Boolean;
      Event   : out SK.Byte)
   with
      Refined_Global  => (In_Out => Global_Events),
      Refined_Depends => ((Event, Found, Global_Events) =>
                              (Global_Events, Subject))
   is
      Bits        : Bitfield64_Type;
      Bit_In_Word : Event_Bit_Type;
   begin
      Event := 0;

      Search_Event_Words :
      for Event_Word in reverse Event_Word_Type loop
         Bits := Global_Events (Subject) (Event_Word).Bits;

         Find_Highest_Bit_Set
           (Field => SK.Word64 (Bits),
            Found => Found,
            Pos   => Bit_In_Word);

         if Found then
            Event := SK.Byte (Event_Word) * SK.Byte (Bits_In_Word)
              + SK.Byte (Bit_In_Word);
            Atomic_Event_Clear
              (Event_Bit_Pos => Event_Count * Event_Pos_Type
                 (Subject) + Event_Pos_Type (Event));
            exit Search_Event_Words;
         end if;
      end loop Search_Event_Words;
   end Consume_Event;

end SK.Events;
