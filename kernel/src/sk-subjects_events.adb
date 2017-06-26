--
--  Copyright (C) 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.Constants;

package body SK.Subjects_Events
with
   Refined_State => (State => Global_Pending_Events)
is

   Bits_In_Word : constant := 32;

   type Event_Bit_Type is range 0 .. (Bits_In_Word - 1);

   type Bitfield32_Type is mod 2 ** Bits_In_Word;

   type Atomic32_Type is record
      Bits : Bitfield32_Type with Atomic;
   end record
   with
      Atomic,
      Size      => 32,
      Alignment => 4;

   type Pending_Events_Array is array (Skp.Global_Subject_ID_Type)
     of Atomic32_Type
   with
      Independent_Components;

   Global_Pending_Events : Pending_Events_Array
   with
      Volatile,
      Async_Writers,
      Async_Readers,
      Linker_Section => Constants.Global_Data_Section;

   -------------------------------------------------------------------------

   --  Clear event for specified subject in global events array.
   procedure Atomic_Clear
     (Subject_ID : Skp.Global_Subject_ID_Type;
      Event_ID   : Byte)
   with
      Global  => (In_Out => Global_Pending_Events),
      Depends => (Global_Pending_Events =>+ (Subject_ID, Event_ID));

   procedure Atomic_Clear
     (Subject_ID : Skp.Global_Subject_ID_Type;
      Event_ID   : Byte)
   with
      SPARK_Mode => Off
   is
   begin
      System.Machine_Code.Asm
        (Template => "lock btr %0, (%1)",
         Inputs   => (Word32'Asm_Input ("r", Word32 (Event_ID)),
                      System.Address'Asm_Input
                        ("r", Global_Pending_Events (Subject_ID)'Address)),
         Clobber  => "memory",
         Volatile => True);
   end Atomic_Clear;

   -------------------------------------------------------------------------

   --  Set event for specified subject in global events array.
   procedure Atomic_Set
     (Subject_ID : Skp.Global_Subject_ID_Type;
      Event_ID   : Byte)
   with
      Global  => (In_Out => Global_Pending_Events),
      Depends => (Global_Pending_Events =>+ (Subject_ID, Event_ID));

   procedure Atomic_Set
     (Subject_ID : Skp.Global_Subject_ID_Type;
      Event_ID   : Byte)
   with
      SPARK_Mode => Off
   is
   begin
      System.Machine_Code.Asm
        (Template => "lock bts %0, (%1)",
         Inputs   => (Word32'Asm_Input ("r", Word32 (Event_ID)),
                      System.Address'Asm_Input
                        ("r", Global_Pending_Events (Subject_ID)'Address)),
         Clobber  => "memory",
         Volatile => True);
   end Atomic_Set;

   -------------------------------------------------------------------------

   --  Find highest bit set in given bitfield. If no bit is set, return False.
   procedure Find_Highest_Bit_Set
     (Field :     Word32;
      Found : out Boolean;
      Pos   : out Event_Bit_Type)
   with
      Depends => ((Found, Pos) => Field);

   procedure Find_Highest_Bit_Set
     (Field :     Word32;
      Found : out Boolean;
      Pos   : out Event_Bit_Type)
   with
      SPARK_Mode => Off
   is
      Tmp_Pos : SK.Word32;
   begin
      Found := Field /= 0;

      if Found then
         System.Machine_Code.Asm
           (Template => "bsrl %1, %0",
            Inputs   => (Word32'Asm_Input ("g", Field)),
            Outputs  => (Word32'Asm_Output ("=r", Tmp_Pos)));

         Pos := Event_Bit_Type (Tmp_Pos); -- Position: 0 .. 31
      end if;
   end Find_Highest_Bit_Set;

   -------------------------------------------------------------------------

   procedure Initialize
   with
      Refined_Global  => (Output   => Global_Pending_Events,
                          Proof_In => CPU_Info.Is_BSP),
      Refined_Depends => (Global_Pending_Events => null)
   is
   begin
      for Subj_ID in Skp.Global_Subject_ID_Type'Range loop
         Global_Pending_Events (Subj_ID) := Atomic32_Type'(Bits => 0);
      end loop;
   end Initialize;

   -------------------------------------------------------------------------

   procedure Set_Event_Pending
     (Subject  : Skp.Global_Subject_ID_Type;
      Event_ID : Skp.Events.Event_Range)
   with
      Refined_Global  => (In_Out => Global_Pending_Events),
      Refined_Depends => (Global_Pending_Events =>+ (Event_ID, Subject))
   is
   begin
      Atomic_Set (Subject_ID => Subject,
                  Event_ID   => Byte (Event_ID));
   end Set_Event_Pending;

   -------------------------------------------------------------------------

   procedure Clear_Events (Subject : Skp.Global_Subject_ID_Type)
   with
      Refined_Global  => (In_Out => Global_Pending_Events),
      Refined_Depends => (Global_Pending_Events =>+ Subject)
   is
   begin
      Global_Pending_Events (Subject) := Atomic32_Type'(Bits => 0);
   end Clear_Events;

   -------------------------------------------------------------------------

   procedure Consume_Event
     (Subject :     Skp.Global_Subject_ID_Type;
      Found   : out Boolean;
      Event   : out Skp.Events.Event_Range)
   with
      Refined_Global  => (In_Out => Global_Pending_Events),
      Refined_Depends => ((Event, Found, Global_Pending_Events) =>
                           (Global_Pending_Events, Subject))
   is
      Bits    : Bitfield32_Type;
      Bit_Pos : Event_Bit_Type;
   begin
      Event := 0;
      Found := False;

      Bits := Global_Pending_Events (Subject).Bits;

      if Bits /= 0 then
         Find_Highest_Bit_Set
           (Field => Word32 (Bits),
            Found => Found,
            Pos   => Bit_Pos);

         if Found then
            Event := Skp.Events.Event_Range (Bit_Pos);
            Atomic_Clear (Subject_ID => Subject,
                          Event_ID   => Byte (Event));
         end if;
      end if;
   end Consume_Event;

end SK.Subjects_Events;
