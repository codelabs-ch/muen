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

with SK.Bitops;

package body SK.Subjects_Events
with
   Refined_State => (State => Global_Pending_Events)
is

   -------------------------------------------------------------------------

   --  Clear interrupt vector for specified subject in global interrupts array.
   procedure Event_Clear
     (Subject_ID : Skp.Global_Subject_ID_Type;
      Event      : SK.Byte)
   with
      Global  => (In_Out => Global_Pending_Events),
      Depends => (Global_Pending_Events =>+ (Subject_ID, Event));

   procedure Event_Clear
     (Subject_ID : Skp.Global_Subject_ID_Type;
      Event      : SK.Byte)
   with
      SPARK_Mode => Off
   is
   begin
      System.Machine_Code.Asm
        (Template => "lock btr %0, (%1)",
         Inputs   => (Word64'Asm_Input ("r", Word64 (Event)),
                      System.Address'Asm_Input
                        ("r", Global_Pending_Events (Subject_ID)'Address)),
         Clobber  => "memory",
         Volatile => True);
   end Event_Clear;

   -------------------------------------------------------------------------

   procedure Set_Event_Pending
     (Subject  : Skp.Global_Subject_ID_Type;
      Event_ID : Skp.Events.Event_Range)
   with
      Refined_Global  => (In_Out => Global_Pending_Events),
      Refined_Depends => (Global_Pending_Events =>+ (Event_ID, Subject)),
      SPARK_Mode      => Off
   is
   begin
      System.Machine_Code.Asm
        (Template => "lock bts %0, (%1)",
         Inputs   => (Word64'Asm_Input ("r", Word64 (Event_ID)),
                      System.Address'Asm_Input
                        ("r", Global_Pending_Events (Subject)'Address)),
         Clobber  => "memory",
         Volatile => True);
   end Set_Event_Pending;

   -------------------------------------------------------------------------

   procedure Initialize
   with
      Refined_Global  => (Output   => Global_Pending_Events,
                          Proof_In => CPU_Info.Is_BSP),
      Refined_Depends => (Global_Pending_Events => null)
   is
   begin
      for Subj_ID in Skp.Global_Subject_ID_Type'Range loop
         Global_Pending_Events (Subj_ID) := Null_Events;
      end loop;
   end Initialize;

   -------------------------------------------------------------------------

   procedure Clear_Events (Subject : Skp.Global_Subject_ID_Type)
   with
      Refined_Global  => (In_Out => Global_Pending_Events),
      Refined_Depends => (Global_Pending_Events =>+ Subject)
   is
   begin
      Global_Pending_Events (Subject) := Null_Events;
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
      use Skp.Events;

      procedure Find_Highest_Bit_Set is new Bitops.Find_Highest_Bit_Set
        (Search_Range => Bitops.Word64_Pos);

      Bits        : Bitfield64_Type;
      Bit_In_Word : Event_Bit_Type;
   begin
      Event := 0;

      Search_Event_Words :
      for Event_Word in reverse Event_Word_Type loop
         Bits := Global_Pending_Events (Subject) (Event_Word);

         Find_Highest_Bit_Set
           (Field => Word64 (Bits),
            Found => Found,
            Pos   => Bitops.Word64_Pos (Bit_In_Word));

         if Found then
            Event := Target_Event_Range (Event_Word)
              * Target_Event_Range (Bits_In_Word)
              + Target_Event_Range (Bit_In_Word);
            Event_Clear (Subject_ID => Subject,
                         Event      => Byte (Event));
            exit Search_Event_Words;
         end if;
      end loop Search_Event_Words;
   end Consume_Event;

end SK.Subjects_Events;
