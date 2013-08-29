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
--  Warning: External modification by concurrent kernels not yet visible to
--  the SPARK examiner.
--# own State is Global_Events;
is

   Event_Count  : constant := 256;
   Bits_In_Word : constant := 64;
   Event_Words  : constant := Event_Count / Bits_In_Word;

   type Event_Word_Type is range 0 .. (Event_Words - 1);

   type Event_Bit_Type is range 0 .. (Bits_In_Word - 1);

   type Bitfield64_Type is mod 2 ** Bits_In_Word;
   pragma Atomic (Bitfield64_Type);

   type Atomic64_Type is record
      Bits : Bitfield64_Type;
   end record;

   pragma Atomic (Atomic64_Type);
   for Atomic64_Type'Size use 64;
   for Atomic64_Type'Alignment use 8;

   Null_Atomic64 : constant Atomic64_Type := Atomic64_Type'(Bits => 0);

   type Event_Array is array (Event_Word_Type) of Atomic64_Type;

   type Global_Event_Array is array (Skp.Subject_Id_Type) of Event_Array;

   Global_Events : Global_Event_Array := Global_Event_Array'
     (others => Event_Array'(others => Null_Atomic64));

   -------------------------------------------------------------------------

   procedure Atomic_Bit_Set
     (Field : in out Atomic64_Type;
      Pos   :        Event_Bit_Type)
   --# derives
   --#    Field from *, Pos;
   is
      --# hide Atomic_Bit_Set;
   begin
      System.Machine_Code.Asm
        (Template => "lock bts %0, (%1)",
         Inputs   =>
           (Word64'Asm_Input ("r", Word64 (Pos)),
            System.Address'Asm_Input ("r", Field.Bits'Address)),
         Clobber  => "memory",
         Volatile => True);
   end Atomic_Bit_Set;

   -------------------------------------------------------------------------

   procedure Atomic_Bit_Clear
     (Field : in out Atomic64_Type;
      Pos   :        Event_Bit_Type)
   --# derives
   --#    Field from *, Pos;
   is
      --# hide Atomic_Bit_Clear;
   begin
      System.Machine_Code.Asm
        (Template => "lock btr %0, (%1)",
         Inputs   =>
           (Word64'Asm_Input ("r", Word64 (Pos)),
            System.Address'Asm_Input ("r", Field.Bits'Address)),
         Clobber  => "memory",
         Volatile => True);
   end Atomic_Bit_Clear;

   -------------------------------------------------------------------------

   --  Find highest bit set in given bitfield. If no bit is set, return False.
   procedure Find_Highest_Bit_Set
     (Field :     SK.Word64;
      Found : out Boolean;
      Pos   : out Event_Bit_Type)
   --# derives Found, Pos from Field;
   is
      --# hide Find_Highest_Bit_Set;

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
   --# global
   --#    in out Global_Events;
   --# derives
   --#    Global_Events from
   --#       *,
   --#       Subject,
   --#       Event;
   is
      Event_Word : Event_Word_Type;
      Event_Bit  : Event_Bit_Type;
   begin
      Event_Word := Event_Word_Type (Event / Bits_In_Word);
      Event_Bit  := Event_Bit_Type (Event mod Bits_In_Word);
      Atomic_Bit_Set (Field => Global_Events (Subject) (Event_Word),
                      Pos   => Event_Bit);
   end Insert_Event;

   -------------------------------------------------------------------------

   function Has_Pending_Events (Subject : Skp.Subject_Id_Type) return Boolean
   --# global
   --#    Global_Events;
   is
      Found  : Boolean;
      Bits   : SK.Word64;
      Bitpos : Event_Bit_Type;
   begin
      Search_Event_Words :
      for Event_Word in reverse Event_Word_Type loop
         Bits := SK.Word64 (Global_Events (Subject) (Event_Word).Bits);

         --# accept Flow, 10, Bitpos, "Result unused";

         Find_Highest_Bit_Set
           (Field => Bits,
            Found => Found,
            Pos   => Bitpos);
         exit Search_Event_Words when Found;
      end loop Search_Event_Words;

      --# accept Flow, 33, Bitpos, "Result unused";

      return Found;
   end Has_Pending_Events;

   -------------------------------------------------------------------------

   procedure Consume_Event
     (Subject :     Skp.Subject_Id_Type;
      Found   : out Boolean;
      Event   : out SK.Byte)
   --# global
   --#    in out Global_Events;
   --# derives
   --#    Found, Event, Global_Events from Global_Events, Subject;
   is
      Bits        : SK.Word64;
      Bit_In_Word : Event_Bit_Type;
   begin
      Event := 0;

      Search_Event_Words :
      for Event_Word in reverse Event_Word_Type loop
         Bits := SK.Word64 (Global_Events (Subject) (Event_Word).Bits);

         Find_Highest_Bit_Set
           (Field => Bits,
            Found => Found,
            Pos   => Bit_In_Word);

         if Found then
            Atomic_Bit_Clear (Field => Global_Events (Subject) (Event_Word),
                              Pos   => Bit_In_Word);
            Event := SK.Byte (Event_Word) * SK.Byte (Bits_In_Word)
              + SK.Byte (Bit_In_Word);
            exit Search_Event_Words;
         end if;
      end loop Search_Event_Words;
   end Consume_Event;

end SK.Events;
