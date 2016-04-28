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

with Skp.Kernel;

package body SK.Subjects_Interrupts
with
   Refined_State => (State => Pending_Interrupts)

--  External modification by concurrent kernels is not modelled.
is

   Interrupt_Count : constant := 256;
   Bits_In_Word    : constant := 64;
   Interrupt_Words : constant := Interrupt_Count / Bits_In_Word;

   type Interrupt_Word_Type is range 0 .. (Interrupt_Words - 1);

   type Interrupt_Bit_Type is range 0 .. (Bits_In_Word - 1);

   type Bitfield64_Type is mod 2 ** Bits_In_Word;

   type Atomic64_Type is record
      Bits : Bitfield64_Type with Atomic;
   end record
   with
       Atomic,
       Size      => 64,
       Alignment => 8;

   type Interrupts_Array is array (Interrupt_Word_Type) of Atomic64_Type;

   pragma Warnings (GNAT, Off, "*padded by * bits");
   type Pending_Interrupts_Array is
     array (Skp.Subject_Id_Type) of Interrupts_Array
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

   -------------------------------------------------------------------------

   --  Clear interrupt vector for specified subject in global interrupts array.
   procedure Atomic_Interrupt_Clear
     (Subject_ID : Skp.Subject_Id_Type;
      Vector     : SK.Byte)
   with
      Global  => (In_Out => Pending_Interrupts),
      Depends => (Pending_Interrupts =>+ (Subject_ID, Vector));

   procedure Atomic_Interrupt_Clear
     (Subject_ID : Skp.Subject_Id_Type;
      Vector     : SK.Byte)
   with
      SPARK_Mode => Off
   is
   begin
      System.Machine_Code.Asm
        (Template => "lock btr %0, (%1)",
         Inputs   => (Word64'Asm_Input ("r", Word64 (Vector)),
                      System.Address'Asm_Input
                        ("r", Pending_Interrupts (Subject_ID)'Address)),
         Clobber  => "memory",
         Volatile => True);
   end Atomic_Interrupt_Clear;

   -------------------------------------------------------------------------

   --  Set interrupt vector for specified subject in global interrupts array.
   procedure Atomic_Interrupt_Set
     (Subject_ID : Skp.Subject_Id_Type;
      Vector     : SK.Byte)
   with
      Global  => (In_Out => Pending_Interrupts),
      Depends => (Pending_Interrupts =>+ (Subject_ID, Vector));

   procedure Atomic_Interrupt_Set
     (Subject_ID : Skp.Subject_Id_Type;
      Vector     : SK.Byte)
   with
      SPARK_Mode => Off
   is
   begin
      System.Machine_Code.Asm
        (Template => "lock bts %0, (%1)",
         Inputs   => (Word64'Asm_Input ("r", Word64 (Vector)),
                      System.Address'Asm_Input
                        ("r", Pending_Interrupts (Subject_ID)'Address)),
         Clobber  => "memory",
         Volatile => True);
   end Atomic_Interrupt_Set;

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

   procedure Insert_Interrupt
     (Subject : Skp.Subject_Id_Type;
      Vector  : SK.Byte)
   with
      Refined_Global  => (In_Out => Pending_Interrupts),
      Refined_Depends => (Pending_Interrupts =>+ (Vector, Subject))
   is
   begin
      Atomic_Interrupt_Set (Subject_ID => Subject,
                            Vector     => Vector);
   end Insert_Interrupt;

   -------------------------------------------------------------------------

   procedure Has_Pending_Interrupt
     (Subject           :     Skp.Subject_Id_Type;
      Interrupt_Pending : out Boolean)
   with
      Refined_Global  => Pending_Interrupts,
      Refined_Depends => (Interrupt_Pending => (Subject, Pending_Interrupts))
   is
      Bits       : Bitfield64_Type;
      Unused_Pos : Interrupt_Bit_Type;
   begin
      Search_Interrupt_Words :
      for Interrupt_Word in reverse Interrupt_Word_Type loop
         Bits := Pending_Interrupts (Subject) (Interrupt_Word).Bits;

         pragma Warnings
           (GNATprove, Off, "unused assignment to ""Unused_Pos""",
            Reason => "Only Interrupt_Pending is needed");
         Find_Highest_Bit_Set
           (Field => SK.Word64 (Bits),
            Found => Interrupt_Pending,
            Pos   => Unused_Pos);
         pragma Warnings
           (GNATprove, On, "unused assignment to ""Unused_Pos""");
         exit Search_Interrupt_Words when Interrupt_Pending;
      end loop Search_Interrupt_Words;
   end Has_Pending_Interrupt;

   -------------------------------------------------------------------------

   procedure Consume_Interrupt
     (Subject :     Skp.Subject_Id_Type;
      Found   : out Boolean;
      Vector  : out SK.Byte)
   with
      Refined_Global  => (In_Out => Pending_Interrupts),
      Refined_Depends => ((Vector, Found, Pending_Interrupts) =>
                              (Pending_Interrupts, Subject))
   is
      Bits        : Bitfield64_Type;
      Bit_In_Word : Interrupt_Bit_Type;
   begin
      Vector := 0;

      Search_Interrupt_Words :
      for Interrupt_Word in reverse Interrupt_Word_Type loop
         Bits := Pending_Interrupts (Subject) (Interrupt_Word).Bits;

         Find_Highest_Bit_Set
           (Field => SK.Word64 (Bits),
            Found => Found,
            Pos   => Bit_In_Word);

         if Found then
            Vector := SK.Byte (Interrupt_Word) * SK.Byte (Bits_In_Word)
              + SK.Byte (Bit_In_Word);
            Atomic_Interrupt_Clear (Subject_ID => Subject,
                                    Vector     => Vector);
            exit Search_Interrupt_Words;
         end if;
      end loop Search_Interrupt_Words;
   end Consume_Interrupt;

begin
   for Subj in Skp.Subject_Id_Type'Range loop
      for Word in Interrupt_Word_Type'Range loop
         Pending_Interrupts (Subj)(Word) := Atomic64_Type'(Bits => 0);
      end loop;
   end loop;
end SK.Subjects_Interrupts;
