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

with SK.Constants;
with SK.Bitops;
with SK.CPU.VMX;
with SK.KC;
with SK.Locks;
with SK.Dump;
with SK.VMX;

package body SK.Subjects.Debug
with
   SPARK_Mode => Off
is

   VMX_EXIT_INTR_INFO_ERROR_CODE_VALID_FLAG : constant := 11;
   VMX_EXIT_INTR_INFO_VALID_FLAG            : constant := 31;

   -------------------------------------------------------------------------

   procedure Print_State (ID : Skp.Subject_Id_Type)
   is
      Exit_Interruption_Info : Word64;
   begin
      VMX.VMCS_Read (Field => Constants.VMX_EXIT_INTR_INFO,
                     Value => Exit_Interruption_Info);

      Locks.Acquire;
      KC.Put_String (Item => "Subject 0x");
      KC.Put_Byte   (Item =>  Byte (ID));
      KC.New_Line;

      KC.Put_String (Item => "Exit reason: ");
      KC.Put_Word16 (Item => Word16 (Descriptors (ID).Exit_Reason));
      KC.Put_String (Item => ", Exit qualification: ");
      KC.Put_Word64 (Item => Descriptors (ID).Exit_Qualification);
      KC.New_Line;

      if Bitops.Bit_Test
        (Value => Exit_Interruption_Info,
         Pos   => VMX_EXIT_INTR_INFO_VALID_FLAG)
      then
         KC.Put_String (Item => "Interrupt info: ");
         KC.Put_Word32 (Item => Word32'Mod (Exit_Interruption_Info));
         if Bitops.Bit_Test
           (Value => Exit_Interruption_Info,
            Pos   => VMX_EXIT_INTR_INFO_ERROR_CODE_VALID_FLAG)
         then
            declare
               Err_Code : Word64;
               Success  : Boolean;
            begin
               CPU.VMX.VMREAD
                 (Field   => Constants.VMX_EXIT_INTR_ERROR_CODE,
                  Value   => Err_Code,
                  Success => Success);
               if Success then
                  KC.Put_String (Item => ", Interrupt error code: ");
                  KC.Put_Word32 (Item => Word32 (Err_Code));
               end if;
            end;
         end if;
         KC.New_Line;
      end if;

      Dump.Print_Registers (Regs => Descriptors (ID).Regs,
                            RIP  => Descriptors (ID).RIP,
                            CS   => Descriptors (ID).CS.Selector,
                            RFL  => Descriptors (ID).RFLAGS,
                            RSP  => Descriptors (ID).RSP,
                            SS   => Descriptors (ID).SS.Selector,
                            CR0  => Descriptors (ID).CR0,
                            CR3  => Descriptors (ID).CR3,
                            CR4  => Descriptors (ID).CR4);
      Dump.Print_Segment (Name => "CS  ",
                          Seg  => Descriptors (ID).CS);
      Dump.Print_Segment (Name => "SS  ",
                          Seg  => Descriptors (ID).SS);
      Dump.Print_Segment (Name => "DS  ",
                          Seg  => Descriptors (ID).DS);
      Dump.Print_Segment (Name => "ES  ",
                          Seg  => Descriptors (ID).ES);
      Dump.Print_Segment (Name => "FS  ",
                          Seg  => Descriptors (ID).FS);
      Dump.Print_Segment (Name => "GS  ",
                          Seg  => Descriptors (ID).GS);
      Dump.Print_Segment (Name => "TR  ",
                          Seg  => Descriptors (ID).TR);
      Dump.Print_Segment (Name => "LDTR",
                          Seg  => Descriptors (ID).LDTR);
      Locks.Release;
   end Print_State;

end SK.Subjects.Debug;
