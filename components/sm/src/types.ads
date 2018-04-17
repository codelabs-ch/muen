--
--  Copyright (C) 2014, 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014, 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK;

package Types
is

   --  Types related to I/O instruction specific exit qualification.

   type Access_Size_Type is mod 2 ** 3
     with Size => 3;

   One_Byte  : constant Access_Size_Type := 0;
   Two_Byte  : constant Access_Size_Type := 1;
   Four_Byte : constant Access_Size_Type := 3;

   type Direction_Type is (Dir_Out, Dir_In)
     with Size => 1;
   for Direction_Type use
     (Dir_Out => 0,
      Dir_In  => 1);

   type Operand_Encoding_Type is (DX, Immediate)
     with Size => 1;
   for Operand_Encoding_Type use
     (DX        => 0,
      Immediate => 1);

   type IO_Info_Type is record
      Size         : Access_Size_Type;
      Direction    : Direction_Type;
      String_Instr : Boolean;
      REP_Prefixed : Boolean;
      Op_Encoding  : Operand_Encoding_Type;
      Reserved     : SK.Bit_Array (1 .. 9);
      Port_Number  : SK.Word16;
   end record
     with Size => 64;

   for IO_Info_Type use record
      Size         at 0 range  0 ..  2;
      Direction    at 0 range  3 ..  3;
      String_Instr at 0 range  4 ..  4;
      REP_Prefixed at 0 range  5 ..  5;
      Op_Encoding  at 0 range  6 ..  6;
      Reserved     at 0 range  7 .. 15;
      Port_Number  at 0 range 16 .. 31;
   end record;

   --  Type related to EPT violation specific exit qualification.
   type EPTV_Info_Type is record
      Read              : Boolean;
      Write             : Boolean;
      Instruction_Fetch : Boolean;
      Is_Readable       : Boolean;
      Is_Writable       : Boolean;
      Valid_Address     : Boolean;
      Is_Linear_Access  : Boolean;
      NMI_Blocking      : Boolean;
   end record;

   type Data_Register_Type is
     (RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI, R8, R9,
      R10, R11, R12, R13, R14, R15)
     with Size => 4;

   for Data_Register_Type use
     (RAX => 0,
      RCX => 1,
      RDX => 2,
      RBX => 3,
      RSP => 4,
      RBP => 5,
      RSI => 6,
      RDI => 7,
      R8  => 8,
      R9  => 9,
      R10 => 10,
      R11 => 11,
      R12 => 12,
      R13 => 13,
      R14 => 14,
      R15 => 15);

   --  Specifies the action to be taken after exit handler processing.
   type Subject_Action_Type is
     (Subject_Start,
      Subject_Continue,
      Subject_Halt,
      Subject_Reset);

   --  Return I/O instruction information from exit qualification, as specified
   --  by Intel SDM Vol. 3C, section 27.2.1, table 27-5.
   function To_IO_Info (Qualification : SK.Word64) return IO_Info_Type;

end Types;
