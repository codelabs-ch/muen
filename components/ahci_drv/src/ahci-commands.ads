--
--  Copyright (C) 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Interfaces;
with System;

use type Interfaces.Unsigned_32;

package Ahci.Commands
is
   procedure Cmd_Slot_Prepare
      (Port_ID :        Port_Range;
       Len     : in out Interfaces.Unsigned_32;
       Address :        Interfaces.Unsigned_64;
       RW      :        RW_Type);

   --  Serial ATA AHCI 1.3.1 Specification, section 4.2.2.

   type Command_Header_Type is record
      CFL        : Unsigned_5;
      A          : Boolean;
      W          : Boolean;
      P          : Boolean;
      R          : Boolean;
      B          : Boolean;
      C          : Boolean;
      Reserved_1 : Boolean;
      PMP        : Unsigned_4;
      PRDTL      : Interfaces.Unsigned_16;
      PRDBC      : Interfaces.Unsigned_32;
      Reserved_2 : Bit_Array (0 .. 6);
      CTBA       : Unsigned_25;
      CTBAU      : Interfaces.Unsigned_32;
      Reserved_3 : Bit_Array (1 .. 128);
   end record
   with
      Size => 32 * 8;

   for Command_Header_Type use record
      CFL        at  0 range  0 ..   4;
      A          at  0 range  5 ..   5;
      W          at  0 range  6 ..   6;
      P          at  0 range  7 ..   7;
      R          at  0 range  8 ..   8;
      B          at  0 range  9 ..   9;
      C          at  0 range 10 ..  10;
      Reserved_1 at  0 range 11 ..  11;
      PMP        at  0 range 12 ..  15;
      PRDTL      at  0 range 16 ..  31;
      PRDBC      at  4 range  0 ..  31;
      Reserved_2 at  8 range  0 ..   6;
      CTBA       at  8 range  7 ..  31;
      CTBAU      at 12 range  0 ..  31;
      Reserved_3 at 16 range  0 .. 127;
   end record;

   type Command_List_Type is array (Natural range 0 .. 31)
     of Command_Header_Type
   with
      Pack;

   --  Serial ATA AHCI 1.3.1 Specification, section 4.2.3.2.
   type ATAPI_Command_Type is new Byte_Array (1 .. 16)
   with
      Size => 16 * 8;

   --  Serial ATA AHCI 1.3.1 Specification, section 4.2.3.3.
   type Physical_Region_Descriptor_Table_Entry_Type is record
      Reserved_1 : Boolean;
      DBA        : Unsigned_31;
      DBAU       : Interfaces.Unsigned_32;
      Reserved_2 : Interfaces.Unsigned_32;
      DBC        : Unsigned_22;
      Reserved_3 : Bit_Array (22 .. 30);
      I          : Boolean;
   end record
   with
      Size => 16 * 8;

   for Physical_Region_Descriptor_Table_Entry_Type use record
      Reserved_1 at  0 range  0 ..  0;
      DBA        at  0 range  1 .. 31;
      DBAU       at  4 range  0 .. 31;
      Reserved_2 at  8 range  0 .. 31;
      DBC        at 12 range  0 .. 21;
      Reserved_3 at 12 range 22 .. 30;
      I          at 12 range 31 .. 31;
   end record;

   type Prdt_Arr_Type is
      array (Natural range 0 .. 7)
      of Physical_Region_Descriptor_Table_Entry_Type;

   type Command_Table_Type is record
      Cfis : Byte_Array (0 .. 63);
      Acmd : Byte_Array (0 .. 15);
      Res1 : Byte_Array (0 .. 47);
      Prdt : Prdt_Arr_Type;
   end record
   with
      Size => 16#100# * 8;

   for Command_Table_Type use record
      Cfis at 16#00# range 0 .. 511;
      Acmd at 16#40# range 0 .. 127;
      Res1 at 16#50# range 0 .. 383;
      Prdt at 16#80# range 0 .. 1023;
   end record;

   type Command_Lists_Array_Type is array (Port_Range)
      of Ahci.Commands.Command_List_Type;

   pragma Warnings
     (GNATprove, Off,
      "writing * is assumed to have no effects on other non-volatile objects",
      Reason => "All objects with address clause are mapped to external "
      & "interfaces. Non-overlap is checked during system build.");
   Command_Lists : Command_Lists_Array_Type
   with
      Volatile,
      Async_Readers,
      Async_Writers,
      Address => System'To_Address (Ahci.Command_Lists_Address);

   type Command_Table_Array_Type is array (Port_Range)
      of Ahci.Commands.Command_Table_Type;

   Command_Table : Command_Table_Array_Type
   with
      Volatile,
      Async_Readers,
      Async_Writers,
      Address => System'To_Address (Ahci.Command_Table_Address);
   pragma Warnings
     (GNATprove, On,
      "writing * is assumed to have no effects on other non-volatile objects");

end Ahci.Commands;
