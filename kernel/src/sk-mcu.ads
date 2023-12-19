--
--  Copyright (C) 2023  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2023  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with X86_64;

--D @Interface
--D This package provides Intel microcode update (MCU) facilities.
--D This package does nothing if the policy does not specify an Intel microcode
--D update blob.
package SK.MCU
with
   Abstract_State => State,
   Initializes    => State
is

   --  Perform MCU.
   procedure Process
   with
      Global => (Input  => State,
                 In_Out => X86_64.State);

   Header_Size : constant := 48;

   type Padding_Type is array (1 .. 3) of Word32;

   --  Microcode update header.
   --  See Intel SDM Vol. 3A, "9.11.1 Microcode Update"
   type Header_Type is record
      Header_Version      : Word32;
      Update_Revision     : Word32;
      Date                : Word32;
      Processor_Signature : Word32;
      Checksum            : Word32;
      Loader_Revision     : Word32;
      Processor_Flags     : Word32;
      Data_Size           : Word32;
      Total_Size          : Word32;
      Reserved            : Padding_Type;
   end record
   with
      Pack,
      Size        => Header_Size * 8,
      Object_Size => Header_Size * 8;

end SK.MCU;
