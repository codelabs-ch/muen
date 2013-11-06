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

with System;

with Interfaces.C;

with bootparam_h;

package body Skp.Writers.Zero_Page
is

   --  Memory size in bytes.
   Memory_Size : constant := 256 * 1024 * 1024;
   Memory_Size_High : constant := Memory_Size - 16#400000#;

   --  arch/x86/include/uapi/asm/e820.h.
   E820_RAM      : constant := 1;
   E820_RESERVED : constant := 2;

   procedure C_Memset
     (S : System.Address;
      C : Interfaces.C.int;
      N : Interfaces.C.size_t);
   pragma Import (C, C_Memset, "memset");

   -------------------------------------------------------------------------

   procedure Write (CMDL_Address : SK.Word32; Filename : String)
   is
      use Ada.Streams.Stream_IO;
      use type Interfaces.C.size_t;

      File   : Ada.Streams.Stream_IO.File_Type;
      Params : bootparam_h.boot_params;
   begin
      C_Memset (S => Params'Address,
                C => 0,
                N => bootparam_h.boot_params'Object_Size / 8);

      Params.e820_entries := 4;
      --  Zero page, Time page, HVC channel
      Params.e820_map (0) := (addr   => 16#000000#,
                              size   => 16#003000#,
                              c_type => E820_RESERVED);
      --  Usable lower memory
      Params.e820_map (1) := (addr   => 16#003000#,
                              size   => 16#09e000#,
                              c_type => E820_RAM);
      --  VGA memory, OPROMs, BIOS extension (ACPI tables), System BIOS
      Params.e820_map (2) := (addr   => 16#0b8000#,
                              size   => 16#048000#,
                              c_type => E820_RESERVED);
      --  High memory
      Params.e820_map (3) := (addr   => 16#400000#,
                              size   => Memory_Size_High,
                              c_type => E820_RAM);

      Params.hdr.cmd_line_ptr     := Interfaces.C.unsigned (CMDL_Address);
      Params.hdr.cmdline_size     := 16#0000_0fff#;
      Params.hdr.kernel_alignment := 16#0100_0000#;

      Open (Filename => Filename,
            File     => File);
      bootparam_h.boot_params'Write (Stream (File => File), Params);
      Close (File => File);
   end Write;

end Skp.Writers.Zero_Page;
