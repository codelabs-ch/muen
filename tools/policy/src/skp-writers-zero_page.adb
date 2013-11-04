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
   Memory_Size_High : constant := Memory_Size - 16#100000#;

   --  arch/x86/include/uapi/asm/e820.h.
   E820_RAM      : constant := 1;
   E820_RESERVED : constant := 2;

   procedure C_Memset
     (S : System.Address;
      C : Interfaces.C.int;
      N : Interfaces.C.size_t);
   pragma Import (C, C_Memset, "memset");

   -------------------------------------------------------------------------

   procedure Write (Filename : String)
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
      Params.e820_map (0) := (addr   => 0,
                              size   => 16#0a0000#,
                              c_type => E820_RAM);
      Params.e820_map (1) := (addr   => 16#0a0000#,
                              size   => 16#060000#,
                              c_type => E820_RESERVED);
      Params.e820_map (2) := (addr   => 16#100000#,
                              size   => Memory_Size_High,
                              c_type => E820_RAM);
      Params.e820_map (3) := (addr   => 16#e000_0000#,
                              size   => 16#0100_0000#,
                              c_type => E820_RESERVED);

      Params.the_screen_info.orig_video_mode    := 3;
      Params.the_screen_info.orig_video_cols    := 80;
      Params.the_screen_info.orig_video_lines   := 25;
      Params.the_screen_info.orig_video_points  := 16;

      Open (Filename => Filename,
            File     => File);
      bootparam_h.boot_params'Write (Stream (File => File), Params);
      Close (File => File);
   end Write;

end Skp.Writers.Zero_Page;
