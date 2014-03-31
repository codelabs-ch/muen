--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Mulog;

with Expand.XML_Utils;

package body Expanders.Kernel
is

   -------------------------------------------------------------------------

   procedure Add_Binary_Memory (Data : in out Muxml.XML_Data_Type)
   is
   begin
      Mulog.Log (Msg => "Adding kernel binary memory regions");

      Expand.XML_Utils.Add_Memory_Region
        (Policy      => Data,
         Name        => "kernel_text",
         Address     => 16#0010_0000#,
         Size        => 16#0001_0000#,
         Caching     => "WB",
         File_Name   => "kernel",
         File_Format => "bin_raw",
         File_Offset => 0);
      Expand.XML_Utils.Add_Memory_Region
        (Policy      => Data,
         Name        => "kernel_data",
         Address     => 16#0011_0000#,
         Size        => 16#1000#,
         Caching     => "WB",
         File_Name   => "kernel",
         File_Format => "bin_raw",
         File_Offset => 16#0001_0000#);
      Expand.XML_Utils.Add_Memory_Region
        (Policy      => Data,
         Name        => "kernel_bss",
         Address     => 16#0011_1000#,
         Size        => 16#1000#,
         Caching     => "WB");
      Expand.XML_Utils.Add_Memory_Region
        (Policy      => Data,
         Name        => "kernel_ro",
         Address     => 16#0011_f000#,
         Size        => 16#4000#,
         Caching     => "WB",
         File_Name   => "kernel",
         File_Format => "bin_raw",
         File_Offset => 16#0001_f000#);
   end Add_Binary_Memory;

end Expanders.Kernel;
