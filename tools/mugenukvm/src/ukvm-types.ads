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

package Ukvm.Types
is

   --  struct hvt_boot_info
   --  https://github.com/Solo5/solo5/blob/master/include/solo5/hvt_abi.h
   type UKVM_Boot_Info_Type is record
      Mem_Size   : Interfaces.Unsigned_64;
      Kernel_End : Interfaces.Unsigned_64;
      Tsc_Freq   : Interfaces.Unsigned_64;
      Cmdline    : Interfaces.Unsigned_64;
      Mft        : Interfaces.Unsigned_64;
   end record;

end Ukvm.Types;
