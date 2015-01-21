--
--  Copyright (C) 2013-2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013-2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Subject_Info;

package Exit_Handlers.CPUID
is

   --  Minimal CPUID emulation.
   --
   --  Code inspired by the emulation done by the lguest hypervisor in the
   --  Linux kernel (see arch/x86/lguest/boot.c, lguest_cpuid function).
   --  For reference values see e.g.
   --  http://www.cpu-world.com/cgi-bin/CPUID.pl?CPUID=26937&RAW_DATA=1
   procedure Process (Halt : out Boolean)
   with
      Global  => (In_Out => Subject_Info.State),
      Depends => (Subject_Info.State =>+ null, Halt => null),
      Post    => Halt = False;

end Exit_Handlers.CPUID;
