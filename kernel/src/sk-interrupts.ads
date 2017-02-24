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

with X86_64;

package SK.Interrupts
is

   --  Mask all interrupts in the legacy PIC.
   procedure Disable_Legacy_PIC
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ null);

   --  Disable legacy PIT.
   procedure Disable_Legacy_PIT
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ null);

   pragma Warnings (GNATprove, Off, "unused variable ""Unused_Context""",
      Reason => "Unused Context is only used for debugging");

   --  Halt on (unexpected) exception.
   procedure Dispatch_Exception (Unused_Context : SK.Isr_Context_Type)
   with
      Global     => (In_Out => X86_64.State),
      Depends    => (X86_64.State =>+ null,
                     null         =>  Unused_Context),
      No_Return,
      Export,
      Convention => C,
      Link_Name  => "dispatch_interrupt";
   pragma Warnings (GNATprove, On, "unused variable ""Unused_Context""");

end SK.Interrupts;
