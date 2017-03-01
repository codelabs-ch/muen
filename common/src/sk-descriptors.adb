--
--  Copyright (C) 2013, 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package body SK.Descriptors
is

   -------------------------------------------------------------------------

   procedure Setup_IDT
     (ISRs :        ISR_Array;
      IDT  : in out IDT_Type;
      IST  :        Natural)
   is
   begin
      for I in Vector_Range range ISRs'Range loop
         IDT (I) := Gate_Type'
           (Offset_15_00     => SK.Word16'Mod
              (ISRs (I) and 16#0000_0000_0000_ffff#),
            Segment_Selector => 16#0008#,
            Flags            => 16#8e00# + SK.Word16 (IST),
            Offset_31_16     => SK.Word16'Mod
              ((ISRs (I) and 16#0000_0000_ffff_0000#) / 2 ** 16),
            Offset_63_32     => SK.Word32'Mod
              ((ISRs (I) and 16#ffff_ffff_0000_0000#) / 2 ** 32),
            Reserved         => 0);
      end loop;
   end Setup_IDT;

end SK.Descriptors;
