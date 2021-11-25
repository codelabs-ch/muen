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

package body Mutools.Types
is

   -------------------------------------------------------------------------

   function Is_Valid_Event_ID
     (Group : Event_Group_Type;
      ID    : Natural)
      return Boolean
   is

      --  Reserved VMX exit IDs, see Intel SDM Vol. 3D, "Appendix C VMX Basic
      --  Exit Reasons".
      subtype Reserved_VMX_Exit_IDs_Type is Natural
        with Static_Predicate => Reserved_VMX_Exit_IDs_Type in 35 | 38 | 42;

      --  Kernel reserved IDs. Used internally/Handled by the SK.
      subtype Reserved_Kernel_IDs_Type is Natural
        with Static_Predicate => Reserved_Kernel_IDs_Type in
          1 | 7 | 41 | 52 | 55;
   begin
      return ID <= Get_Max_ID (Group => Group) and then
        (case Group is
            when Vmx_Exit => not (ID in Reserved_VMX_Exit_IDs_Type
                                  or else ID in Reserved_Kernel_IDs_Type),
            when Vmcall   => True);
   end Is_Valid_Event_ID;

end Mutools.Types;
