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

with SK;

generic

   --  Base address of video framebuffer.
   Base_Address : System.Address;

package Terminal_Screen
is

   --  Initialize terminal screen.
   procedure Init;

   --  Update state of terminal screen.
   procedure Update (Char : Character);

private

   type State_Type is
     (State_Ground,
      State_Escape,
      State_CSI_Entry,
      State_CSI_Param);

   type CSI_Param_Idx_Range is range 0 .. 16;

   CSI_Empty_Params : constant CSI_Param_Idx_Range
     := CSI_Param_Idx_Range'First;

   subtype CSI_Param_Range is CSI_Param_Idx_Range range
     1 .. CSI_Param_Idx_Range'Last;

   type CSI_Param_Array is array (CSI_Param_Range) of SK.Byte;

   type Terminal_State_Type is record
      State         : State_Type;
      CSI_Params    : CSI_Param_Array;
      CSI_Param_Idx : CSI_Param_Idx_Range;
   end record;

   Null_State : constant Terminal_State_Type
     := (State         => State_Ground,
         CSI_Params    => (others => 0),
         CSI_Param_Idx => CSI_Param_Idx_Range'First);

   Fsm : Terminal_State_Type := Null_State;

end Terminal_Screen;
