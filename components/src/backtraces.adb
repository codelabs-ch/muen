--
--  Copyright (C) 2022  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2022  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package body Backtraces
is

   type Stack_Frame_Type is record
      Saved_RBP      : Interfaces.Unsigned_64;
      Return_Address : Interfaces.Unsigned_64;
   end record
   with Pack;

   --  Return stack frame located at given address.
   function Read_Stack_Frame
     (Address : Interfaces.Unsigned_64)
      return Stack_Frame_Type;

   -------------------------------------------------------------------------

   procedure Analyse_Stack
     (RIP        :     Interfaces.Unsigned_64;
      RBP        :     Interfaces.Unsigned_64;
      Traces     : out Call_Trace_Type;
      Last_Index : out Natural)
   is
      use type Interfaces.Unsigned_64;

      Cur_Stack_Frame : Stack_Frame_Type
        := (Return_Address => RIP,
            Saved_RBP      => RBP);
   begin
      Traces := (others => 0);
      Last_Index := 0;

      for I in Traces'Range loop
         Traces (I) := Cur_Stack_Frame.Return_Address;
         Last_Index := I;
         exit when Cur_Stack_Frame.Saved_RBP = 0;
         Cur_Stack_Frame
           := Read_Stack_Frame (Address => Cur_Stack_Frame.Saved_RBP);
      end loop;
   end Analyse_Stack;

   -------------------------------------------------------------------------

   function Read_Stack_Frame
     (Address : Interfaces.Unsigned_64)
      return Stack_Frame_Type
     with SPARK_Mode => Off
   is
      Stack_Frame : Stack_Frame_Type
        with Address => System'To_Address (Address);
   begin
      return Stack_Frame;
   end Read_Stack_Frame;

end Backtraces;
