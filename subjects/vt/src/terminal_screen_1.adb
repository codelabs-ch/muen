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

with SK.Console_VGA;
with SK.Console;

package body Terminal_Screen_1
is

   subtype Width_Type  is Natural range 1 .. 80;
   subtype Height_Type is Natural range 1 .. 25;

   package VGA is new SK.Console_VGA
     (Width_Type   => Width_Type,
      Height_Type  => Height_Type,
      Base_Address => System'To_Address (16#10000#));

   package Text_IO is new SK.Console
     (Initialize      => VGA.Init,
      Output_New_Line => VGA.New_Line,
      Output_Char     => VGA.Put_Char);

   type State_Type is
     (State_Ground);

   Current_State : constant State_Type := State_Ground;

   subtype Printable_Range is Positive range 32 .. 127;

   -------------------------------------------------------------------------

   procedure Init
   is
   begin
      Text_IO.Init;
   end Init;

   -------------------------------------------------------------------------

   procedure Update (Char : Character)
   is
      Pos : constant Natural := Character'Pos (Char);
   begin
      case Current_State
      is
         when State_Ground =>
            if Pos in Printable_Range then
               Text_IO.Put_Char (Item => Char);
            elsif Pos = 10 then
               Text_IO.New_Line;
            end if;
      end case;
   end Update;

end Terminal_Screen_1;
