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

with System;

with Skp.Kernel;

package body SK.Timers
with
   Refined_State => (State => Subject_Timers)
is

   type Timer_Interface_Type is record
      Value  : SK.Word64;
      Vector : SK.Byte;
   end record;

   for Timer_Interface_Type use record
      Value  at 0 range 0 .. 63;
      Vector at 8 range 0 ..  7;
   end record;

   Null_Timer : constant Timer_Interface_Type
     := (Value  => SK.Word64'Last,
         Vector => 0);

   pragma $Build_Warnings (Off, "*padded by * bits");
   type Subject_Timer_Array is array
     (Skp.Subject_Id_Type) of Timer_Interface_Type
   with
      Independent_Components,
      Component_Size => Page_Size * 8,
      Alignment      => Page_Size;
   pragma $Build_Warnings (On, "*padded by * bits");

   --  Subject timer pages.
   Subject_Timers : Subject_Timer_Array
   with
      Volatile,
      Async_Readers,
      Async_Writers,
      Address => System'To_Address (Skp.Kernel.Subj_Timers_Address);

   -------------------------------------------------------------------------

   procedure Clear_Timer (Subject : Skp.Subject_Id_Type)
   with
      Refined_Global  => (In_Out => Subject_Timers),
      Refined_Depends => (Subject_Timers =>+ Subject)
   is
   begin
      Subject_Timers (Subject).Value := SK.Word64'Last;
   end Clear_Timer;

   -------------------------------------------------------------------------

   procedure Get_Timer
     (Subject :     Skp.Subject_Id_Type;
      Value   : out SK.Word64;
      Vector  : out SK.Byte)
   with
      Refined_Global  => (Input => Subject_Timers),
      Refined_Depends => ((Value, Vector) => (Subject_Timers, Subject))
   is
   begin
      Value  := Subject_Timers (Subject).Value;
      Vector := Subject_Timers (Subject).Vector;
   end Get_Timer;

   -------------------------------------------------------------------------

   procedure Init_Timer (Subject : Skp.Subject_Id_Type)
   with
      Refined_Global  => (In_Out => Subject_Timers),
      Refined_Depends => (Subject_Timers =>+ Subject)
   is
   begin
      Subject_Timers (Subject) := Null_Timer;
   end Init_Timer;

begin

   --  FIXME: Initialization of "Subject_Timers" hidden.

   pragma SPARK_Mode (Off);

end SK.Timers;
