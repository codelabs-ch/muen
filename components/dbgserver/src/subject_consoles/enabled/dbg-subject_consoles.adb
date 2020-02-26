--
--  Copyright (C) 2023  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2023  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Dbgserver_Component.Channel_Arrays;

with Dbg.Subject_Consoles.Stream.Reader;
with Dbg.Subject_Consoles.Stream.Writer_Instance;

package body Dbg.Subject_Consoles
is

   package Cspecs renames Dbgserver_Component.Channel_Arrays;

   pragma Compile_Time_Error
     ((Cspecs.Subject_Consoles_In_Element_Size /=
          Cspecs.Subject_Consoles_Out_Element_Size),
      "Subject Console input and output channel size mismatch");

   pragma Compile_Time_Error
     ((Cspecs.Subject_Consoles_In_Element_Count /=
          Cspecs.Subject_Consoles_Out_Element_Count),
      "Subject Console input and output channel count mismatch");

   type Extended_Subject_Console_Range is new Natural range
     0 .. Cspecs.Subject_Consoles_In_Element_Count;

   type Subject_Console_Range is new Extended_Subject_Console_Range range
     1 .. Extended_Subject_Console_Range'Last;

   No_Console : constant Extended_Subject_Console_Range
     := Extended_Subject_Console_Range'First;

   type Console_Readers_Array is
     array (Subject_Console_Range) of Stream.Reader.Reader_Type;

   type Console_Channels_Array is
     array (Subject_Console_Range) of Stream.Channel_Type;

   Consoles_In : Console_Channels_Array
   with
      Async_Readers,
      Address => System'To_Address (Cspecs.Subject_Consoles_In_Address_Base),
      Size    => Cspecs.Subject_Consoles_In_Element_Size
        * Cspecs.Subject_Consoles_In_Element_Count * 8;

   Consoles_Out : Console_Channels_Array
   with
      Async_Writers,
      Address => System'To_Address (Cspecs.Subject_Consoles_Out_Address_Base),
      Size    => Cspecs.Subject_Consoles_Out_Element_Size
       * Cspecs.Subject_Consoles_Out_Element_Count * 8;

   --  Console reader states.
   Readers : Console_Readers_Array;

   --  ID of currently attached subject console.
   Attached_Console : Extended_Subject_Console_Range;

   -------------------------------------------------------------------------

   procedure Attach
     (ID      :     Positive;
      Success : out Boolean)
   is
   begin
      if ID <= Natural (Subject_Console_Range'Last) then
         Attached_Console := Extended_Subject_Console_Range (ID);
         Success := True;
      else
         Success := False;
      end if;
   end Attach;

   -------------------------------------------------------------------------

   procedure Detach
   is
   begin
      Attached_Console := No_Console;
   end Detach;

   -------------------------------------------------------------------------

   procedure Get
     (Data    : out Interfaces.Unsigned_8;
      Success : out Boolean)
   is
      use type Dbg.Subject_Consoles.Stream.Reader.Result_Type;

      Read_Result : Stream.Reader.Result_Type;
   begin
      if Attached_Console /= No_Console then
         Stream.Reader.Read
           (Channel => Consoles_Out (Subject_Console_Range (Attached_Console)),
            Reader  => Readers (Subject_Console_Range (Attached_Console)),
            Element => Data,
            Result  => Read_Result);
         Success := Read_Result = Stream.Reader.Success;
      else
         Data := 0;
         Success := False;
      end if;
   end Get;

   -------------------------------------------------------------------------

   procedure Init
   is
   begin
      for I in Subject_Console_Range loop
         Stream.Writer_Instance.Initialize
           (Channel => Consoles_In (I),
            Epoch   => 1);
      end loop;

      Attached_Console := No_Console;
   end Init;

   -------------------------------------------------------------------------

   procedure Put (Data : Interfaces.Unsigned_8)
   is
   begin
      if Attached_Console /= No_Console then
         Stream.Writer_Instance.Write
           (Channel => Consoles_In (Subject_Console_Range (Attached_Console)),
            Element => Data);
      end if;
   end Put;

end Dbg.Subject_Consoles;
