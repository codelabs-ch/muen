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

with Muchannel.Reader;

with Log;

package body Terminals
is

   --  Virtual text console framebuffer.
   type Framebuffer_Type is array (1 .. SK.Page_Size) of SK.Byte;
   for Framebuffer_Type'Size use 8 * SK.Page_Size;

   Framebuffers : array (Slot_Range) of Framebuffer_Type;
   for Framebuffers'Address use System'To_Address (16#10000#);
   for Framebuffers'Size use Slot_Range'Last * 8 * SK.Page_Size;

   --  VGA output page.
   VGA_Out : Framebuffer_Type;
   for VGA_Out'Address use System'To_Address (16#000b_8000#);

   Active_Slot : Slot_Range := Slot_Range'First;
   pragma Atomic (Active_Slot);

   package VT_Channel is new Muchannel
     (Element_Type => Character,
      Elements     => 4032);

   package VT_Channel_Rdr is new VT_Channel.Reader (Protocol => 1);

   Channel_1_In : VT_Channel.Channel_Type;
   for Channel_1_In'Address use System'To_Address (16#40000#);

   Channel_1_Reader : VT_Channel_Rdr.Reader_Type;

   subtype Width_Type  is Natural range 1 .. 80;
   subtype Height_Type is Natural range 1 .. 25;

   package Terminal_1 is new SK.Console_VGA
     (Width_Type   => Width_Type,
      Height_Type  => Height_Type,
      Base_Address => System'To_Address (16#10000#));

   package Terminal_1_IO is new SK.Console
     (Initialize      => Terminal_1.Init,
      Output_New_Line => Terminal_1.New_Line,
      Output_Char     => Terminal_1.Put_Char);

   --  Read data from input channels if new data is present.
   procedure Update_In_Channels;

   --  Process given character.
   procedure Process_Char (Data : Character);

   -------------------------------------------------------------------------

   function Get_Active_Slot return Slot_Range
   is
   begin
      return Active_Slot;
   end Get_Active_Slot;

   -------------------------------------------------------------------------

   procedure Initialize
   is
      use type VT_Channel_Rdr.Result_Type;

      Res : VT_Channel_Rdr.Result_Type;
   begin
      Terminal_1_IO.Init;

      Res := VT_Channel_Rdr.Inactive;

      loop
         VT_Channel_Rdr.Synchronize (Channel => Channel_1_In,
                                     Reader  => Channel_1_Reader,
                                     Result  => Res);
         exit when Res /= VT_Channel_Rdr.Inactive;
      end loop;
   end Initialize;

   -------------------------------------------------------------------------

   procedure Process_Char (Data : Character)
   is
   begin
      if Character'Pos (Data) >= 32 then

         --  Printable character.

         Terminal_1_IO.Put_Char (Item => Data);
      else
         case Data is
            when ASCII.LF => Terminal_1_IO.New_Line;
            when others   => null;
         end case;
      end if;
   end Process_Char;

   -------------------------------------------------------------------------

   procedure Run
   is
      use type SK.Byte;
   begin
      loop
         for I in VGA_Out'Range loop
            Update_In_Channels;

            if VGA_Out (I) /= Framebuffers (Active_Slot) (I) then
               VGA_Out (I) := Framebuffers (Active_Slot) (I);
            end if;
         end loop;
      end loop;
   end Run;

   -------------------------------------------------------------------------

   procedure Set (Slot : Slot_Range)
   is
   begin
      Active_Slot := Slot;
   end Set;

   -------------------------------------------------------------------------

   procedure Update_In_Channels
   is
      use type VT_Channel_Rdr.Result_Type;

      Data : Character;
      Res  : VT_Channel_Rdr.Result_Type;
   begin
      if Active_Slot /= 1 then
         return;
      end if;

      loop
         VT_Channel_Rdr.Read (Channel => Channel_1_In,
                              Reader  => Channel_1_Reader,
                              Element => Data,
                              Result  => Res);
         case Res is
            when VT_Channel_Rdr.Incompatible_Interface =>
               Log.Text_IO.Put_Line
                 ("Channel 1: Incompatible interface detected");
            when VT_Channel_Rdr.Epoch_Changed =>
               Log.Text_IO.Put_Line ("Channel 1: Epoch changed");
               VT_Channel_Rdr.Synchronize (Channel => Channel_1_In,
                                           Reader  => Channel_1_Reader,
                                           Result  => Res);
            when VT_Channel_Rdr.No_Data =>
               null;
            when VT_Channel_Rdr.Overrun_Detected =>
               Log.Text_IO.Put_Line ("Channel 1: Overrun detected");
            when VT_Channel_Rdr.Inactive =>
               Log.Text_IO.Put_Line ("Channel 1: Inactive");
            when VT_Channel_Rdr.Success =>
               Process_Char (Data => Data);
         end case;

         exit when Res /= VT_Channel_Rdr.Success;
      end loop;
   end Update_In_Channels;

end Terminals;
