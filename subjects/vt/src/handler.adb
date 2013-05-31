with System;

with SK.IO;
with SK.Apic;
with SK.Console;
with SK.Console_VGA;
with SK.Hypercall;

with VGA_Output;

package body Handler
is

   subtype Width_Type  is Natural range 1 .. 80;
   subtype Height_Type is Natural range 1 .. 25;

   package VGA is new SK.Console_VGA
     (Width_Type   => Width_Type,
      Height_Type  => Height_Type,
      Base_Address => System'To_Address (16#000b_9000#));

   package Text_IO is new SK.Console
     (Initialize      => VGA.Init,
      Output_New_Line => VGA.New_Line,
      Output_Char     => VGA.Put_Char);

   type Kbd_Driver_Type is record
      Scancode : SK.Byte;
   end record;

   --  Xv6 driver page, currently used to forward keyboard scancodes.
   Kbd_Driver : Kbd_Driver_Type;
   for Kbd_Driver'Address use System'To_Address (16#20000#);
   pragma Volatile (Kbd_Driver);

   --  PS/2 constants.

   Data_Port       : constant := 16#60#;
   Status_Register : constant := 16#64#;

   OUTPUT_BUFFER_STATUS : constant := 0;

   -------------------------------------------------------------------------

   procedure Handle_Interrupt (Vector : SK.Byte)
   is
      use type SK.Byte;
      use type VGA_Output.Slot_Range;

      Status, Data : SK.Byte;
   begin
      if Vector /= 33 then
         Text_IO.Put_String (Item => "Ignoring spurious interrupt ");
         Text_IO.Put_Byte   (Item => Vector);
         Text_IO.New_Line;
         return;
      end if;

      loop
         SK.IO.Inb (Port  => Status_Register,
                    Value => Status);
         exit when not SK.Bit_Test
           (Value => SK.Word64 (Status),
            Pos   => OUTPUT_BUFFER_STATUS);

         SK.IO.Inb (Port  => Data_Port,
                    Value => Data);

         case Data is
            when 1  =>
               Text_IO.Init;
            when 59 =>
               VGA.Enable_Cursor;
               VGA_Output.Set (Slot => 1);
               Text_IO.Put_Line ("Switching to VT 1");
            when 60 =>
               VGA.Disable_Cursor;
               VGA_Output.Set (Slot => 2);
               Text_IO.Put_Line ("Switching to VT 2");
            when 63 =>
               VGA.Disable_Cursor;
               VGA_Output.Set (Slot => 5);
               Text_IO.Put_Line ("Switching to VT 5");
            when 64 =>
               VGA.Disable_Cursor;
               VGA_Output.Set (Slot => 6);
               Text_IO.Put_Line ("Switching to VT 6");
            when others =>
               if VGA_Output.Get_Active_Slot = 1 then
                  Kbd_Driver.Scancode := Data;
                  SK.Hypercall.Trigger_Event (Number => 1);
               end if;
         end case;
      end loop;
   end Handle_Interrupt;

   -------------------------------------------------------------------------

   procedure Initialize
   is
   begin
      Text_IO.Init;
      Text_IO.Put_String (Item => "VT subject running on CPU ");
      Text_IO.Put_Byte   (Item => SK.Apic.Get_ID);
      Text_IO.New_Line;
   end Initialize;

end Handler;
