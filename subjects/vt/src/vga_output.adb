with System;

with SK;

package body VGA_Output
is

   --  Virtual text console framebuffer.
   type Framebuffer_Type is array (1 .. SK.Page_Size) of SK.Byte;
   for Framebuffer_Type'Size use 32768;

   --  Framebuffer base address.
   Framebuffer_Base : constant := 16#10000#;

   --  VGA output page.
   VGA_Out : Framebuffer_Type;
   for VGA_Out'Address use System'To_Address (16#000b_8000#);

   Current_Slot : Slot_Range := Slot_Range'First;

   -------------------------------------------------------------------------

   procedure Set (Slot : Slot_Range)
   is
   begin
      Current_Slot := Slot;
   end Set;

   -------------------------------------------------------------------------

   procedure Sync
   is
      use type SK.Byte;
   begin
      loop
         declare
            Fb : Framebuffer_Type;
            for Fb'Address use System'To_Address
              (Framebuffer_Base + SK.Page_Size * Natural (Current_Slot - 1));
         begin
            for I in Fb'Range loop
               if VGA_Out (I) /= Fb (I) then
                  VGA_Out (I) := Fb (I);
               end if;
            end loop;
         end;
      end loop;
   end Sync;

end VGA_Output;
