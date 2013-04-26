with System;

with SK;

package body VGA_Output
is

   --  Virtual text console framebuffer.
   type Framebuffer_Type is array (1 .. SK.Page_Size) of SK.Byte;
   for Framebuffer_Type'Size use 32768;

   Framebuffers : array (Slot_Range) of Framebuffer_Type;
   for Framebuffers'Address use System'To_Address (16#10000#);
   for Framebuffers'Size use Slot_Range'Last * 32768;

   --  VGA output page.
   VGA_Out : Framebuffer_Type;
   for VGA_Out'Address use System'To_Address (16#000b_8000#);

   Current_Slot : Slot_Range := Slot_Range'First;
   pragma Atomic (Current_Slot);

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
         for I in VGA_Out'Range loop
            if VGA_Out (I) /= Framebuffers (Current_Slot) (I) then
               VGA_Out (I) := Framebuffers (Current_Slot) (I);
            end if;
         end loop;
      end loop;
   end Sync;

end VGA_Output;
