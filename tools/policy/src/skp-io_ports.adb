with Ada.Unchecked_Conversion;

package body Skp.IO_Ports
is

   -------------------------------------------------------------------------

   procedure Allow_Ports
     (B          : in out IO_Bitmap_Type;
      Start_Port :        SK.Word16;
      End_Port   :        SK.Word16)
   is
   begin
      for Port in SK.Word16 range Start_Port .. End_Port loop
         B (Port) := Allowed;
      end loop;
   end Allow_Ports;

   -------------------------------------------------------------------------

   procedure Deny_Ports
     (B          : in out IO_Bitmap_Type;
      Start_Port :        SK.Word16;
      End_Port   :        SK.Word16)
   is
   begin
      for Port in SK.Word16 range Start_Port .. End_Port loop
         B (Port) := Denied;
      end loop;
   end Deny_Ports;

   -------------------------------------------------------------------------

   function To_Stream
     (B : IO_Bitmap_Type)
      return IO_Bitmap_Stream
   is
      function Convert is new Ada.Unchecked_Conversion
        (Source => IO_Bitmap_Type,
         Target => IO_Bitmap_Stream);
   begin
      return Convert (S => B);
   end To_Stream;

end Skp.IO_Ports;
