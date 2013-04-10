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

   function Is_Allowed
     (B    : IO_Bitmap_Type;
      Port : SK.Word16)
      return Boolean
   is
   begin
      return B (Port) = Allowed;
   end Is_Allowed;

   -------------------------------------------------------------------------

   function To_Stream
     (B : IO_Bitmap_Type)
      return Ada.Streams.Stream_Element_Array
   is
      use type Ada.Streams.Stream_Element_Offset;

      subtype IO_Bitmap_Stream is Ada.Streams.Stream_Element_Array
        (1 .. 2 * SK.Page_Size);

      function To_Stream_Convert is new Ada.Unchecked_Conversion
        (Source => IO_Bitmap_Type,
         Target => IO_Bitmap_Stream);
   begin
      return To_Stream_Convert (S => B);
   end To_Stream;

end Skp.IO_Ports;
