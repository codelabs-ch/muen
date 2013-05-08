with Ada.Streams;

package Skp.MSRs
is

   --  MSR bitmap as specified by Intel SDM Vol. 3C, section 24.6.9. This type
   --  encompasses the whole MSR bitmap range (read & write, low & high).
   type MSR_Bitmap_Type is private;

   Null_MSR_Bitmap : constant MSR_Bitmap_Type;

   --  Sets flag in MSR bitmap to allow access of given mode to address range
   --  specified by start and end address (inclusive).
   procedure Allow_MSRs
     (Bitmap     : in out MSR_Bitmap_Type;
      Start_Addr :        SK.Word32;
      End_Addr   :        SK.Word32;
      Mode       :        MSR_Mode_Type);

   use type Ada.Streams.Stream_Element_Offset;

   subtype MSR_Bitmap_Stream is Ada.Streams.Stream_Element_Array
     (0 .. SK.Page_Size - 1);

   --  Convert MSR bitmap to binary stream.
   function To_Stream (Bitmap : MSR_Bitmap_Type) return MSR_Bitmap_Stream;

private

   type MSR_Flag is mod 2 ** 1;
   for MSR_Flag'Size use 1;

   Allowed : constant MSR_Flag := 0;
   Denied  : constant MSR_Flag := 1;

   type MSR_Bitmap_Type is array (0 .. SK.Page_Size * 8 - 1) of MSR_Flag;
   pragma Pack (MSR_Bitmap_Type);

   for MSR_Bitmap_Type'Size use SK.Page_Size * 8;

   Null_MSR_Bitmap : constant MSR_Bitmap_Type :=
     MSR_Bitmap_Type'(others => Denied);

end Skp.MSRs;
