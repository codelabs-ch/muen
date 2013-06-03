with SK;

use type SK.Word16;

--# inherit
--#    SK;
package Crypt
is

   subtype Width_Type  is Natural range 1 .. 80;
   subtype Height_Type is Natural range 1 .. 25;

   subtype Data_Range is SK.Word16 range 1 .. 2048;

   type Data_Array is array (Data_Range) of SK.Byte;
   for Data_Array'Size use Integer (Data_Range'Last * 8);

   Null_Data : constant Data_Array;

   type Message_Type is record
      Size : Data_Range;
      Data : Data_Array;
   end record;
   for Message_Type'Size use (2 + 2048) * 8;

   Null_Message : constant Message_Type;

private

   Null_Data    : constant Data_Array   := Data_Array'(others => 0);
   Null_Message : constant Message_Type := Message_Type'(Size => 1,
                                                         Data => Null_Data);
end Crypt;
