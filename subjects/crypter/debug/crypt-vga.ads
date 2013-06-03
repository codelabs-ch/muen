with System;

with SK.Console_VGA;

package Crypt.VGA is new SK.Console_VGA
  (Width_Type   => Crypt.Width_Type,
   Height_Type  => Crypt.Height_Type,
   Base_Address => System'To_Address (16#000b_8000#));
