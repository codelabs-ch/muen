with System;

with SK.Console_VGA;

package Subject.VGA is new SK.Console_VGA
  (Width_Type   => Subject.Width_Type,
   Height_Type  => Subject.Height_Type,
   Base_Address => System'To_Address (16#000b_8000#));
