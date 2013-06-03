with SK.Console;

with Crypt.VGA;

package Crypt.Text_IO is new SK.Console
  (Initialize      => Crypt.VGA.Init,
   Output_New_Line => Crypt.VGA.New_Line,
   Output_Char     => Crypt.VGA.Put_Char);
