with SK.Console;

with Subject.VGA;

package Subject.Text_IO is new SK.Console
  (Initialize      => Subject.VGA.Init,
   Output_New_Line => Subject.VGA.New_Line,
   Output_Char     => Subject.VGA.Put_Char);
