with SK.Console;
with SK.Console_Serial;

--  Kernel debug console.
package SK.KC is new SK.Console
  (Initialize      => Console_Serial.Init,
   Output_New_Line => Console_Serial.New_Line,
   Output_Char     => Console_Serial.Put_Char);
