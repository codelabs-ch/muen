with System.Machine_Code;

with SK.CPU;
with SK.Console;
with SK.Console_VGA;

procedure Crypter
is

   subtype Width_Type  is Natural range 1 .. 80;
   subtype Height_Type is Natural range 1 .. 25;

   package VGA is new SK.Console_VGA
     (Width_Type   => Width_Type,
      Height_Type  => Height_Type,
      Base_Address => System'To_Address (16#000b_8000#));

   package Text_IO is new SK.Console
     (Initialize      => VGA.Init,
      Output_New_Line => VGA.New_Line,
      Output_Char     => VGA.Put_Char);
begin
   Text_IO.Init;
   Text_IO.Put_Line (Item => "Crypter subject running");
   Text_IO.Put_Line (Item => "Waiting for requests...");

   System.Machine_Code.Asm
     (Template => "sti",
      Volatile => True);
   loop
      SK.CPU.Hlt;
   end loop;
end Crypter;
