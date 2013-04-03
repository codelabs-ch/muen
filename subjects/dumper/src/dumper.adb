with System;

with SK.Console_VGA;
with SK.Console;

procedure Dumper
is

   --  Dumper console width and height.
   subtype Width_Type  is Natural range 1 .. 80;
   subtype Height_Type is Natural range 1 .. 10;

   package VGA is new SK.Console_VGA
     (Width_Type   => Width_Type,
      Height_Type  => Height_Type,
      Base_Address => System'To_Address (16#000b_8500#));

   package Text_IO is new SK.Console
     (Initialize      => VGA.Init,
      Output_New_Line => VGA.New_Line,
      Output_Char     => VGA.Put_Char);
begin
   Text_IO.Init;
   Text_IO.Put_Line (Item => "Dumper");
   Text_IO.New_Line;

   loop
      null;
   end loop;
end Dumper;
