with System.Machine_Code;

with SK.Console;
with SK.Console_VGA;

with SubjC;

procedure Subject_C
is

   use type SK.Word32;

   subtype Width_Type  is Natural range 1 .. 80;
   subtype Height_Type is Natural range 1 .. 3;

   package VGA is new SK.Console_VGA
     (Width_Type   => Width_Type,
      Height_Type  => Height_Type,
      Base_Address => System'To_Address (16#000b_8500#));

   package Text_IO is new SK.Console
     (Initialize      => VGA.Init,
      Output_New_Line => VGA.New_Line,
      Output_Char     => VGA.Put_Char);

   Counter : SK.Word32 := 0;
begin
   SubjC.Initialize;

   Text_IO.Init;
   Text_IO.Put_Line (Item => "Subject C");
   Text_IO.New_Line;

   System.Machine_Code.Asm
     (Template => "sti",
      Volatile => True);
   loop
      if Counter mod 2 ** 15 = 0 then
         VGA.Set_Position (X => 1,
                           Y => 3);
         Text_IO.Put_String (Item => "Interrupts: ");
         Text_IO.Put_Word64 (Item => SubjC.Counter);
      end if;
      Counter := Counter + 1;
   end loop;
end Subject_C;
