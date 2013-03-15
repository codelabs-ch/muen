with System;

with SK.Console;
with SK.Console_VGA;

procedure Subject_2
is

   use type SK.Word32;

   --  Test subject console width and height.
   subtype Width_Type  is Natural range 1 .. 80;
   subtype Height_Type is Natural range 1 .. 12;

   package VGA is new SK.Console_VGA
     (Width_Type   => Width_Type,
      Height_Type  => Height_Type,
      Base_Address => System'To_Address (16#000b_8820#));

   package Text_IO is new SK.Console
     (Initialize      => VGA.Init,
      Output_New_Line => VGA.New_Line,
      Output_Char     => VGA.Put_Char);

   Name    : constant String := "Subject 2";
   Counter : SK.Word32       := 0;
   Idx     : Positive        := 1;
   Dlt     : Integer         := -1;
begin
   Text_IO.Init;
   Text_IO.Put_Line (Item => Name);
   Text_IO.New_Line;

   for I in Name'Range loop
      Text_IO.Put_Char (Item => Character'Val (176));
   end loop;

   loop
      if Counter mod 2**18 = 0 then
         VGA.Set_Position (X => Integer (Idx - Dlt),
                           Y => 3);
         Text_IO.Put_Char (Item => Character'Val (176));
         if Idx = Name'Last then
            Dlt := -1;
         elsif Idx = Name'First then
            Dlt := 1;
         end if;
         VGA.Set_Position (X => Integer (Idx),
                           Y => 3);
         Text_IO.Put_Char (Item => Character'Val (178));
         Idx := Idx + Dlt;
      end if;
      Counter := Counter + 1;
   end loop;

end Subject_2;
