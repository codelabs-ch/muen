with System;

with Skp;

with SK.CPU;
with SK.Console;
with SK.Console_VGA;
with SK.Hypercall;

with Interrupts;

with Crypt.Receiver;
with Crypt.Sender;
with Crypt.Hasher;

with Handler;

procedure Crypter
is

   use type SK.Word16;

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

   Client_Id : Skp.Subject_Id_Type;
   Request   : Crypt.Message_Type;
   Response  : Crypt.Message_Type;
begin
   Text_IO.Init;
   Text_IO.Put_Line (Item => "Crypter subject running");
   Text_IO.Put_Line (Item => "Waiting for requests...");
   Interrupts.Initialize;

   SK.CPU.Sti;

   loop
      SK.CPU.Hlt;
      Client_Id := Handler.Requesting_Subject;
      Text_IO.Put_String (Item => "Processing request from subject ");
      Text_IO.Put_Byte   (Item => SK.Byte (Client_Id));
      Text_IO.New_Line;

      Response := Crypt.Null_Message;
      Crypt.Receiver.Receive (Req => Request);
      if Request.Size'Valid then
         Text_IO.Put_String (Item => " Size : ");
         Text_IO.Put_Word16 (Item => Request.Size);
         Text_IO.New_Line;
         Crypt.Hasher.SHA256_Hash (Input  => Request,
                                   Output => Response);

         Text_IO.Put_String (Item => " Hash : ");
         for I in Crypt.Data_Range range 1 .. Response.Size loop
            Text_IO.Put_Byte (Item => Response.Data (I));
         end loop;
         Text_IO.New_Line;
      else
         Text_IO.Put_String (Item => "Invalid request message size ");
         Text_IO.Put_Word16 (Item => Request.Size);
         Text_IO.New_Line;
      end if;

      Crypt.Sender.Send (Res => Response);
      SK.Hypercall.Trigger_Event (Number => SK.Byte (Client_Id));
   end loop;
end Crypter;
