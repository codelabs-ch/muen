with System.Machine_Code;

with Skp;

with SK.CPU;
with SK.Console;
with SK.Console_VGA;
with SK.Hypercall;

with Interrupts;

with Crypt.Receiver;
with Crypt.Sender;

with Handler;

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

   Client_Id : Skp.Subject_Id_Type;
   Request   : Crypt.Message_Type;
   Response  : Crypt.Message_Type;
begin
   Text_IO.Init;
   Text_IO.Put_Line (Item => "Crypter subject running");
   Text_IO.Put_Line (Item => "Waiting for requests...");
   Interrupts.Initialize;

   System.Machine_Code.Asm
     (Template => "sti",
      Volatile => True);

   loop
      SK.CPU.Hlt;
      Client_Id := Handler.Requesting_Subject;
      Text_IO.Put_String (Item => "Processing request from subject ");
      Text_IO.Put_Byte   (Item => SK.Byte (Client_Id));
      Text_IO.New_Line;

      Crypt.Receiver.Receive (Req => Request);
      Response := Request;
      Crypt.Sender.Send (Res => Response);

      SK.Hypercall.Trigger_Event (Number => SK.Byte (Client_Id));
   end loop;
end Crypter;
