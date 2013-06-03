with Skp;

with SK.CPU;
with SK.Hypercall;

with Interrupts;

with Crypt.Receiver;
with Crypt.Sender;
with Crypt.Hasher;
with Crypt.Text_IO;
with Crypt.Debug;

with Handler;

procedure Crypter
is
   Client_Id : Skp.Subject_Id_Type;
   Request   : Crypt.Message_Type;
   Response  : Crypt.Message_Type;
begin
   pragma Debug (Crypt.Text_IO.Init);
   pragma Debug (Crypt.Text_IO.Put_Line (Item => "Crypter subject running"));
   pragma Debug (Crypt.Text_IO.Put_Line (Item => "Waiting for requests..."));
   Interrupts.Initialize;

   SK.CPU.Sti;

   loop
      SK.CPU.Hlt;
      Client_Id := Handler.Requesting_Subject;
      pragma Debug (Crypt.Text_IO.Put_String
                    (Item => "Processing request from subject "));
      pragma Debug (Crypt.Text_IO.Put_Byte   (Item => SK.Byte (Client_Id)));
      pragma Debug (Crypt.Text_IO.New_Line);

      Response := Crypt.Null_Message;
      Crypt.Receiver.Receive (Req => Request);
      if Request.Size'Valid then
         pragma Debug (Crypt.Text_IO.Put_String (Item => " Size : "));
         pragma Debug (Crypt.Text_IO.Put_Word16 (Item => Request.Size));
         pragma Debug (Crypt.Text_IO.New_Line);
         Crypt.Hasher.SHA256_Hash (Input  => Request,
                                   Output => Response);
         pragma Debug (Crypt.Text_IO.Put_String (Item => " Hash : "));
         pragma Debug (Crypt.Debug.Put_Message (Item => Response));
         pragma Debug (Crypt.Text_IO.New_Line);
      end if;
      pragma Debug (not Request.Size'Valid,
                    Crypt.Text_IO.Put_String
                      (Item => "Invalid request message size "));
      pragma Debug (not Request.Size'Valid,
                    Crypt.Text_IO.Put_Word16 (Item => Request.Size));
      pragma Debug (not Request.Size'Valid, Crypt.Text_IO.New_Line);

      Crypt.Sender.Send (Res => Response);
      SK.Hypercall.Trigger_Event (Number => SK.Byte (Client_Id));
   end loop;
end Crypter;
