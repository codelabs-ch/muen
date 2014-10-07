--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with X86_64;

with Skp;

with SK.CPU;
with SK.Hypercall;

with Interrupts;

with Crypt.Receiver;
with Crypt.Sender;
with Crypt.Hasher;

pragma $Prove_Warnings (Off, "unit * is not referenced",
                        Reason => "Only used for debug");
with Subject.Text_IO;
with Crypt.Debug;
pragma $Prove_Warnings (On, "unit * is not referenced");

with Handler;

procedure Crypter
with
   Global =>
     (Input  => Crypt.Receiver.State,
      Output => Crypt.Sender.State,
      In_Out => (X86_64.State, Interrupts.State, Handler.Requesting_Subject))
is
   Client_Id : Skp.Subject_Id_Type;
   Request   : Crypt.Message_Type;
   Response  : Crypt.Message_Type;
begin
   pragma Debug (Subject.Text_IO.Init);
   pragma Debug (Subject.Text_IO.Put_Line (Item => "Crypter subject running"));
   pragma Debug (Subject.Text_IO.Put_Line (Item => "Waiting for requests..."));
   Interrupts.Initialize;

   SK.CPU.Sti;

   loop
      SK.CPU.Hlt;
      Client_Id := Handler.Requesting_Subject;
      pragma Debug (Subject.Text_IO.Put_String
                    (Item => "Processing request from subject "));
      pragma Debug (Subject.Text_IO.Put_Byte   (Item => SK.Byte (Client_Id)));
      pragma Debug (Subject.Text_IO.New_Line);

      Response := Crypt.Null_Message;
      Crypt.Receiver.Receive (Req => Request);
      pragma Warnings (Off);
      if Request.Size'Valid then
         pragma Warnings (On);
         pragma Debug (Subject.Text_IO.Put_String (Item => " Size : "));
         pragma Debug (Subject.Text_IO.Put_Word16 (Item => Request.Size));
         pragma Debug (Subject.Text_IO.New_Line);
         Crypt.Hasher.SHA256_Hash (Input  => Request,
                                   Output => Response);
         pragma Debug (Subject.Text_IO.Put_String (Item => " Hash : "));
         pragma Debug (Crypt.Debug.Put_Message (Item => Response));
         pragma Debug (Subject.Text_IO.New_Line);
      end if;
      pragma Debug (not Request.Size'Valid,
                    Subject.Text_IO.Put_String
                      (Item => "Invalid request message size "));
      pragma Debug (not Request.Size'Valid,
                    Subject.Text_IO.Put_Word16 (Item => Request.Size));
      pragma Debug (not Request.Size'Valid, Subject.Text_IO.New_Line);

      Crypt.Sender.Send (Res => Response);
      SK.Hypercall.Trigger_Event (Number => SK.Byte (Client_Id));
   end loop;
end Crypter;
