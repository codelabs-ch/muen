--
--  Copyright (C) 2013, 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Debuglog.Client;

with SK.Strings;

with Musinfo.Instance;

with Example_Component.Config;

package body Crypt.Debug
is

   use SK.Strings;

   -------------------------------------------------------------------------

   procedure Put_Greeter
   is
   begin
      Debuglog.Client.Put (Item => Example_Component.Config.Greeter);
      pragma Debug
        (Example_Component.Config.Print_Serial,
         Debuglog.Client.Put
           (Item => "Serial " & Img
                (SK.Word64 (Example_Component.Config.Serial))));
      pragma Debug
        (Musinfo.Instance.Is_Valid and then
         Example_Component.Config.Print_Vcpu_Speed,
         Debuglog.Client.Put
           (Item => "VCPU running with " & Img
                (Musinfo.Instance.TSC_Khz) & " Khz"));
      pragma Debug
        (not Musinfo.Instance.Is_Valid,
         Debuglog.Client.Put
           (Item => "Warning: sinfo invalid"));

      Debuglog.Client.Put (Item => "Waiting for requests...");
   end Put_Greeter;

   -------------------------------------------------------------------------

   procedure Put_Hash (Item : Crypt.Message_Type)
   is
   begin
      Debuglog.Client.Put (Item => " Hash: ");
      for I in Crypt.Data_Range range 1 .. Item.Size loop
         Debuglog.Client.Put (Item => Img (Item.Data (I)));
      end loop;
      Debuglog.Client.New_Line;
   end Put_Hash;

   -------------------------------------------------------------------------

   procedure Put_Process_Message
   is
   begin
      Debuglog.Client.Put_Line (Item => "Processing request from client");
   end Put_Process_Message;

   -------------------------------------------------------------------------

   procedure Put_Vector (Vector : SK.Byte)
   is
   begin
      Debuglog.Client.Put_Line
        (Item => "Received vector " & Img (Vector) & " => wakeup");
   end Put_Vector;

   -------------------------------------------------------------------------

   procedure Put_Word16
     (Message : String;
      Value   : SK.Word16)
   is
   begin
      Debuglog.Client.Put      (Item => Message);
      Debuglog.Client.Put_Line (Item => " " & Img (Value));
   end Put_Word16;

end Crypt.Debug;
