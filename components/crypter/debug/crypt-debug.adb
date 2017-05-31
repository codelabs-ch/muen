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

package body Crypt.Debug
is

   -------------------------------------------------------------------------

   procedure Put_Greeter
   is
   begin
      Debuglog.Client.Put (Item => "Crypter subject running");
      Debuglog.Client.Put (Item => "Waiting for requests...");
   end Put_Greeter;

   -------------------------------------------------------------------------

   procedure Put_Hash (Item : Crypt.Message_Type)
   is
   begin
      Debuglog.Client.Put (Item => " Hash: ");
      for I in Crypt.Data_Range range 1 .. Item.Size loop
         Debuglog.Client.Put_Byte (Item => Item.Data (I));
      end loop;
      Debuglog.Client.New_Line;
   end Put_Hash;

   -------------------------------------------------------------------------

   procedure Put_Process_Message (Client_ID : SK.Byte)
   is
   begin
      Debuglog.Client.Put      (Item => "Processing request from subject ");
      Debuglog.Client.Put_Byte (Item => Client_ID);
      Debuglog.Client.New_Line;
   end Put_Process_Message;

   -------------------------------------------------------------------------

   procedure Put_Spurious (Vector : SK.Byte)
   is
   begin
      Debuglog.Client.Put      (Item => "Ignoring spurious interrupt ");
      Debuglog.Client.Put_Byte (Item => Vector);
      Debuglog.Client.New_Line;
   end Put_Spurious;

   -------------------------------------------------------------------------

   procedure Put_Word16
     (Message : String;
      Value   : SK.Word16)
   is
   begin
      Debuglog.Client.Put_Reg16
        (Name  => Message,
         Value => Value);
   end Put_Word16;

end Crypt.Debug;
