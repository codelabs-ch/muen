--
--  Copyright (C) 2019  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2019  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Exceptions;

with Mutools.Constants;
with Mutools.Utils;

package body Cmd_Stream.Utils
is

   use Ada.Strings.Unbounded;

   --  Create command node with specified name and attributes.
   function Create_Command
     (Name  : String;
      Attrs : Attribute_Array := Null_Attrs)
      return Unbounded_String;

   -------------------------------------------------------------------------

   procedure Append_Command
     (Stream_Doc : in out Stream_Document_Type;
      Name       :        String;
      Attrs      :        Attribute_Array := Null_Attrs)
   is
   begin
      Ada.Text_IO.Put_Line (File => Stream_Doc.File,
                            Item => S (Create_Command
                              (Name  => Name,
                               Attrs => Attrs)));
   end Append_Command;

   -------------------------------------------------------------------------

   procedure Append_Command
     (Buffer : in out Command_Buffer_Type;
      Name   :        String;
      Attrs  :        Attribute_Array := Null_Attrs)
   is
   begin
      Buffer.Cmds.Append (New_Item => Create_Command
        (Name  => Name,
         Attrs => Attrs));
   end Append_Command;

   -------------------------------------------------------------------------

   procedure Append_Commands
     (Stream_Doc : in out Stream_Document_Type;
      Buffer     :        Command_Buffer_Type)
   is
   begin
      Ada.Text_IO.New_Line (File => Stream_Doc.File);
      for Cmd of Buffer.Cmds loop
         Ada.Text_IO.Put_Line (File => Stream_Doc.File,
                               Item => S (Cmd));
      end loop;
   end Append_Commands;

   -------------------------------------------------------------------------

   procedure Clear_Region
     (Stream_Doc   : in out Stream_Document_Type;
      Base_Address :        Interfaces.Unsigned_64;
      Size         :        Interfaces.Unsigned_64)
   is
      use type Interfaces.Unsigned_64;

      Page_Count : constant Interfaces.Unsigned_64
        := Size / Mutools.Constants.Page_Size;
   begin
      Append_Command
        (Stream_Doc => Stream_Doc,
         Name       => "clearPages",
         Attrs      => ((Attr  => U ("basePage"),
                         Value => U (Mutools.Utils.To_Hex
                           (Number => Base_Address))),
                        (Attr  => U ("count"),
                         Value => U (Trim (Page_Count'Img)))));
   end Clear_Region;

   -------------------------------------------------------------------------

   procedure Close (Stream_Doc : in out Stream_Document_Type)
   is
   begin
      Ada.Text_IO.Put_Line (File => Stream_Doc.File,
                            Item => " </commands>" & ASCII.LF
                            & "</tau0>" & ASCII.LF);
      Ada.Text_IO.Close (File => Stream_Doc.File);
   end Close;

   -------------------------------------------------------------------------

   procedure Create
     (Stream_Doc : out Stream_Document_Type;
      Filename   :     String)
   is
   begin
      Ada.Text_IO.Create
        (File => Stream_Doc.File,
         Mode => Ada.Text_IO.Out_File,
         Name => Filename);
      Ada.Text_IO.Put_Line (File => Stream_Doc.File,
                            Item => "<tau0>" & ASCII.LF & " <commands>");

   exception
      when E : others =>
         raise IO_Error with "Unable to open file '" & Filename & "' - "
           & Ada.Exceptions.Exception_Message (X => E);
   end Create;

   -------------------------------------------------------------------------

   function Create_Command
     (Name  : String;
      Attrs : Attribute_Array := Null_Attrs)
      return Ada.Strings.Unbounded.Unbounded_String
   is
   begin
      return Result : Ada.Strings.Unbounded.Unbounded_String do
         Result := "  <" & U (Name);

         for A of Attrs loop
            if A /= Null_Attr then
               Result := Result & " " & A.Attr & "=""" & A.Value & """";
            end if;
         end loop;
         Result := Result & "/>";
      end return;
   end Create_Command;

   -------------------------------------------------------------------------

   procedure Reverse_Commands (Buffer : in out Command_Buffer_Type)
   is
   begin
      Buffer.Cmds.Reverse_Elements;
   end Reverse_Commands;

end Cmd_Stream.Utils;
