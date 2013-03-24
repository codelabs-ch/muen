-----------------------------------------------------------------------
--  Appenders -- Log appenders
--  Copyright (C) 2001, 2002, 2003, 2006, 2008, 2009, 2010, 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------
with Ada.Calendar.Formatting;
with Ada.Strings;
with Ada.Strings.Fixed;

with Util.Strings.Transforms;
with Util.Log.Loggers;
package body Util.Log.Appenders is

   use Ada;

   --  ------------------------------
   --  Get the log level that triggers display of the log events
   --  ------------------------------
   function Get_Level (Self : in Appender) return Level_Type is
   begin
      return Self.Level;
   end Get_Level;

   --  ------------------------------
   --  Set the log level.
   --  ------------------------------
   procedure Set_Level (Self       : in out Appender;
                        Name       : in String;
                        Properties : in Util.Properties.Manager;
                        Level      : in Level_Type) is
      Prop_Name : constant String := Name & ".level";
   begin
      if Properties.Exists (Prop_Name) then
         Self.Level := Get_Level (Properties.Get (Prop_Name), Level);
      else
         Self.Level := Level;
      end if;
   end Set_Level;

   --  ------------------------------
   --  Set the log layout format.
   --  ------------------------------
   procedure Set_Layout (Self       : in out Appender;
                         Name       : in String;
                         Properties : in Util.Properties.Manager;
                         Layout     : in Layout_Type) is
      use Ada.Strings;
      use Util.Strings.Transforms;
      Prop_Name : constant String := Name & ".layout";
   begin
      if Properties.Exists (Prop_Name) then
         declare
            Value : constant String
              := To_Lower_Case (Fixed.Trim (Properties.Get (Prop_Name), Both));
         begin
            if Value = "message" then
               Self.Layout := MESSAGE;
            elsif Value = "level-message" then
               Self.Layout := LEVEL_MESSAGE;
            elsif Value = "date-level-message" or Value = "level-date-message" then
               Self.Layout := DATE_LEVEL_MESSAGE;
            else
               Self.Layout := FULL;
            end if;
         end;
      else
         Self.Layout := Layout;
      end if;
   end Set_Layout;

   --  ------------------------------
   --  Format the event into a string
   --  ------------------------------
   function Format (Self  : in Appender;
                    Event : in Log_Event) return String is
   begin
      case Self.Layout is
         when MESSAGE =>
            return To_String (Event.Message);

         when LEVEL_MESSAGE =>
            return Get_Level_Name (Event.Level) & ": " & To_String (Event.Message);

         when DATE_LEVEL_MESSAGE =>
            return "[" & Calendar.Formatting.Image (Event.Time) & "] "
              & Get_Level_Name (Event.Level) & ": "
              & To_String (Event.Message);

         when FULL =>
            return "[" & Calendar.Formatting.Image (Event.Time) & "] "
              & Get_Level_Name (Event.Level) & " - "
              & Loggers.Get_Logger_Name (Event.Logger.all) & " - "
              & To_String (Event.Message);

      end case;
   end Format;

   procedure Append (Self  : in out File_Appender;
                     Event : in Log_Event) is
   begin
      if Self.Level >= Event.Level then
         Text_IO.Put_Line (Self.Output, Format (Self, Event));
      end if;
   end Append;

   --  ------------------------------
   --  Flush the log events.
   --  ------------------------------
   overriding
   procedure Flush (Self   : in out File_Appender) is
   begin
      Text_IO.Flush (Self.Output);
   end Flush;

   --  Flush and close the file.
   overriding
   procedure Finalize (Self : in out File_Appender) is
   begin
      Self.Flush;
      Text_IO.Close (File => Self.Output);
   end Finalize;

   --  ------------------------------
   --  Create a file appender and configure it according to the properties
   --  ------------------------------
   function Create_File_Appender (Name       : in String;
                                  Properties : in Util.Properties.Manager;
                                  Default    : in Level_Type)
                                  return Appender_Access is
      Path   : constant String := Properties.Get (Name & ".File");
      Result : constant File_Appender_Access := new File_Appender;
   begin
      Result.Set_Level (Name, Properties, Default);
      Result.Set_Layout (Name, Properties, FULL);
      Text_IO.Open (File => Result.Output,
                    Name => Path,
                    Mode => Text_IO.Out_File);
      return Result.all'Access;
   exception
      when Text_IO.Name_Error =>
         Text_IO.Create (File => Result.Output, Name => Path);
         return Result.all'Access;
   end Create_File_Appender;

   --  ------------------------------
   --  Set the file where the appender will write the logs
   --  ------------------------------
   procedure Set_File (Self : in out File_Appender;
                       Path : in String) is
   begin
      Text_IO.Open (File => Self.Output,
                    Name => Path,
                    Mode => Text_IO.Out_File);
   end Set_File;

   procedure Append (Self  : in out Console_Appender;
                     Event : in Log_Event) is
   begin
      if Self.Level >= Event.Level then
         Text_IO.Put_Line (Format (Self, Event));
      end if;
   end Append;

   --  ------------------------------
   --  Flush the log events.
   --  ------------------------------
   overriding
   procedure Flush (Self : in out Console_Appender) is
      pragma Unreferenced (Self);
   begin
      Text_IO.Flush;
   end Flush;

   overriding
   procedure Append (Self  : in out List_Appender;
                     Event : in Log_Event) is
   begin
      for I in 1 .. Self.Count loop
         Self.Appenders (I).Append (Event);
      end loop;
   end Append;

   --  ------------------------------
   --  Create a console appender and configure it according to the properties
   --  ------------------------------
   function Create_Console_Appender (Name       : in String;
                                     Properties : in Util.Properties.Manager;
                                     Default    : in Level_Type)
                                     return Appender_Access is
      Result : constant Console_Appender_Access := new Console_Appender;
   begin
      Result.Set_Level (Name, Properties, Default);
      Result.Set_Layout (Name, Properties, FULL);
      return Result.all'Access;
   end Create_Console_Appender;

   --  ------------------------------
   --  Flush the log events.
   --  ------------------------------
   overriding
   procedure Flush (Self   : in out List_Appender) is
   begin
      for I in 1 .. Self.Count loop
         Self.Appenders (I).Flush;
      end loop;
   end Flush;

   --  ------------------------------
   --  Add the appender to the list.
   --  ------------------------------
   procedure Add_Appender (Self   : in out List_Appender;
                           Object : in Appender_Access) is
   begin
      if Self.Count < Self.Appenders'Last then
         Self.Count := Self.Count + 1;
         Self.Appenders (Self.Count) := Object;
      end if;
   end Add_Appender;

   --  ------------------------------
   --  Create a list appender and configure it according to the properties
   --  ------------------------------
   function Create_List_Appender return List_Appender_Access is
      Result : constant List_Appender_Access := new List_Appender;
   begin
      return Result;
   end Create_List_Appender;

end Util.Log.Appenders;
