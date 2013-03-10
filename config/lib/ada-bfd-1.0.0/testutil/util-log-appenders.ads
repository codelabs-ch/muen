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
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Calendar;
with Ada.Finalization;
with Util.Properties;
limited with Util.Log.Loggers;

--  The log <b>Appender</b> will handle the low level operations to write
--  the log content to a file, the console, a database.
package Util.Log.Appenders is

   use Ada.Strings.Unbounded;

   --  ------------------------------
   --  Log event
   --  ------------------------------
   --  The <b>Log_Event</b> represent a log message reported by one of the
   --  <b>log</b> operation (Debug, Info, Warn, Error).
   type Log_Event is record
      --  The log message (formatted)
      Message : Unbounded_String;

      --  The timestamp when the message was produced.
      Time    : Ada.Calendar.Time;

      --  The log level
      Level   : Level_Type;

      --  The logger
      Logger  : access Util.Log.Loggers.Logger_Info;
   end record;

   --  The layout type to indicate how to format the message.
   --  Unlike Logj4, there is no customizable layout.
   type Layout_Type
     is (
         --  The <b>message</b> layout with only the log message.
         --  Ex: "Cannot open file"
         MESSAGE,

         --  The <b>level-message</b> layout with level and message.
         --  Ex: "ERROR: Cannot open file"
         LEVEL_MESSAGE,

         --  The <b>date-level-message</b> layout with date
         --  Ex: "2011-03-04 12:13:34 ERROR: Cannot open file"
         DATE_LEVEL_MESSAGE,

         --  The <b>full</b> layout with everything (the default).
         --  Ex: "2011-03-04 12:13:34 ERROR - my.application - Cannot open file"
         FULL);

   --  ------------------------------
   --  Log appender
   --  ------------------------------
   type Appender is abstract new Ada.Finalization.Limited_Controlled with private;

   type Appender_Access is access all Appender'Class;

   --  Get the log level that triggers display of the log events
   function Get_Level (Self : in Appender) return Level_Type;

   --  Set the log level.
   procedure Set_Level (Self       : in out Appender;
                        Name       : in String;
                        Properties : in Util.Properties.Manager;
                        Level      : in Level_Type);

   --  Set the log layout format.
   procedure Set_Layout (Self       : in out Appender;
                         Name       : in String;
                         Properties : in Util.Properties.Manager;
                         Layout     : in Layout_Type);

   --  Format the event into a string
   function Format (Self  : in Appender;
                    Event : in Log_Event) return String;

   --  Append a log event to the appender.  Depending on the log level
   --  defined on the appender, the event can be taken into account or
   --  ignored.
   procedure Append (Self  : in out Appender;
                     Event : in Log_Event) is abstract;

   --  Flush the log events.
   procedure Flush (Self   : in out Appender) is abstract;

   --  ------------------------------
   --  File appender
   --  ------------------------------
   --  Write log events to a file
   type File_Appender is new Appender with private;
   type File_Appender_Access is access all File_Appender'Class;

   overriding
   procedure Append (Self  : in out File_Appender;
                     Event : in Log_Event);

   --  Set the file where the appender will write the logs
   procedure Set_File (Self : in out File_Appender;
                       Path : in String);

   --  Flush the log events.
   overriding
   procedure Flush (Self   : in out File_Appender);

   --  Flush and close the file.
   overriding
   procedure Finalize (Self : in out File_Appender);

   --  Create a file appender and configure it according to the properties
   function Create_File_Appender (Name       : in String;
                                  Properties : in Util.Properties.Manager;
                                  Default    : in Level_Type)
     return Appender_Access;

   --  ------------------------------
   --  Console appender
   --  ------------------------------
   --  Write log events to the console
   type Console_Appender is new Appender with private;
   type Console_Appender_Access is access all Console_Appender'Class;

   overriding
   procedure Append (Self  : in out Console_Appender;
                     Event : in Log_Event);

   --  Flush the log events.
   overriding
   procedure Flush (Self   : in out Console_Appender);

   --  Create a console appender and configure it according to the properties
   function Create_Console_Appender (Name       : in String;
                                     Properties : in Util.Properties.Manager;
                                     Default    : in Level_Type)
                                     return Appender_Access;

   --  ------------------------------
   --  List appender
   --  ------------------------------
   --  Write log events to a list of appenders
   type List_Appender is new Appender with private;
   type List_Appender_Access is access all List_Appender'Class;

   --  Max number of appenders that can be added to the list.
   --  In most cases, 2 or 3 appenders will be used.
   MAX_APPENDERS : constant Natural := 10;

   overriding
   procedure Append (Self  : in out List_Appender;
                     Event : in Log_Event);

   --  Flush the log events.
   overriding
   procedure Flush (Self   : in out List_Appender);

   --  Add the appender to the list.
   procedure Add_Appender (Self   : in out List_Appender;
                           Object : in Appender_Access);

   --  Create a list appender and configure it according to the properties
   function Create_List_Appender return List_Appender_Access;

private

   type Appender is abstract new Ada.Finalization.Limited_Controlled with record
      Level    : Level_Type := INFO_LEVEL;
      Layout   : Layout_Type := FULL;
   end record;

   type File_Appender is new Appender with record
      Output : Ada.Text_IO.File_Type;
   end record;

   type Appender_Array_Access is array (1 .. MAX_APPENDERS) of Appender_Access;

   type List_Appender is new Appender with record
      Appenders : Appender_Array_Access;
      Count     : Natural := 0;
   end record;

   type Console_Appender is new Appender with null record;

end Util.Log.Appenders;
