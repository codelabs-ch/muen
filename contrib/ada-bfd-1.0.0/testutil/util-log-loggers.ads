-----------------------------------------------------------------------
--  Logs -- Utility Log Package
--  Copyright (C) 2006, 2008, 2009, 2011 Free Software Foundation, Inc.
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
with Ada.Exceptions;
with Ada.Strings.Unbounded;

with Util.Log.Appenders;
with Util.Properties;
with Ada.Finalization;
package Util.Log.Loggers is

   use Ada.Exceptions;
   use Ada.Strings.Unbounded;

   --  The logger identifies and configures the log produced
   --  by a component that uses it.  The logger has a name
   --  which can be printed in the log outputs.  The logger instance
   --  contains a log level which can be used to control the level of
   --  logs.
   type Logger is tagged limited private;
   type Logger_Access is access constant Logger;

   --  Create a logger with the given name.
   function Create (Name : in String) return Logger;

   --  Create a logger with the given name and use the specified level.
   function Create (Name  : in String;
                    Level : in Level_Type) return Logger;

   --  Initialize the logger and create a logger with the given name.
   function Create (Name   : in String;
                    Config : in String) return Logger;

   --  Change the log level
   procedure Set_Level (Log   : in out Logger;
                        Level : in Level_Type);

   --  Get the log level.
   function Get_Level (Log : in Logger) return Level_Type;

   --  Get the log level name.
   function Get_Level_Name (Log : in Logger) return String;

   procedure Print (Log     : in Logger;
                    Level   : in Level_Type;
                    Message : in String;
                    Arg1    : in String := "";
                    Arg2    : in String := "";
                    Arg3    : in String := "";
                    Arg4    : in String := "");

   procedure Debug (Log     : in Logger'Class;
                    Message : in String;
                    Arg1    : in String := "";
                    Arg2    : in String := "";
                    Arg3    : in String := "");

   procedure Debug (Log     : in Logger'Class;
                    Message : in String;
                    Arg1    : in Unbounded_String;
                    Arg2    : in String := "";
                    Arg3    : in String := "");

   procedure Debug (Log     : in Logger'Class;
                    Message : in String;
                    Arg1    : in Unbounded_String;
                    Arg2    : in Unbounded_String;
                    Arg3    : in String := "");

   procedure Info (Log     : in Logger'Class;
                   Message : in String;
                   Arg1    : in String := "";
                   Arg2    : in String := "";
                   Arg3    : in String := "");

   procedure Info (Log     : in Logger'Class;
                   Message : in String;
                   Arg1    : in Unbounded_String;
                   Arg2    : in String := "";
                   Arg3    : in String := "");

   procedure Warn (Log     : in Logger'Class;
                   Message : in String;
                   Arg1    : in String := "";
                   Arg2    : in String := "";
                   Arg3    : in String := "");

   procedure Error (Log     : in Logger'Class;
                    Message : in String;
                    Arg1    : in String := "";
                    Arg2    : in String := "";
                    Arg3    : in String := "");

   procedure Error (Log     : in Logger'Class;
                    Message : in String;
                    E       : in Exception_Occurrence;
                    Trace   : in Boolean := False);

   --  Set the appender that will handle the log events
   procedure Set_Appender (Log      : in out Logger'Class;
                           Appender : in Util.Log.Appenders.Appender_Access);

   --  Initialize the log environment with the property file.
   procedure Initialize (Name : in String);

   --  Initialize the log environment with the properties.
   procedure Initialize (Properties : in Util.Properties.Manager);

   type Logger_Info (<>) is limited private;

   --  Get the logger name.
   function Get_Logger_Name (Log : in Logger_Info) return String;

   --  Return a printable traceback that correspond to the exception.
   function Traceback (E : in Exception_Occurrence) return String;

private

   type Logger_Info_Access is access all Logger_Info;

   type Logger_Info (Len : Positive) is record
      Next_Logger : Logger_Info_Access;
      Prev_Logger : Logger_Info_Access;
      Level       : Level_Type := INFO_LEVEL;
      Appender    : Util.Log.Appenders.Appender_Access;
      Name        : String (1 .. Len);
   end record;

   type Logger is new Ada.Finalization.Limited_Controlled with record
      Instance : Logger_Info_Access;
   end record;

   --  Finalize the logger and flush the associated appender
   overriding
   procedure Finalize (Log : in out Logger);

end Util.Log.Loggers;
