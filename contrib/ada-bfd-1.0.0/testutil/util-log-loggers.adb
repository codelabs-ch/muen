-----------------------------------------------------------------------
--  Logs -- Utility Log Package
--  Copyright (C) 2001, 2002, 2003, 2006, 2008, 2009, 2010, 2011, 2012 Stephane Carrez
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
with Ada.Containers;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Calendar;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Unchecked_Deallocation;
with Ada.IO_Exceptions;
with Util.Strings;
package body Util.Log.Loggers is

   use Util;
   use Ada.Text_IO;
   use Ada.Strings;
   use Ada.Strings.Fixed;
   use Log.Appenders;

   package Appender_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Log.Appenders.Appender_Access, Ada.Strings.Hash, "=");

   function Traceback (E : in Exception_Occurrence) return String is separate;

   --  The log manager controls the configuration of loggers.
   --  The log appenders are shared by loggers and they are created by
   --  the log manager when a logger is created.
   --
   protected type Log_Manager is

      --  Initialize the log environment with the property file.
      procedure Initialize (Name : in String);

      --  Initialize the log environment with the property file.
      procedure Initialize (Properties : in Util.Properties.Manager);

      --  Create and initialize the logger
      procedure Create (Name : in String;
                        Log  : out Logger_Info_Access);

      --  Remove the logger from the list
      procedure Remove (Log : in out Logger_Info_Access);

   private

      --  Initialize the logger by reading the configuration, setting its level
      --  and creating the appender
      procedure Initialize (Log : in out Logger_Info);

      --  Re-initializes the loggers after the configuration is changed.
      procedure Initialize_Again;

      --  Find the appender to be used for the given logger.
      --  Create the appender if necessary.
      procedure Find_Appender (Property : in String;
                               Appender : out Appender_Access);

      --  Obtain an appender given its name.  If the appender does not exist, it is created.
      procedure Build_Appender (Name     : in String;
                                Appender : out Appender_Access);

      procedure Delete_Appenders;

      Config           : Properties.Manager;
      Appenders        : Appender_Maps.Map;
      Default_Level    : Level_Type := INFO_LEVEL;
      Default_Appender : Log.Appenders.Appender_Access := null;
      First_Logger     : Logger_Info_Access := null;
   end Log_Manager;

   Manager : Log_Manager;

   function Get_Appender (Value : in String) return String;

   procedure Format (Result  : in out Unbounded_String;
                     Message : in String;
                     Arg1    : in String;
                     Arg2    : in String;
                     Arg3    : in String;
                     Arg4    : in String);

   --  Get the logger property associated with a given logger
   function Get_Logger_Property (Properties : in Util.Properties.Manager;
                                 Name       : in String) return String;

   --  ------------------------------
   --  Get the logger property associated with a given logger
   --  ------------------------------
   function Get_Logger_Property (Properties : in Util.Properties.Manager;
                                 Name       : in String) return String is
      Prop_Name : constant String := "log4j.logger." & Name;
      Pos       : Natural := Prop_Name'Last;
   begin
      while Pos > Prop_Name'First loop
         if Properties.Exists (Prop_Name (Prop_Name'First .. Pos)) then
            return Trim (Properties.Get (Prop_Name (Prop_Name'First .. Pos)), Both);
         end if;
         Pos := Util.Strings.Rindex (Prop_Name, '.', Pos);
         if Pos > 0 then
            Pos := Pos - 1;
         end if;
      end loop;
      return "";
   end Get_Logger_Property;

   --  ------------------------------
   --  Initialize the log environment with the property file.
   --  ------------------------------
   procedure Initialize (Name : in String) is
   begin
      Manager.Initialize (Name);
   end Initialize;

   --  ------------------------------
   --  Initialize the log environment with the properties.
   --  ------------------------------
   procedure Initialize (Properties : in Util.Properties.Manager) is
   begin
      Manager.Initialize (Properties);
   end Initialize;

   --  ------------------------------
   --  Get the logger name.
   --  ------------------------------
   function Get_Logger_Name (Log : in Logger_Info) return String is
   begin
      return Log.Name;
   end Get_Logger_Name;

   protected body Log_Manager is

      --  ------------------------------
      --  Initialize the log environment with the property file.
      --  ------------------------------
      procedure Initialize (Name : in String) is
         F : File_Type;
      begin
         Open (F, In_File, Name);
         Properties.Load_Properties (Config, F);
         Close (F);

         Initialize_Again;

      exception
         when Ada.IO_Exceptions.Name_Error =>
            declare
               Event : Util.Log.Appenders.Log_Event;
               Log   : aliased Logger_Info := Logger_Info '(Len    => 8,
                                                            Name   => "Util.Log",
                                                            others => <>);
            begin
               Event.Time    := Ada.Calendar.Clock;
               Event.Level   := WARN_LEVEL;
               Event.Logger  := Log'Unchecked_Access;
               Format (Event.Message, "Log configuration file {0} not found", Name, "", "", "");
               if Default_Appender = null then
                  Default_Appender := new Console_Appender;
               end if;
               Default_Appender.Append (Event);
            end;
      end Initialize;

      --  ------------------------------
      --  Re-initializes the loggers after the configuration is changed.
      --  ------------------------------
      procedure Initialize_Again is
         L : Logger_Info_Access := First_Logger;
      begin
         Delete_Appenders;

         --  Initialize the default category.
         if Config.Exists ("log4j.rootCategory") then
            declare
               Value : constant String := Config.Get ("log4j.rootCategory");
            begin
               Default_Level := Get_Level (Value, Default_Level);
               Find_Appender (Property => Value, Appender => Default_Appender);
            end;
         end if;
         if Default_Appender = null then
            Default_Appender := new Log.Appenders.Console_Appender;
         end if;
         Appenders.Insert (Key => "root", New_Item => Default_Appender);

         --  Re-initialize the existing loggers.  Note that there is no concurrency
         --  protection if a thread calls 'Initialize' while another thread is using
         --  an already initialized logger.
         while L /= null loop
            Initialize (L.all);
            L := L.Next_Logger;
         end loop;
      end Initialize_Again;

      --  ------------------------------
      --  Initialize the log environment with the properties.
      --  ------------------------------
      procedure Initialize (Properties : in Util.Properties.Manager) is
      begin
         Config.Copy (From => Properties, Prefix => "log4j.");
         Initialize_Again;
      end Initialize;

      --  ------------------------------
      --  Initialize the logger by reading the configuration, setting its level
      --  and creating the appender
      --  ------------------------------
      procedure Initialize (Log : in out Logger_Info) is
         Prop : constant String := Get_Logger_Property (Config, Log.Name);
      begin
         Log.Level := Get_Level (Prop, Default_Level);
         Find_Appender (Prop, Log.Appender);
      end Initialize;

      --  ------------------------------
      --  Create and initialize the logger
      --  ------------------------------
      procedure Create (Name : in String;
                        Log  : out Logger_Info_Access) is
      begin
         Log       := new Logger_Info (Len => Name'Length);
         Log.Name  := Name;
         Initialize (Log.all);

         Log.Next_Logger := First_Logger;
         Log.Prev_Logger := null;
         if First_Logger /= null then
            First_Logger.Prev_Logger := Log;
         end if;
         First_Logger := Log;
      end Create;

      --  ------------------------------
      --  Remove the logger from the list
      --  ------------------------------
      procedure Remove (Log : in out Logger_Info_Access) is
         procedure Free is new Ada.Unchecked_Deallocation (Object => Logger_Info,
                                                           Name   => Logger_Info_Access);
      begin
         --  Remove first logger
         if Log = First_Logger then
            First_Logger := First_Logger.Next_Logger;
            if First_Logger /= null then
               First_Logger.Prev_Logger := null;
            end if;

            --  Remove last logger
         elsif Log.Next_Logger = null then
            Log.Prev_Logger.Next_Logger := null;

         else
            Log.Next_Logger.Prev_Logger := Log.Prev_Logger;
            Log.Prev_Logger.Next_Logger := Log.Next_Logger;
         end if;
         Free (Log);
      end Remove;

      procedure Delete_Appenders is
         procedure Free is new Ada.Unchecked_Deallocation (Object => Appender'Class,
                                                           Name   => Appender_Access);

      begin
         while not Appenders.Is_Empty loop
            declare
               Iter : Appender_Maps.Cursor := Appenders.First;
               E    : Appender_Access := Appender_Maps.Element (Iter);
            begin
               Appenders.Delete (Iter);

               --  Remove each mapping that points to the same appender.
               Iter := Appenders.First;
               while Appender_Maps.Has_Element (Iter) loop
                  if Appender_Maps.Element (Iter) = E then
                     Appenders.Delete (Iter);
                     Iter := Appenders.First;
                  else
                     Appender_Maps.Next (Iter);
                  end if;
               end loop;

               Free (E);
            end;
         end loop;
         Default_Appender := null;
      end Delete_Appenders;

      --  ------------------------------
      --  Obtain an appender given its name.  If the appender does not exist, it is created.
      --  ------------------------------
      procedure Build_Appender (Name     : in String;
                                Appender : out Appender_Access) is
         use Appender_Maps;

         Pos : constant Appender_Maps.Cursor := Appenders.Find (Name);
      begin
         if Pos /= Appender_Maps.No_Element then
            Appender := Appender_Maps.Element (Pos);
            return;
         end if;
         declare
            Prop_Name     : constant String := "log4j.appender." & Name;
            Appender_Type : constant String := Config.Get (Prop_Name, "console");
         begin
            if Index (Appender_Type, "File") > 0 then
               Appender := Create_File_Appender (Prop_Name, Config, Default_Level);
            else
               Appender := Create_Console_Appender (Prop_Name, Config, Default_Level);
            end if;
            Appenders.Insert (Name, Appender);
         end;
      end Build_Appender;

      --  ------------------------------
      --  Find an appender given the property value
      --  ------------------------------
      procedure Find_Appender (Property : in String;
                               Appender : out Appender_Access) is
         use Appender_Maps;

         Appender_Name  : constant String := Get_Appender (Property);
         Pos            : constant Appender_Maps.Cursor := Appenders.Find (Appender_Name);
      begin
         if Pos /= Appender_Maps.No_Element then
            Appender := Appender_Maps.Element (Pos);
            return;
         end if;

         declare
            N        : Natural := Index (Appender_Name, ",");
            Last_Pos : Natural := Appender_Name'First;
            List     : List_Appender_Access;
            A        : Appender_Access;
         begin
            --  The appender configuration refers to a list of appenders.
            --  Example:  DEBUG, out1, console
            if N > 0 then
               List := Create_List_Appender;
               loop
                  Build_Appender (Trim (Appender_Name (Last_Pos .. N - 1), Both), A);
                  List.Add_Appender (A);
                  exit when N > Appender_Name'Last;
                  Last_Pos := N + 1;
                  N := Ada.Strings.Fixed.Index (Appender_Name, ",", Last_Pos);
                  if N = 0 then
                     N := Appender_Name'Last + 1;
                  end if;
               end loop;
               Appender := List.all'Access;
               Appenders.Insert (Appender_Name, Appender);
            else
               Build_Appender (Appender_Name, Appender);
            end if;
         end;
      end Find_Appender;

   end Log_Manager;

   --  ------------------------------
   --  Get the log appender name from the property value
   --  ------------------------------
   function Get_Appender (Value : in String) return String is
      Pos : constant Natural := Index (Value, ",");
   begin
      if Pos <= Value'First or Pos >= Value'Last then
         return "root";
      else
         return Trim (Value (Pos + 1 .. Value'Last), Both);
      end if;
   end Get_Appender;

   --  ------------------------------
   --  Create a logger with the given name.
   --  ------------------------------
   function Create (Name : in String) return Logger is
      Log : Logger_Info_Access;
   begin
      Manager.Create (Name, Log);
--        declare
--           Event : Util.Log.Appenders.Log_Event;
--        begin
--           Event.Time    := Ada.Calendar.Clock;
--           Event.Level   := DEBUG_LEVEL;
--           Event.Message := Format ("Create logger {0}", Name, "", "");
--           Event.Logger  := To_Unbounded_String (Name);
--           Log.Appender.Append (Event);
--        end;
      return Logger '(Ada.Finalization.Limited_Controlled with
                      Instance => Log);
   end Create;

   --  Create a logger with the given name and use the specified level.
   function Create (Name  : in String;
                    Level : in Level_Type) return Logger is
      Log : Logger_Info_Access;
   begin
      Manager.Create (Name, Log);
      Log.Level := Level;
      return Logger '(Ada.Finalization.Limited_Controlled with
                      Instance => Log);
   end Create;

   --  ------------------------------
   --  Initialize the logger and create a logger with the given name.
   --  ------------------------------
   function Create (Name   : in String;
                    Config : in String) return Logger is
   begin
      Initialize (Config);
      return Create (Name);
   end Create;

   --  ------------------------------
   --  Change the log level
   --  ------------------------------
   procedure Set_Level (Log   : in out Logger;
                        Level : in Level_Type) is
   begin
      Log.Instance.Level := Level;
   end Set_Level;

   --  ------------------------------
   --  Get the log level.
   --  ------------------------------
   function Get_Level (Log : in Logger) return Level_Type is
   begin
      return Log.Instance.Level;
   end Get_Level;

   --  ------------------------------
   --  Get the log level name.
   --  ------------------------------
   function Get_Level_Name (Log : in Logger) return String is
   begin
      return Get_Level_Name (Log.Instance.Level);
   end Get_Level_Name;

   --  ------------------------------
   --  Format the message with the arguments
   --  ------------------------------
   procedure Format (Result  : in out Unbounded_String;
                     Message : in String;
                     Arg1    : in String;
                     Arg2    : in String;
                     Arg3    : in String;
                     Arg4    : in String) is
      Pos    : Natural := Message'First;
      C      : Character;
   begin
      --  Replace {N} with arg1, arg2, arg3 or ?
      while Pos <= Message'Last loop
         C := Message (Pos);
         if C = '{' then
            if Pos + 2 > Message'Last or else Message (Pos + 2) /= '}' then
               Append (Result, C);
            else
               C := Message (Pos + 1);
               Pos := Pos + 2;
               case C is
               when '0' =>
                  Append (Result, Arg1);

               when '1' =>
                  Append (Result, Arg2);

               when '2' =>
                  Append (Result, Arg3);

               when '3' =>
                  Append (Result, Arg4);

               when others =>
                  Append (Result, "?");
               end case;
            end if;

         else
            Append (Result, C);
         end if;
         Pos := Pos + 1;
      end loop;
   end Format;

   procedure Print (Log     : in Logger;
                    Level   : in Level_Type;
                    Message : in String;
                    Arg1    : in String := "";
                    Arg2    : in String := "";
                    Arg3    : in String := "";
                    Arg4    : in String := "") is
   begin
      if Log.Instance /= null and then Log.Instance.Level >= Level then
         declare
            Event : Util.Log.Appenders.Log_Event;
         begin
            Event.Time    := Ada.Calendar.Clock;
            Event.Level   := Level;
            Event.Logger  := Log.Instance;
            Format (Event.Message, Message, Arg1, Arg2, Arg3, Arg4);
            Log.Instance.Appender.Append (Event);
         end;
      end if;
   end Print;

   procedure Debug (Log     : in Logger'Class;
                    Message : in String;
                    Arg1    : in String := "";
                    Arg2    : in String := "";
                    Arg3    : in String := "") is
   begin
      Print (Log, DEBUG_LEVEL, Message, Arg1, Arg2, Arg3);
   end Debug;

   procedure Debug (Log     : in Logger'Class;
                    Message : in String;
                    Arg1    : in Unbounded_String;
                    Arg2    : in String := "";
                    Arg3    : in String := "") is
   begin
      if Log.Instance /= null and then Log.Instance.Level >= DEBUG_LEVEL then
         Print (Log, DEBUG_LEVEL, Message, To_String (Arg1), Arg2, Arg3);
      end if;
   end Debug;

   procedure Debug (Log     : in Logger'Class;
                    Message : in String;
                    Arg1    : in Unbounded_String;
                    Arg2    : in Unbounded_String;
                    Arg3    : in String := "") is
   begin
      if Log.Instance /= null and then Log.Instance.Level >= DEBUG_LEVEL then
         Print (Log, DEBUG_LEVEL, Message, To_String (Arg1), To_String (Arg2), Arg3);
      end if;
   end Debug;

   procedure Info (Log     : in Logger'Class;
                   Message : in String;
                   Arg1    : in String := "";
                   Arg2    : in String := "";
                   Arg3    : in String := "") is
   begin
      Print (Log, INFO_LEVEL, Message, Arg1, Arg2, Arg3);
   end Info;

   procedure Info (Log     : in Logger'Class;
                   Message : in String;
                   Arg1    : in Unbounded_String;
                   Arg2    : in String := "";
                   Arg3    : in String := "") is
   begin
      if Log.Instance /= null and then Log.Instance.Level >= INFO_LEVEL then
         Print (Log, INFO_LEVEL, Message, To_String (Arg1), Arg2, Arg3);
      end if;
   end Info;

   procedure Warn (Log     : in Logger'Class;
                   Message : in String;
                   Arg1    : in String := "";
                   Arg2    : in String := "";
                   Arg3    : in String := "") is
   begin
      if Log.Instance /= null and then Log.Instance.Level >= WARN_LEVEL then
         Print (Log, WARN_LEVEL, Message, Arg1, Arg2, Arg3);
      end if;
   end Warn;

   procedure Error (Log     : in Logger'Class;
                    Message : in String;
                    Arg1    : in String := "";
                    Arg2    : in String := "";
                    Arg3    : in String := "") is
   begin
      Print (Log, ERROR_LEVEL, Message, Arg1, Arg2, Arg3);
   end Error;

   procedure Error (Log     : in Logger'Class;
                    Message : in String;
                    E       : in Exception_Occurrence;
                    Trace   : in Boolean := False) is
   begin

      if Trace then
         Print (Log, ERROR_LEVEL,
                "{0}: Exception {1}: {2} at {3}",
                Message,
                Exception_Name (E),
                Exception_Message (E),
                Traceback (E));
      else
         Print (Log, ERROR_LEVEL,
                "{0}: Exception {1}: {2}",
                Message,
                Exception_Name (E),
                Exception_Message (E));
      end if;
   end Error;

   --  ------------------------------
   --  Set the appender that will handle the log events
   --  ------------------------------
   procedure Set_Appender (Log      : in out Logger'Class;
                           Appender : in Util.Log.Appenders.Appender_Access) is
   begin
      Log.Instance.Appender := Appender;
   end Set_Appender;

   --  ------------------------------
   --  Finalize the logger and flush the associated appender
   --  ------------------------------
   procedure Finalize (Log : in out Logger) is
   begin
      if Log.Instance.Appender /= null then
         Log.Instance.Appender.Flush;
      end if;
      Manager.Remove (Log.Instance);
   end Finalize;

end Util.Log.Loggers;
