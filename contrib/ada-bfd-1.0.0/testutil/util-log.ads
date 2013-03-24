-----------------------------------------------------------------------
--  Logs -- Utility Log Package
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

--  The <b>Util.Log</b> package provides a simple logging framework inspired
--  from the Java Log4j library.
package Util.Log is

   pragma Preelaborate;

   subtype Level_Type is Natural;

   FATAL_LEVEL : constant Level_Type := 0;
   ERROR_LEVEL : constant Level_Type := 5;
   WARN_LEVEL  : constant Level_Type := 7;
   INFO_LEVEL  : constant Level_Type := 10;
   DEBUG_LEVEL : constant Level_Type := 20;

   --  Get the log level name.
   function Get_Level_Name (Level : Level_Type) return String;

   --  Get the log level from the property value
   function Get_Level (Value   : in String;
                       Default : in Level_Type := INFO_LEVEL) return Level_Type;

end Util.Log;
