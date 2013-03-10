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
with Ada.Strings;
with Ada.Strings.Fixed;

package body Util.Log is

   --  ------------------------------
   --  Get the log level name.
   --  ------------------------------
   function Get_Level_Name (Level : Level_Type) return String is
   begin
      if Level = FATAL_LEVEL then
         return "FATAL";
      elsif Level = ERROR_LEVEL then
         return "ERROR";
      elsif Level = WARN_LEVEL then
         return "WARN ";
      elsif Level = INFO_LEVEL then
         return "INFO ";
      elsif Level = DEBUG_LEVEL then
         return "DEBUG";
      else
         return Level_Type'Image (Level);
      end if;
   end Get_Level_Name;

   --  ------------------------------
   --  Get the log level from the property value
   --  ------------------------------
   function Get_Level (Value   : in String;
                       Default : in Level_Type := INFO_LEVEL) return Level_Type is
      use Ada.Strings;

      Val : constant String  := Fixed.Trim (Value, Both);
      Pos : constant Natural := Fixed.Index (Val, ",");
   begin
      if Pos > Val'First then
         return Get_Level (Val (Val'First .. Pos - 1), Default);
      elsif Val = "INFO" then
         return INFO_LEVEL;
      elsif Val = "DEBUG" then
         return DEBUG_LEVEL;
      elsif Val = "WARN" then
         return WARN_LEVEL;
      elsif Val = "ERROR" then
         return ERROR_LEVEL;
      elsif Val = "FATAL" then
         return FATAL_LEVEL;
      else
         return Default;
      end if;
   end Get_Level;

end Util.Log;
