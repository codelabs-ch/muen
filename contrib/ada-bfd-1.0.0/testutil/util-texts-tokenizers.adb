-----------------------------------------------------------------------
--  util-tests-tokenizers -- Split texts into tokens
--  Copyright (C) 2012 Stephane Carrez
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

package body Util.Texts.Tokenizers is

   --  ------------------------------
   --  Iterate over the tokens of the <b>Content</b> input.  Each token is separated by
   --  a pattern represented by <b>Pattern</b>.  For each token, call the
   --  procedure <b>Process</b> with the token.  When <b>Going</b> is <b>Backward</b>,
   --  scan the input from the end.  Stop iterating over the tokens when the <b>Process</b>
   --  procedure returns True in <b>Done</b>.
   --  ------------------------------
   procedure Iterate_Tokens (Content   : in Input;
                             Pattern   : in Input;
                             Process   : access procedure (Token : in Input;
                                                           Done  : out Boolean);
                             Going     : in Ada.Strings.Direction := Ada.Strings.Forward) is
      use Ada.Strings;

      Sep_Pos : Natural;
      Pos     : Natural;
      Last    : constant Natural := Content'Last;
   begin
      case Going is
         when Forward =>
            Pos := Content'First;
            while Pos <= Last loop
               Sep_Pos := Index (Content, Pattern, Pos, Forward);
               if Sep_Pos = 0 then
                  Sep_Pos := Last;
               else
                  Sep_Pos := Sep_Pos - 1;
               end if;
               declare
                  Done : Boolean;
               begin
                  Process (Token => Content (Pos .. Sep_Pos), Done => Done);
                  exit when Done;
               end;
               Pos := Sep_Pos + 1 + Pattern'Length;
            end loop;

         when Backward =>
            Pos := Content'Last;
            while Pos >= Content'First loop
               Sep_Pos := Index (Content, Pattern, Pos, Backward);
               if Sep_Pos = 0 then
                  Sep_Pos := Content'First;
               else
                  Sep_Pos := Sep_Pos + 1;
               end if;
               declare
                  Done : Boolean;
               begin
                  Process (Token => Content (Sep_Pos .. Pos), Done => Done);
                  exit when Done or Sep_Pos = Content'First;
               end;
               Pos := Sep_Pos - 1 - Pattern'Length;
            end loop;

      end case;
   end Iterate_Tokens;

end Util.Texts.Tokenizers;
