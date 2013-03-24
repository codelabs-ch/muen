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
with Ada.Strings;
generic
   type Char is (<>);
   type Input is array (Positive range <>) of Char;
   with function Index (Item    : in Input;
                        Pattern : in Input;
                        From    : in Positive;
                        Going   : in Ada.Strings.Direction := Ada.Strings.Forward)
                        return Natural is <>;
package Util.Texts.Tokenizers is

   pragma Preelaborate;

   --  Iterate over the tokens of the <b>Content</b> input.  Each token is separated by
   --  a pattern represented by <b>Pattern</b>.  For each token, call the
   --  procedure <b>Process</b> with the token.  When <b>Going</b> is <b>Backward</b>,
   --  scan the input from the end.  Stop iterating over the tokens when the <b>Process</b>
   --  procedure returns True in <b>Done</b>.
   procedure Iterate_Tokens (Content   : in Input;
                             Pattern   : in Input;
                             Process   : access procedure (Token : in Input;
                                                           Done  : out Boolean);
                             Going     : in Ada.Strings.Direction := Ada.Strings.Forward);

end Util.Texts.Tokenizers;
