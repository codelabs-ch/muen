-----------------------------------------------------------------------
--  util-strings-tokenizers --  Split strings into tokens
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

with Util.Texts.Tokenizers;

--  The <b>Util.Strings.Tokenizers</b> package provides an instantiation of the text
--  tokenizer.  The package provides operations to easily separate a string into tokens.
package Util.Strings.Tokenizers is new Util.Texts.Tokenizers
  (Char     => Character,
   Input    => String,
   Index    => Util.Strings.Index);
