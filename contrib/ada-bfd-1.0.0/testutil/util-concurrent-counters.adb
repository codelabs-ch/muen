-----------------------------------------------------------------------
--  Util.Concurrent -- Concurrent Counters
--  Copyright (C) 2009, 2010 Stephane Carrez
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

--  This implementation of atomic counters is the portable Ada05 implementation.
--  It uses a protected type to implement the increment/decrement operations,
--  thus providing the thread-safe capability.
package body Util.Concurrent.Counters is

   function ONE return Counter is
   begin
      return C : Counter do
         C.Value.Increment;
      end return;
   end ONE;

   --  ------------------------------
   --  Increment the counter atomically.
   --  ------------------------------
   procedure Increment (C : in out Counter) is
   begin
      C.Value.Increment;
   end Increment;

   --  ------------------------------
   --  Increment the counter atomically and return the value before increment.
   --  ------------------------------
   procedure Increment (C     : in out Counter;
                        Value : out Integer) is
   begin
      C.Value.Increment (Value);
   end Increment;

   --  ------------------------------
   --  Decrement the counter atomically.
   --  ------------------------------
   procedure Decrement (C : in out Counter) is
      Is_Zero : Boolean;
   begin
      C.Value.Decrement (Is_Zero);
   end Decrement;

   --  ------------------------------
   --  Decrement the counter atomically and return a status.
   --  ------------------------------
   procedure Decrement (C : in out Counter;
                        Is_Zero : out Boolean) is
   begin
      C.Value.Decrement (Is_Zero);
   end Decrement;

   --  ------------------------------
   --  Get the counter value
   --  ------------------------------
   function Value (C : in Counter) return Integer is
   begin
      return C.Value.Get;
   end Value;

   protected body Cnt is

      procedure Increment is
      begin
         N := N + 1;
      end Increment;

      procedure Increment (Value : out Integer) is
      begin
         Value := N;
         N := N + 1;
      end Increment;

      procedure Decrement (Is_Zero : out Boolean) is
      begin
         N := N - 1;
         Is_Zero := N = 0;
      end Decrement;

      function Get return Natural is
      begin
         return N;
      end Get;

   end Cnt;

end Util.Concurrent.Counters;
