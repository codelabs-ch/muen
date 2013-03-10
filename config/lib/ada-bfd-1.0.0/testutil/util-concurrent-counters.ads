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

--  The <b>Counters</b> package defines the <b>Counter</b> type which provides
--  atomic increment and decrement operations.  It is intended to be used to
--  implement reference counting in a multi-threaded environment.
--
--    type Ref is limited record
--        Cnt  : Counter;
--        Data : ...;
--    end record;
--
--    Object  : access Ref;
--    Is_Last : Boolean;
--  begin
--    Decrement (Object.Cnt, Is_Last);  -- Multi-task safe operation
--    if Is_Last then
--        Free (Object);
--    end if;
--
package Util.Concurrent.Counters is

   pragma Preelaborate;

   --  ------------------------------
   --  Atomic Counter
   --  ------------------------------
   --  The atomic <b>Counter</b> implements a simple counter that can be
   --  incremented or decremented atomically.
   type Counter is limited private;
   type Counter_Access is access all Counter;

   --  Increment the counter atomically.
   procedure Increment (C : in out Counter);

   --  Increment the counter atomically and return the value before increment.
   procedure Increment (C     : in out Counter;
                        Value : out Integer);

   --  Decrement the counter atomically.
   procedure Decrement (C : in out Counter);

   --  Decrement the counter atomically and return a status.
   procedure Decrement (C : in out Counter;
                        Is_Zero : out Boolean);

   --  Get the counter value
   function Value (C : in Counter) return Integer;

   --  Get the counter initialized to 1
   function ONE return Counter;

private

   --  This is the Ada05 portable implementation.  It has the following drawbacks:
   --  o The counter must be a limited type
   --  o The size of the target object is 10 times larger that the counter.
   --  o Increment and Decrement operations are 5 times slower.
   protected type Cnt is

      procedure Increment;

      procedure Increment (Value : out Integer);

      procedure Decrement (Is_Zero : out Boolean);

      function Get return Natural;

   private
      N : Integer := 0;
   end Cnt;

   type Counter is limited record
      Value : Cnt;
   end record;

end Util.Concurrent.Counters;
