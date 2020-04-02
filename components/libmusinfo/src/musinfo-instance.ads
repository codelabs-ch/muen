--
--  Copyright (C) 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--    * Redistributions of source code must retain the above copyright notice,
--      this list of conditions and the following disclaimer.
--
--    * Redistributions in binary form must reproduce the above copyright
--      notice, this list of conditions and the following disclaimer in the
--      documentation and/or other materials provided with the distribution.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
--  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
--  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
--  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
--  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
--  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
--  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
--  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--  POSSIBILITY OF SUCH DAMAGE.
--

with System;

with Musinfo.Utils;

private with Muschedinfo;

package Musinfo.Instance
with
   Abstract_State => (State, (Scheduling_Info with External => Async_Writers)),
   Initializes    => State
is

   --  Returns True if the sinfo data is valid.
   function Is_Valid return Boolean
   with
      Global => (Input => State);

   --  Return subject name stored in subject info instance.
   function Subject_Name return Name_Type
   with
      Global => (Input => State),
      Pre    => Is_Valid;

   --  Return TSC tick rate in kHz.
   function TSC_Khz return TSC_Tick_Rate_Khz_Type
   with
      Global => (Input => State),
      Pre    => Is_Valid;

   --  Return current TSC schedule start value.
   function TSC_Schedule_Start return Interfaces.Unsigned_64
   with
      Global => (Input    => Scheduling_Info,
                 Proof_In => State),
      Pre    => Is_Valid,
      Volatile_Function;

   --  Return current TSC schedule end value.
   function TSC_Schedule_End return Interfaces.Unsigned_64
   with
      Global => (Input    => Scheduling_Info,
                 Proof_In => State),
      Pre    => Is_Valid,
      Volatile_Function;

   --  Return memory region with specified name. If no such memory region
   --  exists, Null_Memregion is returned.
   function Memory_By_Name (Name : Name_Type) return Memregion_Type
   with
      Global => (Input => State),
      Pre    => Is_Valid;

   --  Return memory region with specified hash and content type. If no such
   --  memory region exists, Null_Memregion is returned. If multiple regions
   --  with the same hash/content exist, the first occurrence is returned.
   function Memory_By_Hash
     (Hash    : Hash_Type;
      Content : Content_Type)
      return Memregion_Type
   with
      Global => (Input => State),
      Pre    => Is_Valid;

   --  Return memory region by memory kind. If no such memory region exists,
   --  Null_Memregion is returned.
   function Memory_By_Kind (Kind : Memory_Kind) return Memregion_Type
   with
      Global => (Input => State),
      Pre    => Is_Valid;

   --  Returns True if the given iterator belongs to this sinfo instance.
   function Belongs_To
     (Iter : Utils.Resource_Iterator_Type)
      return Boolean
   with
      Ghost;

   --  Create resource iterator for sinfo instance.
   function Create_Resource_Iterator
     return Utils.Resource_Iterator_Type
   with
      Global => (Input => State),
      Pre    => Is_Valid,
      Post   => Belongs_To (Iter => Create_Resource_Iterator'Result);

   --  Returns True if the iterator points to a valid resource of the sinfo
   --  instance.
   function Has_Element
     (Iter : Utils.Resource_Iterator_Type)
      return Boolean
   with
      Global => (Input => State),
      Pre    => Is_Valid and Belongs_To (Iter => Iter);

   --  Return element at current iterator position. If the iterator points to
   --  no valid element, Null_Resource is returned.
   function Element
     (Iter : Utils.Resource_Iterator_Type)
      return Resource_Type
   with
      Global => (Input => State),
      Pre    => Is_Valid and Belongs_To (Iter => Iter);

   --  Advance resource iterator to next position (if available).
   procedure Next (Iter : in out Utils.Resource_Iterator_Type)
   with
      Global  => (Proof_In => State),
      Depends => (Iter => Iter),
      Pre     => Is_Valid and Belongs_To (Iter => Iter),
      Post    => Belongs_To (Iter => Iter);

   --  Return device info for device with given SID. If no such device exists,
   --  Null_Device is returned.
   function Device_By_SID (SID : SID_Type) return Device_Type
   with
      Global => (Input => State),
      Pre    => Is_Valid;

private

   Subject_Info_Virtual_Addr : constant := 16#000e_0000_0000#;
   Subject_Info_Size         : constant := 16#8000#;

   Object : Subject_Info_Type
   with
      Import,
      Part_Of => State,
      Address => System'To_Address (Subject_Info_Virtual_Addr);

   Sched_Info : Muschedinfo.Scheduling_Info_Type
   with
      Import,
      Volatile,
      Async_Writers,
      Part_Of => Scheduling_Info,
      Address => System'To_Address
        (Subject_Info_Virtual_Addr + Subject_Info_Size);

   function Create_Resource_Iterator
     return Utils.Resource_Iterator_Type
   is (Utils.Create_Resource_Iterator (Container => Object));

   function Has_Element
     (Iter : Utils.Resource_Iterator_Type)
      return Boolean
   is (Utils.Has_Element (Container => Object,
                          Iter      => Iter));

   function Element
     (Iter : Utils.Resource_Iterator_Type)
      return Resource_Type
   is (Utils.Element (Container => Object,
                      Iter      => Iter));

   function Belongs_To
     (Iter : Utils.Resource_Iterator_Type)
      return Boolean
   is (Utils.Belongs_To (Container => Object,
                         Iter      => Iter));

end Musinfo.Instance;
