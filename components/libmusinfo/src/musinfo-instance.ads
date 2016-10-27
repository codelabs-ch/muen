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

package Musinfo.Instance
with
   Abstract_State => State,
   Initializes    => State
is

   --  Returns True if the sinfo data is valid.
   function Is_Valid return Boolean;

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
      Global => (Input => State),
      Pre    => Is_Valid;

   --  Return current TSC schedule end value.
   function TSC_Schedule_End return Interfaces.Unsigned_64
   with
      Global => (Input => State),
      Pre    => Is_Valid;

   --  Return memory region with specified name. If no such memory region
   --  exists, Null_Memregion is returned.
   function Memory_By_Name (Name : String) return Memregion_Type
   with
      Global => (Input => State),
      Pre    => Is_Valid and Name'Length <= Name_Index_Type'Last;

   --  Return memory region with specified hash. If no such memory region
   --  exists, Null_Memregion is returned.
   function Memory_By_Hash (Hash : Hash_Type) return Memregion_Type
   with
      Global => (Input => State),
      Pre    => Is_Valid;

   --  Returns True if the given iterator belongs to us.
   function Belongs_To
     (Iter : Musinfo.Utils.Memory_Iterator_Type)
      return Boolean
   with
      Ghost;

   --  Create memory region iterator for sinfo instance. If the sinfo data
   --  contains memory resources, the iterator points to the first region
   --  available. Otherwise it points to No_Resource.
   function Create_Memory_Iterator return Musinfo.Utils.Memory_Iterator_Type
   with
      Pre  => Is_Valid,
      Post => Belongs_To (Iter => Create_Memory_Iterator'Result);

   --  Returns True if the iterator points to a valid resource in the sinfo
   --  instance.
   function Has_Element
     (Iter : Musinfo.Utils.Memory_Iterator_Type)
      return Boolean
   with
      Pre => Is_Valid and Belongs_To (Iter => Iter);

   --  Return element at current iterator position. If the iterator points to
   --  no valid element, Null_Named_Memregion is returned.
   function Element
     (Iter : Musinfo.Utils.Memory_Iterator_Type)
      return Musinfo.Utils.Named_Memregion_Type
   with
      Pre => Is_Valid and Belongs_To (Iter => Iter);

   --  Advance memory iterator to next position (if available).
   procedure Next (Iter : in out Musinfo.Utils.Memory_Iterator_Type)
   with
      Depends => (Iter =>+ State),
      Pre     => Is_Valid and Belongs_To (Iter => Iter);

private

   Subject_Info_Virtual_Addr : constant := 16#000e_0000_0000#;

   Object : Musinfo.Subject_Info_Type
   with
      Import,
      Part_Of => State,
      Address => System'To_Address (Subject_Info_Virtual_Addr);

   function Belongs_To
     (Iter : Musinfo.Utils.Memory_Iterator_Type)
      return Boolean
   is (Utils.Belongs_To (Container => Object,
                         Iter      => Iter));

   function Create_Memory_Iterator return Musinfo.Utils.Memory_Iterator_Type
   is (Utils.Create_Memory_Iterator (Container => Object));

   function Has_Element
     (Iter : Musinfo.Utils.Memory_Iterator_Type)
      return Boolean
   is (Utils.Has_Element (Container => Object,
                          Iter      => Iter));

   function Element
     (Iter : Musinfo.Utils.Memory_Iterator_Type)
      return Musinfo.Utils.Named_Memregion_Type
   is (Utils.Element (Container => Object,
                      Iter      => Iter));

end Musinfo.Instance;
