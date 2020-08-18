--
--  Copyright (C) 2020  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2020  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Interfaces;

with Musinfo.Instance;

package Init.Memory
is

   --  Zeroize all writable memory regions present in subject info.
   procedure Clear_Writable
   with
      Pre => Musinfo.Instance.Is_Valid;

   --  Returns the base addresses of the component text and stack memory
   --  regions. Success is set to True if both addresses could be determined.
   procedure Get_Base_Addresses
     (Text_Base  : out Interfaces.Unsigned_64;
      Stack_Base : out Interfaces.Unsigned_64;
      Success    : out Boolean)
   with
      Pre => Musinfo.Instance.Is_Valid;

   --  Returns the base address of the stack memory region.
   function Get_Stack_Base return Interfaces.Unsigned_64
   with
      Pre => Musinfo.Instance.Is_Valid;

   --  Returns the base address of the component text memory region.
   function Get_Text_Base return Interfaces.Unsigned_64
   with
      Pre => Musinfo.Instance.Is_Valid;

   --  Setup all writable memory regions present in subject info with their
   --  initial content.
   procedure Setup_Writable (Success : out Boolean)
   with
      Pre => Musinfo.Instance.Is_Valid;

   --  Check hashes of all memory regions present in subject info against
   --  current in-memory content hashes.
   procedure Check_Hashes (Success : out Boolean)
   with
      Pre => Musinfo.Instance.Is_Valid;

end Init.Memory;
