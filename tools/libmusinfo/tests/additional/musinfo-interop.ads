--
--  Copyright (C) 2014-2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014-2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

--  Template package for additional C interop tests.
package Musinfo.Interop
is

   --  Verify name type interoperability.
   procedure Name_To_C;

   --  Verify memregion type interoperability.
   procedure Memregion_To_C;

   --  Verify channel type interoperability.
   procedure Channel_To_C;

   --  Verify resource type interoperability.
   procedure Resource_To_C;

   --  Verify dev info interoperability.
   procedure Dev_Info_To_C;

   --  Verify subject info type interoperability.
   procedure Subject_Info_To_C;

   --  Verify name type equivalence.
   procedure Check_Name_Type;

   --  Verify memregion type equivalence.
   procedure Check_Memregion_Type;

   --  Verify channel type equivalence.
   procedure Check_Channel_Type;

   --  Verify resource type equivalence.
   procedure Check_Resource_Type;

   --  Verify subject info type equivalence.
   procedure Check_Subject_Info_Type;

end Musinfo.Interop;
