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

with System;

with Interfaces.C;

package C_Imports
is

   function C_Assert_Name
     (Name : System.Address)
      return Interfaces.C.int
     with
       Import     => True,
       Convention => C,
       Link_Name  => "assert_name";

   function C_Assert_Memregion
     (Memregion : System.Address)
      return Interfaces.C.int
     with
       Import     => True,
       Convention => C,
       Link_Name  => "assert_memregion";

   function C_Assert_Resource
     (Resource : System.Address)
      return Interfaces.C.int
     with
       Import     => True,
       Convention => C,
       Link_Name  => "assert_resource";

   function C_Assert_Device
     (Device : System.Address)
      return Interfaces.C.int
     with
       Import     => True,
       Convention => C,
       Link_Name  => "assert_device";

   function C_Assert_Subject_Info
     (Info : System.Address)
      return Interfaces.C.int
     with
       Import     => True,
       Convention => C,
       Link_Name  => "assert_subject_info";

   function C_Assert_Name_Type
     (Size          : Interfaces.C.int;
      Alignment     : Interfaces.C.int;
      Length_Offset : Interfaces.C.int;
      Data_Offset   : Interfaces.C.int)
      return Interfaces.C.int
     with
       Import     => True,
       Convention => C,
       Link_Name  => "assert_name_type";

   function C_Assert_Memregion_Type
     (Size           : Interfaces.C.int;
      Content_Offset : Interfaces.C.int;
      Address_Offset : Interfaces.C.int;
      Size_Offset    : Interfaces.C.int;
      Hash_Offset    : Interfaces.C.int;
      Flags_Offset   : Interfaces.C.int;
      Pattern_Offset : Interfaces.C.int)
      return Interfaces.C.int
     with
       Import     => True,
       Convention => C,
       Link_Name  => "assert_memregion_type";

   function C_Assert_Resource_Type
     (Size        : Interfaces.C.int;
      Alignment   : Interfaces.C.int;
      Name_Offset : Interfaces.C.int;
      Data_Offset : Interfaces.C.int)
      return Interfaces.C.int
     with
       Import     => True,
       Convention => C,
       Link_Name  => "assert_resource_type";

   function C_Assert_Device_Type
     (Size              : Interfaces.C.int;
      IRTE_Start_Offset : Interfaces.C.int;
      IRQ_Start_Offset  : Interfaces.C.int;
      IR_Count_Offset   : Interfaces.C.int;
      Flags_Offset      : Interfaces.C.int)
      return Interfaces.C.int
     with
       Import     => True,
       Convention => C,
       Link_Name  => "assert_device_type";

   function C_Assert_Subject_Info_Type
     (Size             : Interfaces.C.int;
      Alignment        : Interfaces.C.int;
      Magic_Offset     : Interfaces.C.int;
      TSC_Khz_Offset   : Interfaces.C.int;
      Name_Offset      : Interfaces.C.int;
      Res_Count_Offset : Interfaces.C.int;
      Resources_Offset : Interfaces.C.int)
      return Interfaces.C.int
     with
       Import     => True,
       Convention => C,
       Link_Name  => "assert_subject_info_type";

end C_Imports;
