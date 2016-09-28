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

package Musinfo.Utils
is

   --  Create name from given string.
   function Create_Name (Str : String) return Name_Type
     with
       Pre => Str'Length in Name_Index_Type;

   --  Create memory region with given parameters.
   function Create_Memregion
     (Content    : Content_Type;
      Address    : Interfaces.Unsigned_64;
      Size       : Interfaces.Unsigned_64;
      Hash       : Hash_Type    := No_Hash;
      Pattern    : Pattern_Type := No_Pattern;
      Writable   : Boolean;
      Executable : Boolean)
      return Memregion_Type;

   --  Create channel information with given parameters.
   function Create_Channel_Info
     (Has_Event  : Boolean;
      Has_Vector : Boolean;
      Event      : Event_Number_Range;
      Vector     : Vector_Range)
      return Channel_Info_Type;

   --  Create resource with given parameters.
   function Create_Resource
     (Name               : Name_Type;
      Memregion_Index    : Resource_Count_Type;
      Channel_Info_Index : Resource_Count_Type)
      return Resource_Type;

   --  Create dev info with given parameters.
   function Create_Dev_Info
     (SID         : Interfaces.Unsigned_16;
      IRTE_Start  : Interfaces.Unsigned_16;
      IRQ_Start   : Interfaces.Unsigned_8;
      IR_Count    : Interfaces.Unsigned_8;
      MSI_Capable : Boolean)
      return Dev_Info_Type;

   --  Append memory region to given subject info.
   procedure Append_Memregion
     (Info   : in out Subject_Info_Type;
      Name   :        Name_Type;
      Region :        Memregion_Type)
     with
       Pre =>
         Info.Resource_Count < Resource_Count_Type'Last and
         Info.Memregion_Count < Resource_Count_Type'Last;

   --  Append channel with specified parameters to given subject info.
   procedure Append_Channel
     (Info       : in out Subject_Info_Type;
      Name       :        Name_Type;
      Memregion  :        Memregion_Type;
      Has_Event  :        Boolean;
      Has_Vector :        Boolean;
      Event      :        Event_Number_Range;
      Vector     :        Vector_Range)
     with
       Pre =>
         not Memregion.Flags.Executable and
         Info.Resource_Count < Resource_Count_Type'Last and
         Info.Memregion_Count < Resource_Count_Type'Last and
         Info.Channel_Info_Count < Resource_Count_Type'Last;

   --  Append device data to given subject info record.
   procedure Append_Dev
     (Info        : in out Subject_Info_Type;
      SID         :        Interfaces.Unsigned_16;
      IRTE_Start  :        Interfaces.Unsigned_16;
      IRQ_Start   :        Interfaces.Unsigned_8;
      IR_Count    :        Interfaces.Unsigned_8;
      MSI_Capable :        Boolean)
     with
       Pre =>
         Info.Dev_Info_Count < Resource_Count_Type'Last;

end Musinfo.Utils;
