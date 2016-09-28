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

package body Musinfo.Utils
is

   -------------------------------------------------------------------------

   procedure Append_Channel
     (Info       : in out Subject_Info_Type;
      Name       :        Name_Type;
      Memregion  :        Memregion_Type;
      Has_Event  :        Boolean;
      Has_Vector :        Boolean;
      Event      :        Event_Number_Range;
      Vector     :        Vector_Range)
   is
   begin
      Append_Memregion
        (Info   => Info,
         Name   => Name,
         Region => Memregion);

      Info.Channel_Info_Count := Info.Channel_Info_Count + 1;
      Info.Channels_Info (Info.Channel_Info_Count)
        := Create_Channel_Info
          (Has_Event  => Has_Event,
           Has_Vector => Has_Vector,
           Event      => Event,
           Vector     => Vector);

      Info.Resources (Info.Resource_Count).Channel_Info_Idx
        := Info.Channel_Info_Count;
   end Append_Channel;

   -------------------------------------------------------------------------

   procedure Append_Dev
     (Info        : in out Subject_Info_Type;
      SID         :        Interfaces.Unsigned_16;
      IRTE_Start  :        Interfaces.Unsigned_16;
      IRQ_Start   :        Interfaces.Unsigned_8;
      IR_Count    :        Interfaces.Unsigned_8;
      MSI_Capable :        Boolean)
   is
   begin
      Info.Dev_Info_Count := Info.Dev_Info_Count + 1;
      Info.Dev_Info (Info.Dev_Info_Count)
        := Create_Dev_Info
          (SID         => SID,
           IRTE_Start  => IRTE_Start,
           IRQ_Start   => IRQ_Start,
           IR_Count    => IR_Count,
           MSI_Capable => MSI_Capable);
   end Append_Dev;

   -------------------------------------------------------------------------

   procedure Append_Memregion
     (Info   : in out Subject_Info_Type;
      Name   :        Name_Type;
      Region :        Memregion_Type)
   is
   begin
      Info.Memregion_Count := Info.Memregion_Count + 1;
      Info.Memregions (Info.Memregion_Count) := Region;

      Info.Resource_Count := Info.Resource_Count + 1;
      Info.Resources (Info.Resource_Count)
        := Create_Resource
          (Name               => Name,
           Memregion_Index    => Info.Memregion_Count,
           Channel_Info_Index => No_Resource);
   end Append_Memregion;

   -------------------------------------------------------------------------

   function Create_Channel_Info
     (Has_Event  : Boolean;
      Has_Vector : Boolean;
      Event      : Event_Number_Range;
      Vector     : Vector_Range)
      return Channel_Info_Type
   is
   begin
      return Channel_Info : Channel_Info_Type := Null_Channel_Info do
         Channel_Info.Flags.Has_Event  := Has_Event;
         Channel_Info.Flags.Has_Vector := Has_Vector;
         Channel_Info.Event            := Event;
         Channel_Info.Vector           := Vector;
      end return;
   end Create_Channel_Info;

   -------------------------------------------------------------------------

   function Create_Dev_Info
     (SID         : Interfaces.Unsigned_16;
      IRTE_Start  : Interfaces.Unsigned_16;
      IRQ_Start   : Interfaces.Unsigned_8;
      IR_Count    : Interfaces.Unsigned_8;
      MSI_Capable : Boolean)
      return Dev_Info_Type
   is
   begin
      return Dev_Info : Dev_Info_Type := Null_Dev_Info do
         Dev_Info.SID               := SID;
         Dev_Info.IRTE_Start        := IRTE_Start;
         Dev_Info.IRQ_Start         := IRQ_Start;
         Dev_Info.IR_Count          := IR_Count;
         Dev_Info.Flags.MSI_Capable := MSI_Capable;
      end return;
   end Create_Dev_Info;

   -------------------------------------------------------------------------

   function Create_Memregion
     (Content    : Content_Type;
      Address    : Interfaces.Unsigned_64;
      Size       : Interfaces.Unsigned_64;
      Hash       : Hash_Type    := No_Hash;
      Pattern    : Pattern_Type := No_Pattern;
      Writable   : Boolean;
      Executable : Boolean)
      return Memregion_Type
   is
   begin
      return Memregion_Type'
        (Content => Content,
         Address => Address,
         Size    => Size,
         Hash    => Hash,
         Flags   =>
           (Writable   => Writable,
            Executable => Executable,
            Padding    => (others => 0)),
         Pattern => Pattern,
         Padding => (others => 0));
   end Create_Memregion;

   -------------------------------------------------------------------------

   function Create_Name (Str : String) return Name_Type
   is
      Name    : Name_Type := Null_Name;
      Cur_Idx : Positive  := Name_Index_Type'First;
   begin
      Name.Length := Str'Length;

      for Char of Str loop
         Name.Data (Cur_Idx) := Char;
         Cur_Idx             := Cur_Idx + 1;
      end loop;

      return Name;
   end Create_Name;

   -------------------------------------------------------------------------

   function Create_Resource
     (Name               : Name_Type;
      Memregion_Index    : Resource_Count_Type;
      Channel_Info_Index : Resource_Count_Type)
      return Resource_Type
   is
   begin
      return Resource : Resource_Type := Null_Resource do
         Resource.Name             := Name;
         Resource.Memregion_Idx    := Memregion_Index;
         Resource.Channel_Info_Idx := Channel_Info_Index;
      end return;
   end Create_Resource;

end Musinfo.Utils;
