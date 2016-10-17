--
--  Copyright (C) 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with DOM.Core.Elements;

with Muxml.Utils;

package body Sinfo.Utils
is

   -------------------------------------------------------------------------

   procedure Append_Channel
     (Info       : in out Musinfo.Subject_Info_Type;
      Name       :        Musinfo.Name_Type;
      Memregion  :        Musinfo.Memregion_Type;
      Has_Event  :        Boolean;
      Has_Vector :        Boolean;
      Event      :        Musinfo.Event_Number_Range;
      Vector     :        Musinfo.Vector_Range)
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
     (Info        : in out Musinfo.Subject_Info_Type;
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
     (Info   : in out Musinfo.Subject_Info_Type;
      Name   :        Musinfo.Name_Type;
      Region :        Musinfo.Memregion_Type)
   is
   begin
      Info.Memregion_Count := Info.Memregion_Count + 1;
      Info.Memregions (Info.Memregion_Count) := Region;

      Info.Resource_Count := Info.Resource_Count + 1;
      Info.Resources (Info.Resource_Count)
        := Create_Resource
          (Name               => Name,
           Memregion_Index    => Info.Memregion_Count,
           Channel_Info_Index => Musinfo.No_Resource);
   end Append_Memregion;

   -------------------------------------------------------------------------

   function Create_Channel_Info
     (Has_Event  : Boolean;
      Has_Vector : Boolean;
      Event      : Musinfo.Event_Number_Range;
      Vector     : Musinfo.Vector_Range)
      return Musinfo.Channel_Info_Type
   is
   begin
      return Channel_Info : Musinfo.Channel_Info_Type
        := Musinfo.Null_Channel_Info
      do
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
      return Musinfo.Dev_Info_Type
   is
   begin
      return Dev_Info : Musinfo.Dev_Info_Type := Musinfo.Null_Dev_Info do
         Dev_Info.SID               := SID;
         Dev_Info.IRTE_Start        := IRTE_Start;
         Dev_Info.IRQ_Start         := IRQ_Start;
         Dev_Info.IR_Count          := IR_Count;
         Dev_Info.Flags.MSI_Capable := MSI_Capable;
      end return;
   end Create_Dev_Info;

   -------------------------------------------------------------------------

   function Create_Memregion
     (Content    : Musinfo.Content_Type;
      Address    : Interfaces.Unsigned_64;
      Size       : Interfaces.Unsigned_64;
      Hash       : Musinfo.Hash_Type    := Musinfo.No_Hash;
      Pattern    : Musinfo.Pattern_Type := Musinfo.No_Pattern;
      Writable   : Boolean;
      Executable : Boolean)
      return Musinfo.Memregion_Type
   is
   begin
      return Musinfo.Memregion_Type'
        (Content => Content,
         Address => Address,
         Size    => Size,
         Hash    => Hash,
         Flags   =>
           (Writable   => Writable,
            Executable => Executable,
            Padding    => 0),
         Pattern => Pattern,
         Padding => 0);
   end Create_Memregion;

   -------------------------------------------------------------------------

   function Create_Name (Str : String) return Musinfo.Name_Type
   is
      Name    : Musinfo.Name_Type := Musinfo.Null_Name;
      Cur_Idx : Positive          := Musinfo.Name_Index_Type'First;
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
     (Name               : Musinfo.Name_Type;
      Memregion_Index    : Musinfo.Resource_Count_Type;
      Channel_Info_Index : Musinfo.Resource_Count_Type)
      return Musinfo.Resource_Type
   is
   begin
      return Resource : Musinfo.Resource_Type := Musinfo.Null_Resource do
         Resource.Name             := Name;
         Resource.Memregion_Idx    := Memregion_Index;
         Resource.Channel_Info_Idx := Channel_Info_Index;
      end return;
   end Create_Resource;

   -------------------------------------------------------------------------

   function Get_Memory_Info
     (Virt_Mem_Node : DOM.Core.Node;
      Phys_Mem_Node : DOM.Core.Node)
      return Musinfo.Memregion_Type
   is
      use type DOM.Core.Node;

      Content : Musinfo.Content_Type;
      Address : constant Interfaces.Unsigned_64
        := Interfaces.Unsigned_64'Value
          (DOM.Core.Elements.Get_Attribute
             (Elem => Virt_Mem_Node,
              Name => "virtualAddress"));
      Size : constant Interfaces.Unsigned_64
        := Interfaces.Unsigned_64'Value
          (DOM.Core.Elements.Get_Attribute
             (Elem => Phys_Mem_Node,
              Name => "size"));
      Writable : constant Boolean
        := Boolean'Value
          (DOM.Core.Elements.Get_Attribute
             (Elem => Virt_Mem_Node,
              Name => "writable"));
      Executable : constant Boolean
        := Boolean'Value
          (DOM.Core.Elements.Get_Attribute
             (Elem => Virt_Mem_Node,
              Name => "executable"));

      Hash_Node : constant DOM.Core.Node
        := Muxml.Utils.Get_Element (Doc   => Phys_Mem_Node,
                                    XPath => "hash");
      Fill_Node : constant DOM.Core.Node
        := Muxml.Utils.Get_Element (Doc   => Phys_Mem_Node,
                                    XPath => "fill");
      File_Node : constant DOM.Core.Node
        := Muxml.Utils.Get_Element (Doc   => Phys_Mem_Node,
                                    XPath => "file");

      Pattern : Musinfo.Pattern_Type := Musinfo.No_Pattern;
      Hash    : Musinfo.Hash_Type    := Musinfo.No_Hash;
   begin
      if Hash_Node /= null then
         Hash := Utils.To_Hash
           (Hex => DOM.Core.Elements.Get_Attribute
              (Elem => Hash_Node,
               Name => "value"));
      end if;

      if Fill_Node /= null then
         Content := Musinfo.Content_Fill;
         Pattern := Musinfo.Pattern_Type'Value
           (DOM.Core.Elements.Get_Attribute
              (Elem => Fill_Node,
               Name => "pattern"));
      elsif File_Node /= null then
         Content := Musinfo.Content_File;
      else
         Content := Musinfo.Content_Uninitialized;
      end if;

      return M : Musinfo.Memregion_Type do
         M := Create_Memregion
           (Content    => Content,
            Address    => Address,
            Size       => Size,
            Hash       => Hash,
            Pattern    => Pattern,
            Writable   => Writable,
            Executable => Executable);
      end return;
   end Get_Memory_Info;

   -------------------------------------------------------------------------

   function To_Hash (Hex : String) return Musinfo.Hash_Type
   is
      Hash : Musinfo.Hash_Type;
      Idx  : Positive := 3;
   begin
      for B of Hash loop
         B := Interfaces.Unsigned_8'Value
           ("16#" & Hex (Hex'First + Idx .. Hex'First + Idx + 1) & "#");
         Idx := Idx + 2;
      end loop;

      return Hash;
   end To_Hash;

end Sinfo.Utils;
