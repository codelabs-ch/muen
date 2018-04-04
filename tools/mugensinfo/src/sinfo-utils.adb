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

with Interfaces;

with DOM.Core.Elements;

with Muxml.Utils;

package body Sinfo.Utils
is

   -------------------------------------------------------------------------

   procedure Append_Resource
     (Info     : in out Musinfo.Subject_Info_Type;
      Resource : Musinfo.Resource_Type)
   is
      use type Interfaces.Unsigned_16;
      use type Musinfo.Resource_Index_Type;
   begin
      if Info.Resource_Count < Interfaces.Unsigned_16
        (Musinfo.Resource_Index_Type'Last)
      then
         Info.Resource_Count := Info.Resource_Count + 1;
         Info.Resources (Musinfo.Resource_Index_Type (Info.Resource_Count))
           := Resource;
         return;
      end if;

      raise Sinfo_Full with "Unable to insert " & Resource.Kind'Img
        & " resource - sinfo full";
   end Append_Resource;

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
      Channel : constant Boolean
        := DOM.Core.Elements.Get_Attribute
          (Elem => Phys_Mem_Node,
           Name => "type") = "subject_channel";

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

      return (Content => Content,
              Address => Address,
              Size    => Size,
              Hash    => Hash,
              Flags   => (Writable   => Writable,
                          Executable => Executable,
                          Channel    => Channel,
                          Padding    => 0),
              Pattern => Pattern,
              Padding => 0);
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
