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
with Mutools.Image;
with Mutools.Utils;

package body Memhashes.Utils
is

   -------------------------------------------------------------------------

   function To_Stream
     (Node      : DOM.Core.Node;
      Input_Dir : String := "")
      return Ada.Streams.Stream_Element_Array
   is
      type Content_Type is (File, Fill);

      use type Ada.Streams.Stream_Element_Offset;

      Mem_Size : constant Ada.Streams.Stream_Element_Offset
        := Ada.Streams.Stream_Element_Offset'Value
          (DOM.Core.Elements.Get_Attribute
             (Elem => Node,
              Name => "size"));
      Content_Node : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Node,
           XPath => "*[self::fill or self::file]");
      Content_Kind : constant Content_Type
        := Content_Type'Value
          (DOM.Core.Elements.Get_Tag_Name
             (Elem => Content_Node));
      Img : Mutools.Image.Image_Type (End_Address => Mem_Size - 1);
   begin
      case Content_Kind
      is
         when File =>
            declare
               use type Interfaces.Unsigned_64;

               Offset_Str : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Content_Node,
                    Name => "offset");
               Filename : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Content_Node,
                    Name => "filename");
               Offset : Interfaces.Unsigned_64 := 0;
               Added  : Interfaces.Unsigned_64;
            begin
               if Offset_Str /= "none" then
                  Offset := Interfaces.Unsigned_64'Value (Offset_Str);
               end if;

               Mutools.Image.Add_File
                 (Image   => Img,
                  Path    => Input_Dir & "/" & Filename,
                  Address => 0,
                  Size    => Interfaces.Unsigned_64 (Mem_Size),
                  Offset  => Offset,
                  Added   => Added);

               DOM.Core.Elements.Set_Attribute
                 (Elem  => Content_Node,
                  Name  => "size",
                  Value => Mutools.Utils.To_Hex (Number => Offset + Added));
            end;
         when Fill =>
            declare
               Pattern : constant Ada.Streams.Stream_Element
                 := Ada.Streams.Stream_Element'Value
                   (DOM.Core.Elements.Get_Attribute
                      (Elem => Content_Node,
                       Name => "pattern"));
            begin
               Mutools.Image.Add_Pattern
                 (Image   => Img,
                  Pattern => Pattern,
                  Size    => Interfaces.Unsigned_64 (Mem_Size),
                  Address => 0);
            end;
      end case;

      return Mutools.Image.Get_Buffer
        (Image   => Img,
         Address => 0,
         Size    => Interfaces.Unsigned_64 (Mem_Size));
   end To_Stream;

end Memhashes.Utils;
