--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.XML_Utils;

package body Expanders.Channels
is

   -------------------------------------------------------------------------

   procedure Add_Physical_Memory (Data : in out Muxml.XML_Data_Type)
   is
      Channels : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/channels/channel");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Channels) - 1 loop
         declare
            Channel_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Channels,
                 Index => I);
            Channel_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Channel_Node,
                 Name => "name");
            Channel_Size : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Channel_Node,
                 Name => "size");
         begin
            Mulog.Log (Msg => "Adding physical memory region with size "
                       & Channel_Size & " for channel '" & Channel_Name & "'");
            Mutools.XML_Utils.Add_Memory_Region
              (Policy       => Data,
               Name         => Channel_Name,
               Address      => "",
               Size         => Channel_Size,
               Caching      => "WB",
               Alignment    => "16#1000#",
               Memory_Type  => "subject_channel",
               Fill_Pattern => "16#00#");
         end;
      end loop;
   end Add_Physical_Memory;

   -------------------------------------------------------------------------

   procedure Remove_Global_Channels (Data : in out Muxml.XML_Data_Type)
   is
      use type DOM.Core.Node;

      System_Node : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Data.Doc,
           XPath => "/system");
      Channels_Node : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Data.Doc,
           XPath => "/system/channels");
   begin
      if Channels_Node /= null then
         Muxml.Utils.Remove_Child
           (Node       => System_Node,
            Child_Name => "channels");
      end if;
   end Remove_Global_Channels;

end Expanders.Channels;
