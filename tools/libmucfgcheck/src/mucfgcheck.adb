--
--  Copyright (C) 2014, 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014, 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package body Mucfgcheck
is

   -------------------------------------------------------------------------

   procedure Check_Attribute
     (Nodes     : DOM.Core.Node_List;
      Node_Type : String;
      Attr      : String;
      Name_Attr : String;
      Test      : Test_Function_2;
      B         : Interfaces.Unsigned_64;
      Error_Msg : String)
   is
   begin
      Mulog.Log (Msg => "Checking attribute '" & Attr & "' of"
                 & DOM.Core.Nodes.Length (List => Nodes)'Img & " "
                 & Node_Type & " element(s)");

      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Node       : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Nodes,
                                      Index => I);
            Name       : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => Name_Attr);
            Attr_Str   : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Node,
               Name => Attr);
            Attr_Value : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value (Attr_Str);
         begin
            if not Test (A => Attr_Value, B => B) then
               raise Validation_Error with "Attribute '" & Attr & " => "
                 & Attr_Str & "' of '" & Name & "' " & Node_Type  & " element "
                 & Error_Msg;
            end if;
         end;
      end loop;
   end Check_Attribute;

   -------------------------------------------------------------------------

   procedure Check_Attribute
     (Nodes     : DOM.Core.Node_List;
      Node_Type : String;
      Attr      : String;
      Name_Attr : String;
      Test      : Test_Function_3;
      B         : Interfaces.Unsigned_64;
      C         : Interfaces.Unsigned_64;
      Error_Msg : String)
   is
   begin
      Mulog.Log (Msg => "Checking attribute '" & Attr & "' of"
                 & DOM.Core.Nodes.Length (List => Nodes)'Img & " "
                 & Node_Type & " element(s)");

      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Node       : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Nodes,
                                      Index => I);
            Name       : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Node,
                 Name => Name_Attr);
            Attr_Str   : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Node,
                 Name => Attr);
            Attr_Value : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value (Attr_Str);
         begin
            if not Test (A => Attr_Value, B => B, C => C) then
               raise Validation_Error with "Attribute '" & Attr & " => "
                 & Attr_Str & "' of '" & Name & "' " & Node_Type  & " element "
                 & Error_Msg;
            end if;
         end;
      end loop;
   end Check_Attribute;

   -------------------------------------------------------------------------

   procedure Check_Memory_Overlap
     (Nodes        : DOM.Core.Node_List;
      Region_Type  : String;
      Address_Attr : String;
      Name_Attr    : String := "name";
      Add_Msg      : String := "")
   is
      use Interfaces;

      --  Check if left and right memory region overlap.
      procedure Check_Overlap (Left, Right : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Check_Overlap (Left, Right : DOM.Core.Node)
      is
         Left_Name  : constant String      := DOM.Core.Elements.Get_Attribute
           (Elem => Left,
            Name => Name_Attr);
         Left_Addr  : constant Unsigned_64 := Unsigned_64'Value
           (DOM.Core.Elements.Get_Attribute
              (Elem => Left,
               Name => Address_Attr));
         Left_Size  : constant Unsigned_64 := Unsigned_64'Value
           (DOM.Core.Elements.Get_Attribute
              (Elem => Left,
               Name => "size"));
         Right_Name : constant String      := DOM.Core.Elements.Get_Attribute
           (Elem => Right,
            Name => Name_Attr);
         Right_Addr : constant Unsigned_64 := Unsigned_64'Value
           (DOM.Core.Elements.Get_Attribute
              (Elem => Right,
               Name => Address_Attr));
         Right_Size : constant Unsigned_64 := Unsigned_64'Value
           (DOM.Core.Elements.Get_Attribute
              (Elem => Right,
               Name => "size"));
      begin
         if (Left_Addr <= Right_Addr
             and then Left_Addr + Left_Size > Right_Addr)
           or
             (Right_Addr < Left_Addr
              and then Right_Addr + Right_Size > Left_Addr)
         then
            raise Validation_Error with "Overlap of " & Region_Type
              & " '" & Left_Name & "' and '" & Right_Name & "'"
              & Add_Msg;
         end if;
      end Check_Overlap;
   begin
      Mulog.Log (Msg => "Checking overlap of" & DOM.Core.Nodes.Length
                 (List => Nodes)'Img & " " & Region_Type & "(s)" & Add_Msg);

      Compare_All (Nodes      => Nodes,
                   Comparator => Check_Overlap'Access);
   end Check_Memory_Overlap;

   -------------------------------------------------------------------------

   procedure Compare_All
     (Nodes      : DOM.Core.Node_List;
      Comparator : not null access procedure (Left, Right : DOM.Core.Node))
   is
      Node_Count : constant Natural := DOM.Core.Nodes.Length (List => Nodes);
   begin
      if Node_Count < 2 then
         return;
      end if;

      for I in 0 .. Node_Count - 2 loop
         declare
            Left : constant DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => Nodes,
               Index => I);
         begin
            for J in I + 1 .. Node_Count - 1 loop
               declare
                  Right : constant DOM.Core.Node := DOM.Core.Nodes.Item
                    (List  => Nodes,
                     Index => J);
               begin
                  Comparator (Left  => Left,
                              Right => Right);
               end;
            end loop;
         end;
      end loop;
   end Compare_All;

   -------------------------------------------------------------------------

   procedure For_Each_Match
     (Source_Nodes : DOM.Core.Node_List;
      Ref_Nodes    : DOM.Core.Node_List;
      Log_Message  : String;
      Error        : not null access function
        (Node : DOM.Core.Node) return String;
      Match        : not null access function
        (Left, Right : DOM.Core.Node) return Boolean)
   is
   begin
      Mulog.Log (Msg => "Checking" & DOM.Core.Nodes.Length
                 (List => Source_Nodes)'Img & " " & Log_Message);

      for I in 0 .. DOM.Core.Nodes.Length (List => Source_Nodes) - 1 loop
         declare
            Node : constant DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => Source_Nodes,
               Index => I);

            Match_Found : Boolean := False;
         begin
            Find_Match :
            for J in 0 .. DOM.Core.Nodes.Length (List => Ref_Nodes) - 1 loop
               if Match
                 (Left  => Node,
                  Right => DOM.Core.Nodes.Item (List  => Ref_Nodes,
                                                Index => J))
               then
                  Match_Found := True;
                  exit Find_Match;
               end if;
            end loop Find_Match;

            if not Match_Found then
               raise Validation_Error with Error (Node => Node);
            end if;
         end;
      end loop;
   end For_Each_Match;

   -------------------------------------------------------------------------

   procedure For_Each_Match
     (XML_Data     : Muxml.XML_Data_Type;
      Source_XPath : String;
      Ref_XPath    : String;
      Log_Message  : String;
      Error        : not null access function
        (Node : DOM.Core.Node) return String;
      Match        : not null access function
        (Left, Right : DOM.Core.Node) return Boolean)
   is
      use McKae.XML.XPath.XIA;

      Ref_Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => Ref_XPath);
      Nodes     : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => Source_XPath);
   begin
      For_Each_Match (Source_Nodes => Nodes,
                      Ref_Nodes    => Ref_Nodes,
                      Log_Message  => Log_Message,
                      Error        => Error,
                      Match        => Match);
   end For_Each_Match;

   -------------------------------------------------------------------------

   function Match_Subject_Name (Left, Right : DOM.Core.Node) return Boolean
   is
      Ref_Name : constant String := DOM.Core.Elements.Get_Attribute
        (Elem => Left,
         Name => "subject");
      Subject_Name : constant String := DOM.Core.Elements.Get_Attribute
        (Elem => Right,
         Name => "name");
   begin
      return Ref_Name = Subject_Name;
   end Match_Subject_Name;

end Mucfgcheck;
