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

with Ada.Strings.Fixed;
with Ada.Characters.Handling;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Muxml.Utils;

with Mutools.Utils;

package body Cspec.Utils
is

   use Ada.Strings.Unbounded;

   function I
     (N         : Positive := 1;
      Unit_Size : Positive := 3)
      return String renames Mutools.Utils.Indent;

   function S
     (Source : Unbounded_String)
      return String
      renames To_String;

   function U
     (Source : String)
      return Unbounded_String
      renames To_Unbounded_String;

   --  Return common array attributes as unbounded strings.
   procedure Array_Attrs_As_String
     (Arr          :     DOM.Core.Node;
      Array_Kind   :     String;
      Element_Size : out Unbounded_String;
      Virtual_Base : out Unbounded_String);

   --  Convert given array node to string representation.
   function To_Array_Str
     (Arr        : DOM.Core.Node;
      Array_Kind : String;
      Logical    : Unbounded_String)
      return Unbounded_String;

   -------------------------------------------------------------------------

   procedure Array_Attrs_As_String
     (Arr          :     DOM.Core.Node;
      Array_Kind   :     String;
      Element_Size : out Unbounded_String;
      Virtual_Base : out Unbounded_String)
   is
   begin
      Element_Size := U
        (DOM.Core.Elements.Get_Attribute
           (Elem => Arr,
            Name => "elementSize"));
      Virtual_Base := U
        (DOM.Core.Elements.Get_Attribute
           (Elem => Arr,
            Name => "virtualAddressBase"));

      if Element_Size = Null_Unbounded_String
        or else Virtual_Base = Null_Unbounded_String
      then
         raise Attribute_Error with Array_Kind & " array node does not "
           & "provide expected attributes";
      end if;
   end Array_Attrs_As_String;

   -------------------------------------------------------------------------

   procedure Channel_Attrs_As_String
     (Node   :     DOM.Core.Node;
      Kind   : out Ada.Strings.Unbounded.Unbounded_String;
      Vector : out Ada.Strings.Unbounded.Unbounded_String;
      Event  : out Ada.Strings.Unbounded.Unbounded_String)
   is
   begin
      Kind := U
        (Ada.Characters.Handling.To_Lower
           (Item => Get_Channel_Kind (Node => Node)'Img));
      Vector := U
        (DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "vector"));
      Event := U
        (DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "event"));
   end Channel_Attrs_As_String;

   -------------------------------------------------------------------------

   procedure Channel_Reader_Array_Attrs_As_String
     (Arr         :     DOM.Core.Node;
      Vector_Base : out Ada.Strings.Unbounded.Unbounded_String)
   is
      Event_Base : Unbounded_String;
   begin
      Vector_Base := U
        (DOM.Core.Elements.Get_Attribute
           (Elem => Arr,
            Name => "vectorBase"));
      Event_Base := U
        (DOM.Core.Elements.Get_Attribute
           (Elem => Arr,
            Name => "eventBase"));

      if Event_Base /= Null_Unbounded_String then
         raise Attribute_Error with "Channel reader array specifies invalid "
           & "'eventBase' attribute";
      end if;
   end Channel_Reader_Array_Attrs_As_String;

   -------------------------------------------------------------------------

   procedure Channel_Writer_Array_Attrs_As_String
     (Arr        :     DOM.Core.Node;
      Event_Base : out Ada.Strings.Unbounded.Unbounded_String)
   is
      Vector_Base : Unbounded_String;
   begin
      Event_Base := U
        (DOM.Core.Elements.Get_Attribute
           (Elem => Arr,
            Name => "eventBase"));
      Vector_Base := U
        (DOM.Core.Elements.Get_Attribute
           (Elem => Arr,
            Name => "vectorBase"));

      if Vector_Base /= Null_Unbounded_String then
         raise Attribute_Error with "Channel writer array specifies invalid "
           & "'vectorBase' attribute";
      end if;
   end Channel_Writer_Array_Attrs_As_String;

   -------------------------------------------------------------------------

   procedure Device_Irq_Attrs_As_String
     (Irq     :     DOM.Core.Node;
      Logical : out Ada.Strings.Unbounded.Unbounded_String;
      Vector  : out Ada.Strings.Unbounded.Unbounded_String)
   is
   begin
      Logical := U (DOM.Core.Elements.Get_Attribute
                    (Elem => Irq,
                     Name => "logical"));
      Vector := U (DOM.Core.Elements.Get_Attribute
                   (Elem => Irq,
                    Name => "vector"));

      if Logical = Null_Unbounded_String
        or else Vector = Null_Unbounded_String
      then
         raise Attribute_Error with "Device irq node does not provide "
           & "expected attributes";
      end if;
   end Device_Irq_Attrs_As_String;

   -------------------------------------------------------------------------

   function Get_Array_Element_Count (Arr : DOM.Core.Node) return Natural
   is
      Children : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Arr,
           XPath => "*");
      Length : constant Natural := DOM.Core.Nodes.Length (List => Children);
   begin
      for I in reverse 1 .. Length loop
         declare
            Element : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Children,
                                      Index => I - 1);
            Idx_Str : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Element,
                 Name => "index");
         begin
            if Idx_Str /= "" then
               return Natural'Value (Idx_Str) + Length - I;
            end if;
         end;
      end loop;

      return Length;
   end Get_Array_Element_Count;

   -------------------------------------------------------------------------

   function Get_Channel_Kind (Node : DOM.Core.Node) return Channel_Kind
   is
   begin
      return Channel_Kind'Value
        (DOM.Core.Elements.Get_Tag_Name (Elem => Node));

   exception
      when Constraint_Error =>
         raise Attribute_Error with "Unable to determine channel kind of "
           & "invalid node '" & DOM.Core.Elements.Get_Tag_Name (Elem => Node)
           & "'";
   end Get_Channel_Kind;

   -------------------------------------------------------------------------

   function Is_Present
     (Policy    : Muxml.XML_Data_Type;
      Comp_Name : String)
      return Boolean
   is
      use type DOM.Core.Node;

      C : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Policy.Doc,
           XPath => "/system/components/*[@name='" & Comp_Name & "']");
   begin
      return C /= null;
   end Is_Present;

   -------------------------------------------------------------------------

   procedure Memory_Attrs_As_String
     (Node            :     DOM.Core.Node;
      Logical_Name    : out Unbounded_String;
      Virtual_Address : out Unbounded_String;
      Size            : out Unbounded_String)
   is
   begin
      Logical_Name := U
        (DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "logical"));
      Virtual_Address := U
        (DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "virtualAddress"));
      Size := U
        (DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "size"));

      if Logical_Name = Null_Unbounded_String
        or else Virtual_Address = Null_Unbounded_String
        or else Size = Null_Unbounded_String
      then
         raise Attribute_Error with "Memory node does not provide "
           & "expected attributes";
      end if;
   end Memory_Attrs_As_String;

   -------------------------------------------------------------------------

   procedure Memory_Perm_Attrs_As_String
     (Node       :     DOM.Core.Node;
      Executable : out Ada.Strings.Unbounded.Unbounded_String;
      Writable   : out Ada.Strings.Unbounded.Unbounded_String)
   is
      Exec  : constant String
        := DOM.Core.Elements.Get_Attribute
          (Elem => Node,
           Name => "executable");
      Write : constant String
        := DOM.Core.Elements.Get_Attribute
          (Elem => Node,
           Name => "writable");
   begin
      if Exec'Length = 0 or else Write'Length = 0 then
         raise Attribute_Error with Mutools.Utils.Capitalize
           (Str => DOM.Core.Elements.Get_Tag_Name
              (Elem => Node)) & " node does not provide expected permission "
           & "attributes";
      end if;

      Executable := U (Mutools.Utils.Capitalize (Str => Exec));
      Writable   := U (Mutools.Utils.Capitalize (Str => Write));
   end Memory_Perm_Attrs_As_String;

   -------------------------------------------------------------------------

   function To_Array_Str
     (Arr        : DOM.Core.Node;
      Array_Kind : String;
      Logical    : Unbounded_String)
      return Unbounded_String
   is
      Res, Addr, Size : Unbounded_String;

      Child_Count : constant Positive := Get_Array_Element_Count (Arr => Arr);
   begin
      Array_Attrs_As_String
        (Arr          => Arr,
         Array_Kind   => Array_Kind,
         Element_Size => Size,
         Virtual_Base => Addr);

      Res :=
        I & Logical & "_Address_Base  : constant := " & Addr & ";"
        & ASCII.LF
        & I & Logical & "_Element_Size  : constant := " & Size & ";"
        & ASCII.LF
        & I & Logical & "_Element_Count : constant :=" & Child_Count'Img & ";";

      return Res;
   end To_Array_Str;

   -------------------------------------------------------------------------

   function To_Channel_Array_Str (Arr : DOM.Core.Node) return String
   is
      --  Return string representation of reader/writer specific attributes.
      function To_Reader_Writer_Str
        (Logical : Unbounded_String)
         return Unbounded_String;

      ----------------------------------------------------------------------

      function To_Reader_Writer_Str
        (Logical : Unbounded_String)
         return Unbounded_String
      is
         Kind : constant Channel_Kind := Get_Channel_Kind
           (Node => Muxml.Utils.Get_Element
              (Doc   => Arr,
               XPath => "*[self::reader or self::writer]"));

         --  Return string representation of reader attribute.
         function To_Reader_Str return Unbounded_String;

         --  Return string representation of writer attribute.
         function To_Writer_Str return Unbounded_String;

         -------------------------------------------------------------------

         function To_Reader_Str return Unbounded_String
         is
            Res, Vb : Unbounded_String;
         begin
            Channel_Reader_Array_Attrs_As_String
              (Arr         => Arr,
               Vector_Base => Vb);

            if Vb /= Null_Unbounded_String then
               Res := ASCII.LF &
                 I & Logical & "_Vector_Base   : constant := " & Vb & ";";
            end if;
            return Res;
         end To_Reader_Str;

         -------------------------------------------------------------------

         function To_Writer_Str return Unbounded_String
         is
            Res, Eb : Unbounded_String;
         begin
            Channel_Writer_Array_Attrs_As_String
              (Arr        => Arr,
               Event_Base => Eb);

            if Eb /= Null_Unbounded_String then
               Res := ASCII.LF &
                 I & Logical & "_Event_Base    : constant := " & Eb & ";";
            end if;
            return Res;
         end To_Writer_Str;
      begin
         case Kind is
            when Reader => return To_Reader_Str;
            when Writer => return To_Writer_Str;
         end case;
      end To_Reader_Writer_Str;

      Res, Logical : Unbounded_String;
   begin
      Logical := U (Mutools.Utils.To_Ada_Identifier
                    (Str => DOM.Core.Elements.Get_Attribute
                     (Elem => Arr,
                      Name => "logical")));

      Res := To_Array_Str (Arr        => Arr,
                           Array_Kind => "Channel",
                           Logical    => Logical);

      Res := Res & To_Reader_Writer_Str (Logical => Logical);
      Res := Res & ASCII.LF & ASCII.LF & To_Name_Array (Arr => Arr);

      return S (Res);
   end To_Channel_Array_Str;

   -------------------------------------------------------------------------

   function To_Channel_Str (Channel : DOM.Core.Node) return String
   is
      Res, Logical, Addr, Size, Kind, Vector, Event : Unbounded_String;
   begin
      Memory_Attrs_As_String
        (Node            => Channel,
         Logical_Name    => Logical,
         Virtual_Address => Addr,
         Size            => Size);
      Channel_Attrs_As_String
        (Node   => Channel,
         Kind   => Kind,
         Vector => Vector,
         Event  => Event);

      Logical := U (Mutools.Utils.To_Ada_Identifier (Str => S (Logical)));
      Kind    := U (Mutools.Utils.Capitalize (Str => S (Kind)));

      Res :=
        I & Logical & "_Address : constant := " & Addr & ";"
        & ASCII.LF
        & I & Logical & "_Size    : constant := " & Size & ";"
        & ASCII.LF
        & I & Logical & "_Kind    : constant Channel_Kind := Channel_"
        & Kind & ";";

      if Vector /= Null_Unbounded_String then
         Res := Res
           & ASCII.LF
           & I & Logical & "_Vector  : constant := " & Vector & ";";
      end if;

      if Event /= Null_Unbounded_String then
         Res := Res
           & ASCII.LF
           & I & Logical & "_Event   : constant := " & Event & ";";
      end if;

      return S (Res);
   end To_Channel_Str;

   -------------------------------------------------------------------------

   function To_Device_Str (Device : DOM.Core.Node) return String
   is
      Res      : Unbounded_String;
      Devname  : constant String
        := DOM.Core.Elements.Get_Attribute
          (Elem => Device,
           Name => "logical") & "_";
      Memory   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Device,
           XPath => "memory");
      IRQs     : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Device,
           XPath => "irq");
      Memcount : constant Natural
        := DOM.Core.Nodes.Length (List => Memory);
      Irqcount : constant Natural
        := DOM.Core.Nodes.Length (List => IRQs);
   begin
      for I in 1 .. Irqcount loop
         Res := Res & To_Irq_Str
           (Irq            => DOM.Core.Nodes.Item
              (List  => IRQs,
               Index => I - 1),
            Logical_Prefix => Devname);
         if I /= Irqcount then
            Res := Res & ASCII.LF;
         end if;
      end loop;

      if Irqcount > 0 and then Memcount > 0 then
         Res := Res & ASCII.LF & ASCII.LF;
      end if;

      for I in 1 .. Memcount loop
         Res := Res & To_Memory_Str
           (Memory         => DOM.Core.Nodes.Item
              (List  => Memory,
               Index => I - 1),
            Logical_Prefix => Devname);
         if I /= Memcount then
            Res := Res & ASCII.LF & ASCII.LF;
         end if;
      end loop;

      return S (Res);
   end To_Device_Str;

   -------------------------------------------------------------------------

   function To_Irq_Str
     (Irq            : DOM.Core.Node;
      Logical_Prefix : String)
      return String
   is
      Logical, Vector : Unbounded_String;
   begin
      Device_Irq_Attrs_As_String
        (Irq     => Irq,
         Logical => Logical,
         Vector  => Vector);

      Logical := U (Mutools.Utils.To_Ada_Identifier
                    (Str => Logical_Prefix & S (Logical)));

      return S (I & Logical & " : constant := " & Vector & ";");
   end To_Irq_Str;

   -------------------------------------------------------------------------

   function To_Memory_Array_Str (Arr : DOM.Core.Node) return String
   is
      Res, Logical, Exec, Writ : Unbounded_String;
   begin
      Logical := U (Mutools.Utils.To_Ada_Identifier
                    (Str => DOM.Core.Elements.Get_Attribute
                     (Elem => Arr,
                      Name => "logical")));
      Memory_Perm_Attrs_As_String (Node       => Arr,
                                   Executable => Exec,
                                   Writable   => Writ);

      Res := To_Array_Str (Arr        => Arr,
                           Array_Kind => "Memory",
                           Logical    => Logical);

      Res := Res
        & ASCII.LF
        & I & Logical & "_Executable    : constant Boolean := " & Exec & ";"
        & ASCII.LF
        & I & Logical & "_Writable      : constant Boolean := " & Writ & ";";

      Res := Res & ASCII.LF & ASCII.LF & To_Name_Array (Arr => Arr);

      return S (Res);
   end To_Memory_Array_Str;

   -------------------------------------------------------------------------

   function To_Memory_Str (Memory : DOM.Core.Node) return String
   is
   begin
      return To_Memory_Str
        (Memory         => Memory,
         Logical_Prefix => "");
   end To_Memory_Str;

   -------------------------------------------------------------------------

   function To_Memory_Str
     (Memory         : DOM.Core.Node;
      Logical_Prefix : String)
      return String
   is
      Res, Logical, Addr, Size, Executable, Writable : Unbounded_String;
   begin
      Memory_Attrs_As_String
        (Node            => Memory,
         Logical_Name    => Logical,
         Virtual_Address => Addr,
         Size            => Size);
      Memory_Perm_Attrs_As_String
        (Node       => Memory,
         Executable => Executable,
         Writable   => Writable);

      Logical := U (Mutools.Utils.To_Ada_Identifier
                    (Str => Logical_Prefix & S (Logical)));

      Res :=
        I & Logical & "_Address    : constant := " & Addr & ";"
        & ASCII.LF
        & I & Logical & "_Size       : constant := " & Size & ";"
        & ASCII.LF
        & I & Logical & "_Executable : constant Boolean := " & Executable & ";"
        & ASCII.LF
        & I & Logical & "_Writable   : constant Boolean := " & Writable & ";";

      return S (Res);
   end To_Memory_Str;

   -------------------------------------------------------------------------

   function To_Name_Array (Arr : DOM.Core.Node) return String
   is
      Arr_Name : constant Unbounded_String
        := U (Mutools.Utils.To_Ada_Identifier
              (Str => DOM.Core.Elements.Get_Attribute
               (Elem => Arr,
                Name => "logical")));
      Children : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Arr,
           XPath => "*");
      Child_Count : constant Positive
        := DOM.Core.Nodes.Length (List => Children);

      Cur_Idx : Natural := 1;
      Res     : Unbounded_String;
   begin
      Res := I & Arr_Name & "_Names : constant Name_Array (1 .. " & Arr_Name
        & "_Element_Count)" & ASCII.LF
        & I (N         => 5,
             Unit_Size => 1) & ":= (" & ASCII.LF;
      for J in 1 .. Child_Count loop
         declare
            Child : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                   (List  => Children,
                    Index => J - 1);
            Logical : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Child,
                 Name => "logical");
            Idx_Str : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Child,
                 Name => "index");
            Idx_Nr : constant Natural
              := (if Idx_Str /= "" then Natural'Value (Idx_Str) else Cur_Idx);
            Nr : constant String
              := Ada.Strings.Fixed.Trim
                (Source => Idx_Nr'Img,
                 Side   => Ada.Strings.Left);
         begin
            Res := Res & I (N         => 9,
                            Unit_Size => 1)
              & Nr & " => To_Name (Str => """ & Logical & """)";

            if J /= Child_Count then
               Res := Res & "," & ASCII.LF;
            end if;
            Cur_Idx := Idx_Nr + 1;
         end;
      end loop;

      if Cur_Idx > Child_Count + 1 then
         Res := Res & "," & ASCII.LF
           & I (N         => 9,
                Unit_Size => 1)
           & "others => Name_Type'(others => ' ')";
      end if;

      Res := Res & ASCII.LF & I
        (N         => 8,
         Unit_Size => 1) & ");";

      return S (Res);
   end To_Name_Array;

end Cspec.Utils;
