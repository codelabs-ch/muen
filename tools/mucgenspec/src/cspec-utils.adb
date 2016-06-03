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

   -------------------------------------------------------------------------

   procedure Channel_Attrs_As_String
     (Node   :     DOM.Core.Node;
      Kind   : out Ada.Strings.Unbounded.Unbounded_String;
      Vector : out Ada.Strings.Unbounded.Unbounded_String;
      Event  : out Ada.Strings.Unbounded.Unbounded_String)
   is
   begin
      Kind := To_Unbounded_String
        (DOM.Core.Elements.Get_Tag_Name (Elem => Node));
      if Kind /= "reader" and then Kind /= "writer" then
         raise Attribute_Error with "Unable to extract channel attributes from"
           & " unexpected node '" & To_String (Kind) & "'";
      end if;

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

   function Is_Present
     (Policy    : Muxml.XML_Data_Type;
      Comp_Name : String)
      return Boolean
   is
      use type DOM.Core.Node;

      C : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Policy.Doc,
           XPath => "/system/components/component[@name='" & Comp_Name & "']");
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
         raise Attribute_Error with "Memory node does not provide "
           & "expected permission attributes";
      end if;

      Executable := U (Mutools.Utils.Capitalize (Str => Exec));
      Writable   := U (Mutools.Utils.Capitalize (Str => Write));
   end Memory_Perm_Attrs_As_String;

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
      Memcount : constant Natural
        := DOM.Core.Nodes.Length (List => Memory);
   begin
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

end Cspec.Utils;
