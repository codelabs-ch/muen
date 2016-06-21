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

with Ada.Strings.Unbounded;

with DOM.Core;

with Muxml;

package Cspec.Utils
is

   --  Checks whether the component with given name is present in the policy.
   function Is_Present
     (Policy    : Muxml.XML_Data_Type;
      Comp_Name : String)
      return Boolean;

   --  Convert given memory node to string representation.
   function To_Memory_Str (Memory : DOM.Core.Node) return String;

   --  Convert given channel node to string representation.
   function To_Channel_Str (Channel : DOM.Core.Node) return String;

   --  Convert given device node to string representation.
   function To_Device_Str (Device : DOM.Core.Node) return String;

   --  Convert given memory array node to string representation.
   function To_Memory_Array_Str (Arr : DOM.Core.Node) return String;

   Attribute_Error : exception;

private

   --  Convert given memory node to string representation. Add specified prefix
   --  to the generated constant name.
   function To_Memory_Str
     (Memory         : DOM.Core.Node;
      Logical_Prefix : String)
      return String;

   --  Convert given IRQ node to string representation. Add specified prefix
   --  to the generated constant name.
   function To_Irq_Str
     (Irq            : DOM.Core.Node;
      Logical_Prefix : String)
      return String;

   --  Return attributes of memory node as unbounded strings.
   procedure Memory_Attrs_As_String
     (Node            :     DOM.Core.Node;
      Logical_Name    : out Ada.Strings.Unbounded.Unbounded_String;
      Virtual_Address : out Ada.Strings.Unbounded.Unbounded_String;
      Size            : out Ada.Strings.Unbounded.Unbounded_String);

   --  Return permission attributes of memory node as unbounded strings.
   procedure Memory_Perm_Attrs_As_String
     (Node       :     DOM.Core.Node;
      Executable : out Ada.Strings.Unbounded.Unbounded_String;
      Writable   : out Ada.Strings.Unbounded.Unbounded_String);

   --  Return attributes of channel node as unbounded strings. Vector and event
   --  may be empty strings if the channel uses no signalization.
   procedure Channel_Attrs_As_String
     (Node   :     DOM.Core.Node;
      Kind   : out Ada.Strings.Unbounded.Unbounded_String;
      Vector : out Ada.Strings.Unbounded.Unbounded_String;
      Event  : out Ada.Strings.Unbounded.Unbounded_String);

   --  Return device IRQ attributes as unbounded strings.
   procedure Device_Irq_Attrs_As_String
     (Irq     :     DOM.Core.Node;
      Logical : out Ada.Strings.Unbounded.Unbounded_String;
      Vector  : out Ada.Strings.Unbounded.Unbounded_String);

   --  Return memory array attributes as unbounded strings.
   procedure Memory_Array_Attrs_As_String
     (Arr          :     DOM.Core.Node;
      Logical      : out Ada.Strings.Unbounded.Unbounded_String;
      Element_Size : out Ada.Strings.Unbounded.Unbounded_String;
      Virtual_Base : out Ada.Strings.Unbounded.Unbounded_String;
      Executable   : out Ada.Strings.Unbounded.Unbounded_String;
      Writable     : out Ada.Strings.Unbounded.Unbounded_String);

   --  Return channel reader array attributes as unbounded strings.
   procedure Channel_Reader_Array_Attrs_As_String
     (Arr         :     DOM.Core.Node;
      Vector_Base : out Ada.Strings.Unbounded.Unbounded_String);

   --  Return channel writer array attributes as unbounded strings.
   procedure Channel_Writer_Array_Attrs_As_String
     (Arr        :     DOM.Core.Node;
      Event_Base : out Ada.Strings.Unbounded.Unbounded_String);

end Cspec.Utils;
