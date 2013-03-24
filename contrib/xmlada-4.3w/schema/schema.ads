------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_05;
private with Ada.Tags;

package Schema is

   procedure Set_Debug_Output (Output : Boolean);
   --  Whether we should output debug traces

   XML_Not_Implemented : exception;
   --  Raised when a schema uses features that are not supported by XML/Ada yet

   Dump_Internal_XSD : Boolean := False;
   --  If set to True, dump on stdout the structure that was created when
   --  parsing the .xsd files. In particular, this is used to create
   --  the metaschema grammar internally, rather than part it from file
   --  every time.

private

   -----------
   -- Debug --
   -----------
   --  The following subprograms are used to print debug traces for XML/Ada
   --  itself, and should not be used in user applications

   type Debug_Output_Mode is
     (Debug_Default,
      Debug_Seen,    --  to show elements seen in XML stream
      Debug_Action); --  to show actions performed on the grammars

   procedure Debug_Push_Prefix
     (Append : String; Mode : Debug_Output_Mode := Debug_Default);
   procedure Debug_Pop_Prefix;
   --  Append a prefix to the current output

   function Debug_Tag_Name (Self : Ada.Tags.Tag) return String;
   --  Return the external name for Self

   procedure Debug_Output
     (Str : String; Mode : Debug_Output_Mode := Debug_Default);
   pragma Inline (Debug_Output);
   --  Display a string for debugging purposes

   procedure Output_Action (Str : String);
   procedure Output_Seen (Str : String);
   pragma Inline (Output_Action, Output_Seen);
   --  Same as Debug_Output (Str, Debug_Action);
   --  or Debug_Output (Debug_Seen);

   Debug : Boolean := False;
   --  Whether we are in debug mode.
   --  The above subprograms do nothing if not in debug mode, but this
   --  variable can be used to avoid preparing strings for display if we are
   --  not going to display them afterward.

end Schema;
