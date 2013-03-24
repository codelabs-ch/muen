------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

with Ada.Exceptions;
with Sax.Locators;
with Unicode.CES;

package Sax.Exceptions is

   -------------------
   -- Sax_Exception --
   -------------------

   type Sax_Exception (<>) is tagged private;
   --  General type that encapsulates a general SAX error or warning.
   --  It does not contain source location information (see Sax_Parse_Exception
   --  instead)

   function Create (Ada_Exception : Ada.Exceptions.Exception_Id)
      return Sax_Exception'Class;
   --  Create a new SAX exception wrapping an existing exception

   function Create (Message : Unicode.CES.Byte_Sequence)
      return Sax_Exception'Class;
   --  Create a new SAX exception

   function Create
     (Message       : Unicode.CES.Byte_Sequence;
      Ada_Exception : Ada.Exceptions.Exception_Id)
      return Sax_Exception'Class;
   --  Create a new SAX exception from an existing exception

   function Get_Exception (Except : Sax_Exception)
      return Ada.Exceptions.Exception_Id;
   --  Return the embedded exception

   function Get_Message (Except : Sax_Exception)
      return Unicode.CES.Byte_Sequence;
   --  Return the message

   -------------------------
   -- Sax_Parse_Exception --
   -------------------------

   type Sax_Parse_Exception (<>) is new Sax_Exception with private;

   function Create (Message : Unicode.CES.Byte_Sequence;
                    Loc     : Sax.Locators.Location)
      return Sax_Parse_Exception'Class;

   function Create
     (Message       : Unicode.CES.Byte_Sequence;
      Ada_Exception : Ada.Exceptions.Exception_Id;
      Loc           : Sax.Locators.Location)
      return Sax_Exception'Class;
   --  Create a new Sax_Parse_Exception. Note: no copy of Loc is made.

   function Get_Location (Except : Sax_Parse_Exception)
      return Sax.Locators.Location;
   --  return the location where the exception was raised.

private
   type Sax_Exception (Length : Natural) is tagged record
      Message : Unicode.CES.Byte_Sequence (1 .. Length);
      Except  : Ada.Exceptions.Exception_Id;
   end record;

   type Sax_Parse_Exception is new Sax_Exception with record
      Loc : Sax.Locators.Location;
   end record;
end Sax.Exceptions;
