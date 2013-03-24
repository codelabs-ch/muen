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

with Ada.Exceptions;  use Ada.Exceptions;
with Unicode.CES;     use Unicode.CES;
with Sax.Locators;    use Sax.Locators;

package body Sax.Exceptions is

   ------------
   -- Create --
   ------------

   function Create (Ada_Exception : Ada.Exceptions.Exception_Id)
      return Sax_Exception'Class is
   begin
      return Sax_Exception'
        (Length => 0, Message => "", Except => Ada_Exception);
   end Create;

   ------------
   -- Create --
   ------------

   function Create (Message : Byte_Sequence) return Sax_Exception'Class is
   begin
      return Sax_Exception'(Length  => Message'Length,
                            Message => Message,
                            Except  => Null_Id);
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Message : Byte_Sequence;
      Ada_Exception : Ada.Exceptions.Exception_Id) return Sax_Exception'Class
   is
   begin
      return Sax_Exception'(Length  => Message'Length,
                            Message => Message,
                            Except  => Ada_Exception);
   end Create;

   -------------------
   -- Get_Exception --
   -------------------

   function Get_Exception (Except : Sax_Exception)
      return Ada.Exceptions.Exception_Id is
   begin
      return Except.Except;
   end Get_Exception;

   -----------------
   -- Get_Message --
   -----------------

   function Get_Message (Except : Sax_Exception) return Byte_Sequence is
   begin
      return Except.Message;
   end Get_Message;

   ------------
   -- Create --
   ------------

   function Create (Message : Unicode.CES.Byte_Sequence;
                    Loc     : Sax.Locators.Location)
      return Sax_Parse_Exception'Class
   is
      Pe : Sax_Parse_Exception (Message'Length);
   begin
      Pe.Message := Message;
      Pe.Loc := Loc;
      Pe.Except := Null_Id;
      return Pe;
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Message       : Unicode.CES.Byte_Sequence;
      Ada_Exception : Ada.Exceptions.Exception_Id;
      Loc           : Sax.Locators.Location)
      return Sax_Exception'Class
   is
      Pe : Sax_Parse_Exception (Message'Length);
   begin
      Pe.Message := Message;
      Pe.Loc     := Loc;
      Pe.Except  := Ada_Exception;
      return Pe;
   end Create;

   ------------------
   -- Get_Location --
   ------------------

   function Get_Location (Except : Sax_Parse_Exception)
      return Sax.Locators.Location is
   begin
      return Except.Loc;
   end Get_Location;

end Sax.Exceptions;
