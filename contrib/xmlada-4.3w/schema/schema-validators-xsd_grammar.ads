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

--  This file contains a function that manually creates a schema-for-schema
--  grammar, ie a grammar that is used to validate a schema file.
--
--  This is intended both as an example on how to manually write grammars,
--  and as a way to check schema.xsd.

with Schema.Validators;

private package Schema.Validators.XSD_Grammar is

   procedure Add_Schema_For_Schema
     (R : in out Schema.Validators.Abstract_Validation_Reader'Class);
   --  Adds the definition for the standard XML namespaces (schemas,...) to
   --  Grammar. The resulting grammar can thus be used, among other things, to
   --  validate schema files. This doesn't add the predefined types, which
   --  must be added separately.

end Schema.Validators.XSD_Grammar;
