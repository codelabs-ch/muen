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

--  This is the root hierarchy of the DOM packages.
--  DOM is a standard API to interface to XML trees. It is divided into
--  several main parts (Core, Events, ...).
--  Note that to be fully compliant, strings must be UTF-16 encoded. However,
--  you still have the choice in this implementation, so that you can use
--  UTF-8 encoding for documents that only have ASCII characters (and thus save
--  some space in memory).
--  This encoding is in the file xml/encodings.ads
--
--  The comments in these packages are not meant as a tutorial, or even a
--  reference documentation for DOM. Please see the official XML
--  documentation.

package DOM is

end DOM;
