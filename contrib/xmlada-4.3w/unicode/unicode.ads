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

--  This package provides support for wide-characters in Unicode/Iso 10646
--  encoding.
--  A series of child packages are given to convert from any encoding to
--  Unicode.
--  It also supports several transformation format (ie serialization of
--  these characters to files), like UTF8, UTF16,...

--  Vocabulary used in this package: This is only a small extract of
--  documents found at http://www.unicode.org/unicode/reports/tr17
--
--  Repertoire
--  ==========
--  Set of abstract characters to be encoded, normally a familiar alphabet or
--  symbol set.
--  Unicode is one such repertoire, although an open one. New entries are
--  added to it, but none is ever deleted from it.
--  Internally, this package converts all characters to entries in the Unicode
--  repertoire
--
--  Glyphs
--  ======
--  A particular image which represents a character or part of a character. For
--  instance, a given character might have a slightly different aspect in
--  different fonts.
--  Note that a single glyph can correspond to a sequence of characters, or a
--  single character to a sequence of glyphs.
--  This package doesn't deal at all with glyphs, this is left to the end-user
--  application
--
--  Subsets
--  =======
--  Unicode is intended to be a universal repertoire, with all possible
--  characters. Most applications will only support a subset of it, given the
--  complexity of some scripts.
--  The Unicode standad includes a set of internal catalogs, called
--  collections. Several child packages exist to support these collections.
--
--  Coded character sets  (packages Unicode.CCS.*)
--  ====================
--  Mapping from a set of abstract characters to the set of non-negative
--  integers
--  The integer associated with a character is called "code point", and the
--  character is called "encoded character"
--  Examples of these are:  ISO/8859-1, JIS X 0208, ...
--
--  Character naming (packages Unicode.Names.*)
--  ================
--  A unique name is assigned to each abstract character, so that it is
--  possible to get the same character no matter what repertoire is used.
--
--  Character Encoding Forms
--  ========================
--  Mapping from the set of integers used in a Coded Character Set to the set
--  of sequences of code units.
--  A "code unit" is integer occupying a specified binary width in a computer
--  architecture
--  Examples of fixed-width encoding forms:  7-bit, 8-bit, EBCDIC
--  Examples of variable-width encoding forms:  Utf-8, Utf-16,...
--
--  Character Encoding Scheme (packages Unicode.CES.*)
--  =========================
--  Mapping of code units into serialized byte sequences. It also takes into
--  account the byte-order serialization.

--  As a summary, converting a file containing latin-1 characters coded on
--  8 bits to a Utf8 latin2 file, the following steps are involved:
--
--     Latin1 string  (contains bytes associated with code points in Latin1)
--       |    "use Unicode.CES.Basic_8bit.To_Utf32"
--       v
--     Utf32 latin1 string (contains code points in Latin1)
--       |    "Convert argument to To_Utf32 should be
--       v         Unicode.CCS.Iso_8859_1.Convert"
--     Utf32 Unicode string (contains code points in Unicode)
--       |    "use Unicode.CES.Utf8.From_Utf32"
--       v
--     Utf8 Unicode string (contains code points in Unicode)
--       |    "Convert argument to From_Utf32 should be
--       v         Unicode.CCS.Iso_8859_2.Convert"
--     Utf8 Latin2 string (contains code points in Latin2)

--  In the package below, all the functions Is_* are based on values defined
--  in the XML standard.
--  Several child packages are provided, that support different encoding
--  forms, and can all convert from and to Utf32, which thus behaves as the
--  reference.

package Unicode is
   pragma Preelaborate (Unicode);

   type Unicode_Char is mod 2**32;
   --  A code point associated with a given character, taken in the Unicode
   --  repertoire.
   --  Note that by design, the first 127 entries are taken in the ASCII set
   --  and are fully compatible. You can thus easily compare this with
   --  constant characters by using Character'Pos ('.')

   function Is_White_Space (Char : Unicode_Char) return Boolean;
   --  Return True if Char is a space character, ie a space, horizontal tab,
   --  line feed or carriage return.

   function Is_Letter (Char : Unicode_Char) return Boolean;
   --  True if Char is a letter.

   function Is_Base_Char (Char : Unicode_Char) return Boolean;
   --  True if Char is a base character.

   function Is_Digit (Char : Unicode_Char) return Boolean;
   --  True if Char is a digit.

   function Is_Combining_Char (Char : Unicode_Char) return Boolean;
   --  True if Char is a combining character (ie a character that
   --  applies to the preceding character to change its meaning, like
   --  accents in latin-1).

   function Is_Extender (Char : Unicode_Char) return Boolean;
   --  True if Char is an extender character.

   function Is_Ideographic (Char : Unicode_Char) return Boolean;
   --  True if Char is an ideographic character (asian languages).

   function To_Unicode (C : Character) return Unicode_Char;
   --  Convert from Ada Character encoding (extended ASCII) to Unicode
   --  character.

private
   pragma Inline (Is_Ideographic);
   pragma Inline (Is_Letter);
   pragma Inline (Is_White_Space);
   pragma Inline (To_Unicode);
end Unicode;
