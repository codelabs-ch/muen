.. _The_Unicode_module:

******************
The Unicode module
******************

Unicode provides a unique number for every character, no matter what the
platform, no matter what the program, no matter what the language.

Fundamentally, computers just deal with numbers. They store letters and other
characters by assigning a number for each one. Before Unicode was invented,
there were hundreds of different encoding systems for assigning these numbers.
No single encoding could contain enough characters: for example, the European
Union alone requires several different encodings to cover all its languages.
Even for a single language like English no single encoding was adequate for all
the letters, punctuation, and technical symbols in common use.

These encoding systems also conflict with one another. That is, two encodings
can use the same number for two different characters, or use different numbers
for the same character. Any given computer (especially servers) needs to
support many different encodings; yet whenever data is passed between different
encodings or platforms, that data always runs the risk of corruption.

Unicode provides a unique number for every character, no matter what the
platform, no matter what the program, no matter what the language. The Unicode
Standard has been adopted by such industry leaders as Apple, HP, IBM,
JustSystem, Microsoft, Oracle, SAP, Sun, Sybase, Unisys and many others.
Unicode is required by modern standards such as XML, Java, ECMAScript
(JavaScript), LDAP, CORBA 3.0, WML, etc., and is the official way to implement
ISO/IEC 10646. It is supported in many operating systems, all modern browsers,
and many other products. The emergence of the Unicode Standard, and the
availability of tools supporting it, are among the most significant recent
global software technology trends.

The following sections explain the basic vocabulary and concepts associated
with Unicode and encodings.

Most of the information comes from the official Unicode Web site, at
`http://www.unicode.org/unicode/reports/tr17
<http://www.unicode.org/unicode/reports/tr17>`_.

Part of this documentation comes from `http://www.unicode.org
<http://www.unicode.org>`_, the official web site for Unicode.

Some information was also extracted from the "UTF-8 and Unicode FAQ" by M.
Kuhn, available at `??? <???>`_.

.. _Glyphs:

Glyphs
======

A glyph is a particular representation of a character or part of a character.

Several representations are possible, mostly depending on the exact font used
at that time. A single glyph can correspond to a sequence of characters, or a
single character to a sequence of glyphs.

The Unicode standard doesn't deal with glyphs, although a suggested
representation is given for each character in the standard. Likewise, this
module doesn't provide any graphical support for Unicode, and will just deal
with textual memory representation and encodings.

Take a look at the **GtkAda** library that provides the graphical interface for
unicode in the upcoming 2.0 version.

Repertoires and subsets
=======================

A repertoire is a set of abstract characters to be encoded, normally a familiar
alphabet or symbol set. For instance, the alphabet used to spell English words,
or the one used for the Russian alphabet are two such repertoires.

There exist two types of repertoires, close and open ones. The former is the
most common one, and the two examples above are such repertoires.  No character
is ever added to them.

Unicode is also a repertoire, but an open one. New entries are added to it.
However, it is guaranteed that none will ever be deleted from it.  Unicode
intends to be a universal repertoire, with all possible characters currently
used in the world. It currently contains all the alphabets, including a number
of alphabets associated with dead languages like hieroglyphs. It also contains
a number of often used symbols, like mathematical signs.

The goal of this Unicode module is to convert all characters to entries in the
Unicode repertoire, so that any applications can communicate with each other in
a portable manner.

Given its size, most applications will only support a subset of Unicode.  Some
of the scripts, most notably Arabic and Asian languages, require a special
support in the application (right-to-left writing,...), and thus will not be
supported by some applications.

The Unicode standard includes a set of internal catalogs, called collections.
Each character in these collections is given a special name, in addition to its
code, to improve readability.

Several child packages (**Unicode.Names.***) define those names. For instance:

*Unicode.Names.Basic_Latin*
  This contains the basic characters used in most western European languages,
  including the standard ASCII subset.

*Unicode.Names.Cyrillic*
  This contains the Russian alphabet.

*Unicode.Names.Mathematical_Operators*
  This contains several mathematical symbols

More than 80 such packages exist.

Character sets
==============

A character set is a mapping from a set of abstract characters to some
non-negative integers. The integer associated with a character is called its
code point, and the character itself is called the encoded character.

There exist a number of standard character sets, unfortunately not compatible
with each other. For instance, ASCII is one of these character sets, and
contains 128 characters. A super-set of it is the ISO/8859-1 character set.
Another character set is the JIS X 0208, used to encode Japanese characters.

Note that a character set is different from a repertoire. For instance, the
same character C with cedilla doesn't have the same integer value in the
ISO/8859-1 character set and the ISO/8859-2 character set.

Unicode is also such a character set, that contains all the possible characters
and associate a standard integer with them. A similar and fully compatible
character set is ISO/10646. The only addition that Unicode does to ISO/10646 is
that it also specifies algorithms for rendering presentation forms of some
scripts (say Arabic), handling of bi-directional texts that mix for instance
Latin and Hebrew, algorithms for sorting and string comparison, and much more.

Currently, our Unicode package doesn't include any support for these
algorithms.

Unicode and ISO 10646 define formally a 31-bit character set. However, of this
huge code space, so far characters have been assigned only to the first 65534
positions (0x0000 to 0xFFFD). The characters that are expected to be encoded
outside the 16-bit range belong all to rather exotic scripts (e.g.,
Hieroglyphics) that are only used by specialists for historic and scientific
purposes

The Unicode module contains a set of packages to provide conversion from some
of the most common character sets to and from Unicode. These are the
**Unicode.CCS.*** packages.

All these packages have a common structure:

* They define a global variable of type `Character_Set` with two fields, ie the
  two conversion functions between the given character set and Unicode.

  These functions convert one character (actually its code point) at a time.

* They also define a number of standard names associated with this character
  set. For instance, the ISO/8859-1 set is also known as Latin1.

  The function `Unicode.CCS.Get_Character_Set` can be used to find a character
  set by its standard name.

Currently, the following sets are supported:

*ISO/8859-1 aka Latin1*

  This is the standard character set used to represent most Western European
  languages including: Albanian, Catalan, Danish, Dutch, English, Faroese,
  Finnish, French, Galician, German, Irish, Icelandic, Italian, Norwegian,
  Portuguese, Spanish and Swedish.


*ISO/8859-2 aka Latin2*

  This character set supports the Slavic languages of Central Europe which use
  the Latin alphabet. The ISO-8859-2 set is used for the following languages:
  Czech, Croat, German, Hungarian, Polish, Romanian, Slovak and Slovenian.


*ISO/8859-3*

  This character set is used for Esperanto, Galician, Maltese and Turkish


*ISO/8859-4*

  Some letters were added to the ISO-8859-4 to support languages such as
  Estonian, Latvian and Lithuanian. It is an incomplete precursor of the Latin
  6 set.

Character encoding schemes
==========================

We now know how each encoded character can be represented by an integer value
(code point) depending on the character set.

Character encoding schemes deal with the representation of a sequence of
integers to a sequence of code units. A code unit is a sequence of bytes on a
computer architecture.

There exists a number of possible encoding schemes. Some of them encode all
integers on the same number of bytes. They are called fixed-width encoding
forms, and include the standard encoding for Internet emails (**7bits**, but it
can't encode all characters), as well as the simple **8bits** scheme, or the
**EBCDIC** scheme. Among them is also the **UTF-32** scheme which is defined in
the Unicode standard.

Another set of encoding schemes encode integers on a variable number of bytes.
These include two schemes that are also defined in the Unicode standard, namely
**Utf-8** and **Utf-16**.

Unicode doesn't impose any specific encoding. However, it is most often
associated with one of the Utf encodings. They each have their own properties
and advantages:

*Utf32*

  This is the simplest of all these encodings. It simply encodes all the
  characters on 32 bits (4 bytes). This encodes all the possible characters in
  Unicode, and is obviously straightforward to manipulate. However, given that
  the first 65535 characters in Unicode are enough to encode all known
  languages currently in use, Utf32 is also a waste of space in most cases.

*Utf16*

  For the above reason, Utf16 was defined. Most characters are only encoded on
  two bytes (which is enough for the first 65535 and most current characters).
  In addition, a number of special code points have been defined, known as
  *surrogate pairs*, that make the encoding of integers greater than 65535
  possible. The integers are then encoded on four bytes.  As a result, Utf16 is
  thus much more memory-efficient and requires less space than Utf32 to encode
  sequences of characters. However, it is also more complex to decode.

*Utf8*

  This is an even more space-efficient encoding, but is also more complex to
  decode. More important, it is compatible with the most currently used simple
  8bit encoding.

  Utf8 has the following properties:

  * Characters 0 to 127 (ASCII) are encoded simply as a single byte.
    This means that files and strings which contain only 7-bit ASCII
    characters have the same encoding under both ASCII and UTF-8.

  * Characters greater than 127 are encoded as a sequence of several
    bytes, each of which has the most significant bit set. Therefore,
    no ASCII byte can appear as part of any other character.

  * The first byte of a multibyte sequence that represents a non-ASCII
    character is always in the range 0xC0 to 0xFD and it indicates how
    many bytes follow for this character. All further bytes in a
    multibyte sequence are in the range 0x80 to 0xBF. This allows easy
    resynchronization and makes the encoding stateless and robust
    against missing bytes.

  * UTF-8 encoded characters may theoretically be up to six bytes
    long, however the first 16-bit characters are only up to three bytes
    long.

Note that the encodings above, except for Utf8, have two versions, depending on
the chosen byte order on the machine.

The Ada95 Unicode module provides a set of packages that provide an easy
conversion between all the encoding schemes, as well as basic manipulations of
these byte sequences. These are the **Unicode.CES.*** packages.  Currently,
four encoding schemes are supported, the three Utf schemes and the basic 8bit
encoding which corresponds to the standard Ada strings.

It also supports some routines to convert from one byte-order to another.

The following examples show a possible use of these packages::

  Converting a latin1 string coded on 8 bits to a Utf8 latin2 file
  involves the following steps:

     Latin1 string  (bytes associated with code points in Latin1)
       |    "use Unicode.CES.Basic_8bit.To_Utf32"
       v
     Utf32 latin1 string (contains code points in Latin1)
       |    "Convert argument to To_Utf32 should be
       v         Unicode.CCS.Iso_8859_1.Convert"
     Utf32 Unicode string (contains code points in Unicode)
       |    "use Unicode.CES.Utf8.From_Utf32"
       v
     Utf8 Unicode string (contains code points in Unicode)
       |    "Convert argument to From_Utf32 should be
       v         Unicode.CCS.Iso_8859_2.Convert"
     Utf8 Latin2 string (contains code points in Latin2)
  

Unicode_Encoding
================

XML/Ada groups the two notions of character sets and encoding schemes into a
single type, `Unicode.Encodings.Unicode_Encoding`.

This package provides additional functions to manipulate these encodings, for
instance to retrieve them by the common name that is associated with them (for
instance "utf-8", "iso-8859-15",...), since very often the encoding scheme is
implicit. If you are speaking of utf-8 string, most people always assume you
also use the unicode character set. Likewise, if you are speaking of
"iso-8859-1", most people will assume you string is encoded as 8 byte
characters.

The goal of the `Unicode.Encodings` package is to make these implicit
associations more obvious.

It also provides one additional function `Convert`, which can be used to
convert a sequence of bytes from one encoding to another. This is a convenience
function that you can use when for instance creating DOM trees directly through
Ada calls, since XML/Ada excepts all its strings to be in utf-8 by default.

Misc. functions
===============

The package **Unicode** contains a series of `Is_*` functions, matching the
Unicode standard.

`Is_White_Space`

  Return True if the character argument is a space character, ie a space,
  horizontal tab, line feed or carriage return.

`Is_Letter`

  Return True if the character argument is a letter. This includes the
  standard English letters, as well as some less current cases defined in the
  standard.

`Is_Base_Char`
  Return True if the character is a base character, ie a character whose
  meaning can be modified with a combining character.

`Is_Digit`
  Return True if the character is a digit (numeric character)

`Is_Combining_Char`
  Return True if the character is a combining character. Combining characters
  are accents or other diacritical marks that are added to the previous
  character.

  The most important accented characters, like those used in the orthographies
  of common languages, have codes of their own in Unicode to ensure backwards
  compatibility with older character sets. Accented characters that have their
  own code position, but could also be represented as a pair of another
  character followed by a combining character, are known as precomposed
  characters. Precomposed characters are available in Unicode for backwards
  compatibility with older encodings such as ISO 8859 that had no combining
  characters. The combining character mechanism allows to add accents and other
  diacritical marks to any character

  Note however that your application must provide specific support for
  combining characters, at least if you want to represent them visually.

`Is_Extender`
  True if Char is an extender character.

`Is_Ideographic`
  True if Char is an ideographic character. This is defined only for
  Asian languages.
