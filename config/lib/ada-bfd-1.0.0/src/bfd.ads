-----------------------------------------------------------------------
--  BFD -- Binary File Descriptor Library (Ada Interface)
--  Copyright (C) 2001, 2002, 2003, 2004, 2012 Free Software Foundation, Inc.
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  This file is part of BfdAda.
--
--  This program is free software; you can redistribute it and/or
--  modify it under the terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2,
--  or (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation,51 Franklin Street - Fifth Floor,
--  Boston, MA 02110-1301, USA.
-----------------------------------------------------------------------
with System;
with Interfaces;

--  The Bfd package exports the GNU Bfd library found in Binutils
--  and Gdb.  It is not intended to be as complete as the C library
--  but still provide enough methods to read any object or binary,
--  observe its sections, its symbol table.
package Bfd is


   type File_Flags is new Interfaces.Unsigned_32;
   --  Values that may appear in the flags field of a BFD.  These also
   --  appear in the object_flags field of the bfd_target structure, where
   --  they indicate the set of flags used by that backend (not all flags
   --  are meaningful for all object file formats) (FIXME: at the moment,
   --  the object_flags values have mostly just been copied from backend
   --  to another, and are not necessarily correct).

   type Section_Flags is new Interfaces.Unsigned_32;

   type Symbol_Flags is new Interfaces.Unsigned_32;

   type Unsigned_64 is new Interfaces.Unsigned_64;

   type Integer_64 is new Interfaces.Integer_64;

   subtype Vma_Type is Unsigned_64;
   --  The bfd_vma used to represent an address.

   subtype Lma_Type is Vma_Type;
   --  Likewise for lma.

   subtype Size_Type is Unsigned_64;
   --  Used to represent the size of a section.

   subtype Symbol_Value is Unsigned_64;
   --  Used to represent the value of a symbol.

   type Offset_Type is new Integer_64;

   OPEN_ERROR : exception;
   USE_ERROR  : exception;
   NOT_FOUND  : exception;

   ----------------------
   -- General          --
   ----------------------
   type Error is (NO_ERROR,
                  SYSTEM_CALL,
                  INVALID_TARGET,
                  WRONG_FORMAT,
                  INVALID_OPERATION,
                  NO_MEMORY,
                  NO_SYMBOLS,
                  NO_ARMAP,
                  NO_MORE_ARCHIVED_FILES,
                  MALFORMED_ARCHIVE,
                  FILE_NOT_RECOGNIZED,
                  FILE_AMBIGUOUSLY_RECOGNIZED,
                  NO_CONTENTS,
                  NONREPRESENTABLE_SECTION,
                  NO_DEBUG_SECTION,
                  BAD_VALUE,
                  FILE_TRUNCATED,
                  FILE_TOO_BIG,
                  INVALID_ERROR_CODE);

   type Error_Handler is access procedure (Message : in String);
   --  The error handler is a procedure called when BFD functions
   --  want to report an error message.  In the C version, the handler
   --  has a printf-like signature, thus giving freedom for the
   --  formatting.  Here, the message is formatted and passed in Message.
   --
   --  @param Message the message to report
   pragma Convention (C, Error_Handler);

   --  Return the current error code.
   function Get_Error return Error;

   --  Set the current error code.
   procedure Set_Error (To : in Error);

   function Get_Error_Message (Code : in Error) return String;
   --  Return an error message corresponding to the last error
   --  This is equivalent to the C <tt>bfd_errmsg</tt>.
   --
   --  @param Code the error code
   --  @return the error message corresponding to the error code

   --  Set the program name in the BFD library.
   procedure Set_Error_Program_Name (To : in String);

   --  Set a new error handler in BFD library.
   procedure Set_Error_Handler (To  : in Error_Handler;
                                Old : out Error_Handler);

   subtype Ptr is System.Address;
private
   subtype Pointer is System.Address;

   type Bfd_Ptr is new System.Address;

end Bfd;
