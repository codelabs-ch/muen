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
--  The Bfd package exports the GNU Bfd library found in Binutils
--  and Gdb.  It is not intended to be as complete as the C library
--  but still provide enough methods to read any object or binary,
--  observe its sections, its symbol table.
--
with Interfaces.C;
with Interfaces.C.Strings;
with Bfd.Internal;
with Bfd.Thin;
package body Bfd.Files is

   pragma Linker_Options ("-lbfd" & ASCII.NUL & "-liberty" & Bfd.Constants.LINKER_OPTIONS);
   pragma Linker_Options ("-lz");

   use type System.Address;

   Safe_Mode : constant Boolean := True;

   procedure Check_Bfd (File : in File_Type);
   pragma Inline_Always (Check_Bfd);

   --  -----------------------
   --  Check if the BFD file is valid
   --  Raise USE_ERROR if not
   --  -----------------------
   procedure Check_Bfd (File : in File_Type) is
   begin
      if Safe_Mode and File.Abfd = System.Null_Address then
         raise USE_ERROR;
      end if;
   end Check_Bfd;

   --  -----------------------
   --  Open the file and obtain a bfd handler.
   --  Raises OPEN_ERROR if the file cannot be opened.
   --  -----------------------
   procedure Open (File   : in out File_Type;
                   Name   : in String;
                   Target : in String := "") is

   begin
      if File.Abfd /= System.Null_Address then
         Close (File);
      end if;

      File.Name := Interfaces.C.Strings.New_String (Name);
      if Target = "" then
         File.Abfd := Bfd.Thin.Openr (File.Name, System.Null_Address);
      else
         File.Abfd := Bfd.Thin.Openr (File.Name, System.Null_Address);
      end if;

      if File.Abfd = System.Null_Address then
         Interfaces.C.Strings.Free (File.Name);
         raise OPEN_ERROR;
      end if;
   end Open;

   --  -----------------------
   --  Close the file, releasing any resource allocated for it.
   --  -----------------------
   procedure Close (File : in out File_Type) is
      use type Interfaces.C.Strings.chars_ptr;
   begin
      if File.Abfd /= System.Null_Address then
         Bfd.Thin.Close (File.Abfd);
         File.Abfd := System.Null_Address;
      end if;
      if File.Name /= Interfaces.C.Strings.Null_Ptr then
         Interfaces.C.Strings.Free (File.Name);
      end if;
   end Close;

   --  -----------------------
   --  Check if the file is open.
   --  Returns true if the file is open
   --  -----------------------
   function Is_Open (File : in File_Type) return Boolean is
   begin
      return File.Abfd /= System.Null_Address;
   end Is_Open;

   --  -----------------------
   --  Get the filename that was used to open the file.
   --  -----------------------
   function Get_Filename (File : in File_Type) return String is
   begin
      Check_Bfd (File);
      return Interfaces.C.Strings.Value (Bfd.Thin.Get_Filename (File.Abfd));
   end Get_Filename;

   --  -----------------------
   --  Check if the file is of the specified format.
   --  Returns true if the file is open and of the specified format
   --  -----------------------
   function Check_Format (File   : in File_Type;
                          Expect : in Format) return Boolean is
      use type Interfaces.C.int;

      N : Integer;
   begin
      if File.Abfd = System.Null_Address then
         return False;
      end if;
      case Expect is
         when UNKNOWN =>
            N := 0;

         when OBJECT =>
            N := 1;

         when ARCHIVE =>
            N := 2;

         when others =>
            N := 0;
      end case;

      return Bfd.Thin.Check_Format (File.Abfd, N) /= 0;
   end Check_Format;

   --  -----------------------
   --  Get the BFD file flags.
   --  -----------------------
   function Get_File_Flags (File : in File_Type) return File_Flags is
   begin
      Check_Bfd (File);
      return Bfd.Thin.Get_File_Flags (File.Abfd);
   end Get_File_Flags;

   --  -----------------------
   --  Get the start address.
   --  -----------------------
   function Get_Start_Address (File : in File_Type) return Vma_Type is
   begin
      Check_Bfd (File);
      return Bfd.Thin.Get_Start_Address (File.Abfd);
   end Get_Start_Address;

   --  -----------------------
   --  Return number of symbols.
   --  -----------------------
   function Get_Symbol_Count (File : in File_Type) return Natural is
   begin
      Check_Bfd (File);
      return Bfd.Thin.Get_Symbol_Count (File.Abfd);
   end Get_Symbol_Count;

   --  -----------------------
   --  Get the pointer to the BFD structure allocated for the file.
   --  -----------------------
   function Get_Bfd_Pointer (File : in File_Type) return Ptr is
   begin
      Check_Bfd (File);
      return File.Abfd;
   end Get_Bfd_Pointer;

   overriding
   procedure Finalize (File : in out File_Type) is
   begin
      Close (File);
   end Finalize;

end Bfd.Files;
