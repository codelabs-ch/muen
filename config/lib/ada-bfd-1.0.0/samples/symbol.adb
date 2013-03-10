-----------------------------------------------------------------------
--  symbol -- Bfd Ada example to load and search a symbol in the symbol table
--  Copyright (C) 2012 Free Software Foundation, Inc.
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
with Ada.Text_IO;
with Ada.Command_Line;

with Bfd;
with Bfd.Files;
with Bfd.Sections;
with Bfd.Symbols;

procedure Symbol is

   use Ada.Text_IO;
   use type Bfd.Symbols.Symbol;
   use type Bfd.Symbol_Flags;

   Count  : constant Natural := Ada.Command_Line.Argument_Count;
begin
   if Count < 2 then
      Ada.Text_IO.Put_Line ("Usage: symbol obj-file symbol ...");
      return;
   end if;

   Bfd.Set_Error_Program_Name (To => "symbol");
   declare
      Path    : constant String := Ada.Command_Line.Argument (1);
      File    : Bfd.Files.File_Type;
      Symbols : Bfd.Symbols.Symbol_Table;
   begin
      Bfd.Files.Open (File, Path, "");
      if Bfd.Files.Check_Format (File, Bfd.Files.OBJECT) then
         Bfd.Symbols.Read_Symbols (File, Symbols);
         for I in 2 .. Count loop
            declare
               Name : constant String := Ada.Command_Line.Argument (I);
               Sym  : constant Bfd.Symbols.Symbol := Bfd.Symbols.Get_Symbol (Symbols, Name);
            begin
               if Sym /= Bfd.Symbols.Null_Symbol then
                  declare
                     Sec   : constant Bfd.Sections.Section := Bfd.Symbols.Get_Section (Sym);
                     Flags : constant Bfd.Symbol_Flags     := Bfd.Symbols.Get_Flags (Sym);
                     Value : constant Bfd.Symbol_Value     := Bfd.Symbols.Get_Value (Sym);
                  begin
                     if Bfd.Sections.Is_Undefined_Section (Sec) then
                        Ada.Text_IO.Put_Line (Name & ": undefined symbol");

                     elsif (Flags and Bfd.Symbols.BSF_GLOBAL) /= 0 then
                        Ada.Text_IO.Put_Line (Name & ": global symbol in section "
                                              & Bfd.Sections.Get_Name (Sec)
                                              & "(" & Bfd.Symbol_Value'Image (Value) & ")");

                     elsif (Flags and Bfd.Symbols.BSF_LOCAL) /= 0 then
                        Ada.Text_IO.Put_Line (Name & ": local symbol in section "
                                              & Bfd.Sections.Get_Name (Sec)
                                              & "(" & Bfd.Symbol_Value'Image (Value) & ")");

                     else
                        Ada.Text_IO.Put_Line (Name & ": other symbol in section "
                                              & Bfd.Sections.Get_Name (Sec)
                                              & "(" & Bfd.Symbol_Value'Image (Value) & ")");
                     end if;
                  end;
               else
                  Ada.Text_IO.Put_Line (Name & ": not found");
               end if;
            end;
         end loop;
      end if;
      Bfd.Files.Close (File);

   exception
      when Bfd.OPEN_ERROR =>
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                               "Cannot open file '" & Path
                               & "': " & Bfd.Get_Error_Message (Bfd.Get_Error));
         Ada.Command_Line.Set_Exit_Status (1);
   end;
end Symbol;
