--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with Ada.Exceptions;

with Input_Sources.Strings;
with Schema.Schema_Readers;
with Unicode.CES.Utf8;

with Muxml.Schemas;

package body Muxml.Grammar
is

   Current_Grammar : Schema.Validators.XML_Grammar
     := Schema.Validators.No_Grammar;

   -------------------------------------------------------------------------

   function Get_Grammar return Schema.Validators.XML_Grammar
   is
      use type Schema.Validators.XML_Grammar;

      Reader    : Schema.Schema_Readers.Schema_Reader;
      Str_Input : Input_Sources.Strings.String_Input;
   begin
      if Current_Grammar = Schema.Validators.No_Grammar then
         Str_Input.Set_Public_Id (Id => Schemas.XSD_Id);
         Input_Sources.Strings.Open
           (Str      => Schemas.XSD,
            Encoding => Unicode.CES.Utf8.Utf8_Encoding,
            Input    => Str_Input);

         begin
            Schema.Schema_Readers.Parse
              (Parser => Reader,
               Input  => Str_Input);

         exception
            when others =>
               Input_Sources.Strings.Close (Input => Str_Input);
               Reader.Free;
               raise;
         end;

         Input_Sources.Strings.Close (Input => Str_Input);
         Current_Grammar := Reader.Get_Grammar;
      end if;

      return Current_Grammar;

   exception
      when Schema.Validators.XML_Validation_Error =>
         raise Processing_Error with "XML validation error - "
           & Reader.Get_Error_Message;
      when E : others =>
         raise Processing_Error with "Error reading XSD schema '"
           & Schemas.XSD_Id & "' - "
           & Ada.Exceptions.Exception_Message (X => E);
   end Get_Grammar;

end Muxml.Grammar;
