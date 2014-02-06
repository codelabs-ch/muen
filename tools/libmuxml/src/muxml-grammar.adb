--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--  Copyright (C) 2014  Alexander Senier <mail@senier.net>
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

with Muxml.Format_src_Schema;
with Muxml.Format_a_Schema;
with Muxml.Format_b_Schema;

package body Muxml.Grammar
is

   -------------------------------------------------------------------------

   function Get_Grammar
     (Kind : Schema_Kind)
      return Schema.Validators.XML_Grammar
   is
      use type Schema.Validators.XML_Grammar;

      Reader    : Schema.Schema_Readers.Schema_Reader;
      Str_Input : Input_Sources.Strings.String_Input;
   begin
      case Kind is
         when Format_Src =>
            Str_Input.Set_Public_Id (Id => Format_src_Schema.XSD_Id);
            Input_Sources.Strings.Open
              (Str      => Format_src_Schema.XSD,
               Encoding => Unicode.CES.Utf8.Utf8_Encoding,
               Input    => Str_Input);
         when Format_A =>
            Str_Input.Set_Public_Id (Id => Format_a_Schema.XSD_Id);
            Input_Sources.Strings.Open
              (Str      => Format_a_Schema.XSD,
               Encoding => Unicode.CES.Utf8.Utf8_Encoding,
               Input    => Str_Input);
         when Format_B =>
            Str_Input.Set_Public_Id (Id => Format_b_Schema.XSD_Id);
            Input_Sources.Strings.Open
              (Str      => Format_b_Schema.XSD,
               Encoding => Unicode.CES.Utf8.Utf8_Encoding,
               Input    => Str_Input);
      end case;

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

      return Reader.Get_Grammar;

   exception
      when Schema.Validators.XML_Validation_Error =>
         raise Processing_Error with "XML validation error - "
           & Reader.Get_Error_Message;
      when E : others =>
         raise Processing_Error with "Error reading " & Kind'Img
           & " XSD - " & Ada.Exceptions.Exception_Message (X => E);
   end Get_Grammar;

end Muxml.Grammar;
