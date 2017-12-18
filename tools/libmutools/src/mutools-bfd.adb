--
--  Copyright (C) 2017  secunet Security Networks AG
--  Copyright (C) 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package body Mutools.Bfd
is

   --------------------------------------------------------------------------

   procedure Open
     (Filename   :     String;
      Descriptor : out Standard.Bfd.Files.File_Type)
   is
   begin
      begin
         Standard.Bfd.Files.Open (File => Descriptor, Name => Filename);
      exception
         when Standard.Bfd.OPEN_ERROR =>
            raise ELF_Error
              with "Unable to open file '" & Filename & "'";
      end;

      if not Standard.Bfd.Files.Check_Format
        (File   => Descriptor,
         Expect => Standard.Bfd.Files.OBJECT)
      then
         raise ELF_Error
           with "File '" & Filename & "' is not a binary object file";
      end if;
   end Open;

end Mutools.Bfd;
