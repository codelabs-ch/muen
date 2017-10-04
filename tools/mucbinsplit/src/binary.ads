--
--  Copyright (C) 2017  secunet Security Networks AG
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Bfd.Constants;

package Binary is

   package BC renames Bfd.Constants;

   type Section_Flags is new Bfd.Section_Flags;

   Contents : constant Section_Flags := Section_Flags (BC.SEC_HAS_CONTENTS);
   Alloc    : constant Section_Flags := Section_Flags (BC.SEC_ALLOC);
   Load     : constant Section_Flags := Section_Flags (BC.SEC_LOAD);
   Readonly : constant Section_Flags := Section_Flags (BC.SEC_READONLY);
   Code     : constant Section_Flags := Section_Flags (BC.SEC_CODE);
   Data     : constant Section_Flags := Section_Flags (BC.SEC_DATA);

end Binary;
