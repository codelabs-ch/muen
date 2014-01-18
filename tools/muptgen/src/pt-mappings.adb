--
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

with Ada.Streams.Stream_IO;

with Mutools.Files;

package body Pt.Mappings
is

   -------------------------------------------------------------------------

   procedure Write_Pagetables
     (Mem_Layout : Memory_Layout_Type;
      Filename   : String)
   is
      use Ada.Streams.Stream_IO;

      File : File_Type;
   begin
      Mutools.Files.Open (Filename => Filename,
                          File     => File);
      Paging.PML4_Table_Type'Write (Stream (File => File), Mem_Layout.PML4);
      Paging.PDP_Table_Type'Write  (Stream (File => File), Mem_Layout.PDPT);
      Paging.PD_Table_Type'Write   (Stream (File => File), Mem_Layout.PD);
      Paging.Page_Table_Type'Write (Stream (File => File), Mem_Layout.PT);
      Close (File => File);
   end Write_Pagetables;

end Pt.Mappings;
