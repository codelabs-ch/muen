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

package body Skp.Writers.Cmd_Line
is

   procedure Write
     (Cmdline  : String;
      Filename : String)
   is
      use Ada.Streams.Stream_IO;

      File : Ada.Streams.Stream_IO.File_Type;
   begin
      Open (Filename => Filename,
            File     => File);
      String'Write (Stream (File => File), Cmdline);
      Character'Write (Stream (File => File), Character'Val (0));
      Close (File => File);
   end Write;

end Skp.Writers.Cmd_Line;
