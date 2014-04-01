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

with Ada.Directories;
with Ada.Text_IO.Text_Streams;

with DOM.Core.Nodes;

with Ahven;

package body Test_Utils.Expander
is

   -------------------------------------------------------------------------

   procedure Run_Test
     (Filename     : String;
      Ref_Filename : String;
      Pre          : Process_Policy := Process_Nil'Access;
      Expander     : Process_Policy)
   is
      Output_File : Ada.Text_IO.File_Type;
      Policy      : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Pre (Data => Policy);

      Expander (Data => Policy);

      Ada.Text_IO.Create
        (File => Output_File,
         Mode => Ada.Text_IO.Out_File,
         Name => Filename);
      DOM.Core.Nodes.Write
        (Stream       => Ada.Text_IO.Text_Streams.Stream (Output_File),
         N            => Policy.Doc,
         Pretty_Print => True);
      Ada.Text_IO.Close (File => Output_File);

      Ahven.Assert (Condition => Test_Utils.Equal_Files
                    (Filename1 => Filename,
                     Filename2 => Ref_Filename),
                    Message   => "Policy mismatch");

      Ada.Directories.Delete_File (Name => Filename);
   end Run_Test;

end Test_Utils.Expander;
