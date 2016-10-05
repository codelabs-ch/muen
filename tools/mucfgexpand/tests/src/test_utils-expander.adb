--
--  Copyright (C) 2014, 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014, 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Strings.Fixed;
with Ada.Directories;
with Ada.Text_IO.Text_Streams;

with DOM.Core.Nodes;

with AUnit.Assertions;

with Mutools.OS;

package body Test_Utils.Expander
is

   use AUnit.Assertions;

   --  Create side-by-side diff with two lines of context per change.
   Diff_Width     : constant := 300;
   Diff_Cmd_Start : constant String := "diff -y -W" & Diff_Width'Img
     & " -L a -L b -t";
   Diff_Cmd_End   : constant String := " | grep -C2 -E '^.{"
     & Ada.Strings.Fixed.Trim (Source => Positive'Image (Diff_Width / 2 - 1),
                               Side   => Ada.Strings.Left)
     & "}[|<>]( |$)'";

   -------------------------------------------------------------------------

   procedure Run_Test
     (Policy_Filename : String            := "data/test_policy.xml";
      Policy_Format   : Muxml.Schema_Kind := Muxml.Format_Src;
      Filename        : String;
      Ref_Diff        : String;
      Pre             : Process_Policy    := Process_Nil'Access;
      Expander        : Process_Policy)
   is
      Output_File : Ada.Text_IO.File_Type;
      Policy      : Muxml.XML_Data_Type;
      Diff        : constant String := Filename & ".diff";
      Cmd         : constant String := Diff_Cmd_Start & " " & Policy_Filename
        & " " & Filename & Diff_Cmd_End & " > " & Diff & " || true";
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Policy_Format,
                   File => Policy_Filename);
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

      Mutools.OS.Execute (Command => Cmd);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Diff,
               Filename2 => Ref_Diff),
              Message   => "Policy mismatch");

      Ada.Directories.Delete_File (Name => Filename);
      Ada.Directories.Delete_File (Name => Diff);
   end Run_Test;

end Test_Utils.Expander;
