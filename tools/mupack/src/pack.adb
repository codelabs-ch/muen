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

with Ada.Streams;

with Interfaces;

with Muxml;
with Mulog;
with Mutools.Utils;

with Pack.Image;
with Pack.Command_Line;
with Pack.Content_Providers;
with Pack.Utils;
with Pack.Checks;
with Pack.Manifest;

package body Pack
is

   -------------------------------------------------------------------------

   procedure Run
   is
      Out_Dir     : constant String := Command_Line.Get_Output_Dir;
      In_Dir      : constant String := Command_Line.Get_Input_Dir;
      Policy_File : constant String := Command_Line.Get_Policy;
      Policy      : Muxml.XML_Data_Type;
   begin
      Mulog.Log (Msg => "Looking for input files in '" & In_Dir & "'");
      Mulog.Log (Msg => "Using output directory '" & Out_Dir & "'");
      Mulog.Log (Msg => "Processing policy '" & Policy_File & "'");

      Checks.Register_All;
      Mulog.Log (Msg => "Registered checker(s)" & Checks.Get_Count'Img);
      Content_Providers.Register_All;
      Mulog.Log (Msg => "Registered content provider(s)"
                 & Content_Providers.Get_Count'Img);

      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => Policy_File);

      Checks.Run (Data => Policy);

      Pack_Image :
      declare
         Size   : constant Interfaces.Unsigned_64
           := Utils.Get_Image_Size (Policy => Policy);
         Sysimg : constant String := Out_Dir & "/muen.img";
         Mfest  : constant String := Out_Dir & "/muen.img.manifest";
         Data   : Content_Providers.Param_Type
           (Ada.Streams.Stream_Element_Offset (Size));
      begin
         Data.Mmap_File := U (Out_Dir & "/mmap");
         Data.XML_Doc   := Policy.Doc;

         Content_Providers.Run (Data => Data);

         Image.Write (Image    => Data.Image,
                      Filename => Sysimg);
         Mulog.Log (Msg => "Successfully created system image '" & Sysimg
                    & "' of size " & Mutools.Utils.To_Hex (Number => Size)
                    & " bytes");

         Manifest.Write (Manifest => Data.Manifest,
                         Filename => Mfest);
         Mulog.Log (Msg => "Manifest of system image '" & Sysimg
                    & "' written to '" & Mfest & "'");
      end Pack_Image;
   end Run;

end Pack;
