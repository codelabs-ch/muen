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
with Mutools.XML_Utils;
with Mutools.Image;

with Pack.Content_Providers;
with Pack.Pre_Checks;
with Pack.Post_Checks;
with Pack.Manifest;

package body Pack
is

   -------------------------------------------------------------------------

   procedure Run
     (Policy_File    : String;
      Input_Dir      : String;
      Output_Dir     : String;
      Output_Imgname : String;
      Dry_Run        : Boolean)
   is
      Policy : Muxml.XML_Data_Type;
   begin
      Mulog.Log (Msg => "Looking for input files in '" & Input_Dir & "'");
      Mulog.Log (Msg => "Using output directory '" & Output_Dir & "'");
      Mulog.Log (Msg => "Processing policy '" & Policy_File & "'");

      Pre_Checks.Register_All;
      Mulog.Log (Msg => "Registered pre-check(s)" & Pre_Checks.Get_Count'Img);
      Post_Checks.Register_All;
      Mulog.Log (Msg => "Registered post-check(s)"
                 & Post_Checks.Get_Count'Img);
      Content_Providers.Register_All;
      Mulog.Log (Msg => "Registered content provider(s)"
                 & Content_Providers.Get_Count'Img);

      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => Policy_File);

      Pre_Checks.Run (Data      => Policy,
                      Input_Dir => Input_Dir);

      Pack_Image :
      declare
         use type Interfaces.Unsigned_64;

         Size   : constant Interfaces.Unsigned_64
           := Mutools.XML_Utils.Get_Image_Size (Policy => Policy);
         Sysimg : constant String := Output_Dir & "/" & Output_Imgname;
         Mfest  : constant String := Sysimg & ".manifest";
      begin
         if Size = 0 then
            raise Pack_Error with "Image size is zero, no content to pack";
         end if;

         declare
            Data : Content_Providers.Param_Type
              (End_Address => Ada.Streams.Stream_Element_Offset (Size - 1),
               Dry_Run     => Dry_Run);
         begin
            Data.XML_Doc := Policy.Doc;

            Content_Providers.Set_Input_Directory (Dir => Input_Dir);
            Content_Providers.Run (Data => Data);

            Post_Checks.Run (Data => Data);

            Mutools.Image.Write (Image    => Data.Image,
                                 Filename => Sysimg);
            Mulog.Log (Msg => "Successfully created system image '" & Sysimg
                       & "' with end address " & Mutools.Utils.To_Hex
                         (Number => Size));

            Manifest.Write (Manifest => Data.Manifest,
                            Filename => Mfest);
            Mulog.Log (Msg => "Manifest of system image '" & Sysimg
                       & "' written to '" & Mfest & "'");
         end;

         Pre_Checks.Clear;
      end Pack_Image;
   end Run;

end Pack;
