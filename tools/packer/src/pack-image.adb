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

with Ada.Streams.Stream_IO;

with SK.Utils;

with Pack.OS;

package body Pack.Image
is

   Objcopy : constant String := "/usr/bin/objcopy";

   -------------------------------------------------------------------------

   procedure Add_Section
     (Image    : String;
      Filename : String;
      Name     : String;
      Address  : SK.Word64)
   is
   begin
      OS.Execute
        (Command => Objcopy & " --add-section ." & Name & "=" & Filename & " "
         & Image & " --change-section-address ." & Name & "=0x"
         & SK.Utils.To_Hex (Item => Address) & " --set-section-flags ."
         & Name & "=alloc 2>/dev/null");
   end Add_Section;

   -------------------------------------------------------------------------

   procedure Parse_Bzimage
     (Src     : String;
      Dst_Bin : String)
   is
      use Ada.Streams.Stream_IO;
      use type SK.Byte;

      type Sector is array (0 .. 511) of SK.Byte;
      pragma Pack (Sector);

      bzImage32BitEntryPoint : constant array (0 .. 25) of SK.Byte :=
          (16#fc#, 16#f6#, 16#86#, 16#11#,
           16#02#, 16#00#, 16#00#, 16#40#,
           16#75#, 16#10#, 16#fa#, 16#b8#,
           16#18#, 16#00#, 16#00#, 16#00#,
           16#8e#, 16#d8#, 16#8e#, 16#c0#,
           16#8e#, 16#e0#, 16#8e#, 16#e8#,
           16#8e#, 16#d0#);
      bzImage64BitEntryPoint : constant array (0 .. 21) of SK.Byte :=
          (16#fc#, 16#f6#, 16#86#, 16#11#,
           16#02#, 16#00#, 16#00#, 16#40#,
           16#75#, 16#0c#, 16#fa#, 16#b8#,
           16#18#, 16#00#, 16#00#, 16#00#,
           16#8e#, 16#d8#, 16#8e#, 16#c0#,
           16#8e#, 16#d0#);

      File_In  : File_Type;
      File_Out : File_Type;

      S             : Sector;
      Match         : Boolean;
      Setup_Sectors : Integer := 4;
   begin
      Open (Name => Src,
            File => File_In,
            Mode => In_File);
      Create (Name => Dst_Bin,
              File => File_Out,
              Mode => Out_File);

      Sector'Read (Stream (File => File_In), S);
      if Natural (S (497)) > 0 then
         Setup_Sectors := Natural (S (497)) + 1;
      end if;
      for I in Integer range 2 .. Setup_Sectors loop
         Sector'Read (Stream (File => File_In), S);
      end loop;

      begin
         Sector'Read (Stream (File => File_In), S);
      exception
         when End_Error => null;
      end;

      --  This is special handling for the bzImage entry point. It tries to
      --  load segment selectors from the GDT, but fails since Muen doesn't
      --  setup that table at all.
      --  On the upside, it configures the segment selectors properly, so we
      --  don't need this here.
      --  Note: There's a Linux bootparams flag "KEEP_SEGMENTS" that would work
      --        similarily. Unfortunately it also disables later segment
      --        configuration code that we _do_ need.
      --  We assume that there are only two entry points, one for each of 32bit
      --  and 64bit mode images.
      Match := True;
      for I in bzImage32BitEntryPoint'Range loop
         if S (I) /= bzImage32BitEntryPoint (I) then
            Match := False;
         end if;
      end loop;
      if Match then
         for I in bzImage32BitEntryPoint'Range loop
            S (I) := 16#90#; --  overwrite with NOP
         end loop;
      end if;
      Match := True;
      for I in bzImage64BitEntryPoint'Range loop
         if S (I) /= bzImage64BitEntryPoint (I) then
            Match := False;
         end if;
      end loop;
      if Match then
         for I in bzImage64BitEntryPoint'Range loop
            S (I) := 16#90#; --  overwrite with NOP
         end loop;
      end if;

      Sector'Write (Stream (File => File_Out), S);

      while not Ada.Streams.Stream_IO.End_Of_File (File_In) loop
         begin
            Sector'Read (Stream (File => File_In), S);
         exception
            when End_Error => null;
         end;
         Sector'Write (Stream (File => File_Out), S);
      end loop;
      Close (File => File_In);
      Close (File => File_Out);
   end Parse_Bzimage;

   -------------------------------------------------------------------------

   procedure To_Binary
     (Src_Elf : String;
      Dst_Bin : String)
   is
   begin
      OS.Execute (Command => Objcopy & " -O binary --set-section-flags "
                  & ".bss=alloc,load,contents " & Src_Elf & " " & Dst_Bin);
   end To_Binary;

end Pack.Image;
