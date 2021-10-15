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

with Ada.IO_Exceptions;
with Ada.Streams.Stream_IO;

with Interfaces;

with Mulog;
with Mutools.Files;

package body Bzpatch
is

   type Byte_Array is array (Natural range <>) of Interfaces.Unsigned_8;
   pragma Pack (Byte_Array);

   subtype Sector_Type is Byte_Array (0 .. 511);

   bzImage32BitEntryPoint : constant Byte_Array (0 .. 25) :=
     (16#fc#, 16#f6#, 16#86#, 16#11#,
      16#02#, 16#00#, 16#00#, 16#40#,
      16#75#, 16#10#, 16#fa#, 16#b8#,
      16#18#, 16#00#, 16#00#, 16#00#,
      16#8e#, 16#d8#, 16#8e#, 16#c0#,
      16#8e#, 16#e0#, 16#8e#, 16#e8#,
      16#8e#, 16#d0#);
   bzImage64BitEntryPoint : constant Byte_Array (0 .. 21) :=
     (16#fc#, 16#f6#, 16#86#, 16#11#,
      16#02#, 16#00#, 16#00#, 16#40#,
      16#75#, 16#0c#, 16#fa#, 16#b8#,
      16#18#, 16#00#, 16#00#, 16#00#,
      16#8e#, 16#d8#, 16#8e#, 16#c0#,
      16#8e#, 16#d0#);

   -------------------------------------------------------------------------

   procedure Patch (Input, Output : String)
   is
      use Ada.Streams.Stream_IO;

      Sector        : Sector_Type;
      Setup_Sectors : Integer := 4;
      File_In       : File_Type;
      File_Out      : File_Type;
   begin
      Open (Name => Input,
            File => File_In,
            Mode => In_File);

      begin
         Sector_Type'Read (Stream (File => File_In), Sector);
         if Natural (Sector (497)) > 0 then
            Setup_Sectors := Natural (Sector (497)) + 1;
         end if;
         for I in Integer range 2 .. Setup_Sectors loop
            Sector_Type'Read (Stream (File => File_In), Sector);
         end loop;

      exception
         when Ada.IO_Exceptions.End_Error =>
            Close (File => File_In);
            raise Patch_Error with "Unexpected file layout in bzImage '"
              & Input & "'";
      end;

      begin
         Sector_Type'Read (Stream (File => File_In), Sector);
      exception
         when End_Error => null;
      end;

      --  This is special handling for the bzImage entry point. It tries to
      --  load segment selectors from the GDT, but fails since Muen doesn't
      --  setup that table at all.
      --  On the upside, it configures the segment selectors properly, so we
      --  don't need this here.
      --  Note: There's a Linux bootparams flag "KEEP_SEGMENTS" that would work
      --        similarly. Unfortunately it also disables later segment
      --        configuration code that we _do_ need.
      --  We assume that there are only two entry points, one for each of 32bit
      --  and 64bit mode images.

      if Sector (bzImage32BitEntryPoint'Range) = bzImage32BitEntryPoint then

         --  Overwrite start of 32-bit bzImage entry point with NOP.

         Mulog.Log (Msg => "Patching 32-bit entry point in '" & Input & "'");
         Sector (bzImage32BitEntryPoint'Range) := (others => 16#90#);
      elsif Sector (bzImage64BitEntryPoint'Range) = bzImage64BitEntryPoint then

         --  Overwrite start of 64-bit bzImage entry point with NOP.

         Mulog.Log (Msg => "Patching 64-bit entry point in '" & Input & "'");
         Sector (bzImage64BitEntryPoint'Range) := (others => 16#90#);
      else
         Close (File => File_In);
         raise Patch_Error with "Unable to find entry point in bzImage '"
           & Input & "'";
      end if;

      Mutools.Files.Open (Filename => Output,
                          File     => File_Out);
      Sector_Type'Write (Stream (File => File_Out), Sector);

      while not End_Of_File (File_In) loop
         begin
            Sector_Type'Read (Stream (File => File_In), Sector);
         exception
            when End_Error => null;
         end;
         Sector_Type'Write (Stream (File => File_Out), Sector);
      end loop;
      Close (File => File_In);
      Close (File => File_Out);
   end Patch;

end Bzpatch;
