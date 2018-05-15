--
--  Copyright (C) 2018  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2018  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

pragma Warnings (Off);
with System.Mmap.Unix;
pragma Warnings (On);

with GNAT.OS_Lib;

with Ada.Directories;

with Interfaces.C;

with Mulog;
with Muchannel;
with Muchannel.Readers;
with Muchannel_Constants;

package body Shmemlog
is

   subtype Data_Index is Natural range 1 .. 64;

   type Data_Type is array (Data_Index) of Character;

   Null_Data : constant Data_Type := (others => ASCII.NUL);

   MAP_FAILED : constant System.Address := System'To_Address (-1);

   -------------------------------------------------------------------------

   procedure Read (Input, Output : String)
   is
      use type System.Address;
      use type GNAT.OS_Lib.File_Descriptor;

      Fd_I, Fd_O : GNAT.OS_Lib.File_Descriptor;
      Addr       : System.Address;
      Size       : Ada.Directories.File_Size;
   begin
      Fd_I := GNAT.OS_Lib.Open_Read
        (Name  => Input,
         Fmode => GNAT.OS_Lib.Binary);
      if Fd_I = GNAT.OS_Lib.Invalid_FD then
         raise Open_Error with "Unable to open input file '" & Input & "'";
      end if;

      Size := Ada.Directories.Size (Name => Input);
      Addr := System.Mmap.Unix.Mmap
        (Length => Interfaces.C.size_t (Size),
           Fd     => Fd_I,
           Offset => 0);
      if Addr = MAP_FAILED then
         raise Open_Error with "Unable to map input file '" & Input & "'";
      end if;

      Fd_O := GNAT.OS_Lib.Create_New_File
        (Name  => Output,
         Fmode => GNAT.OS_Lib.Binary);
      if Fd_O = GNAT.OS_Lib.Invalid_FD then
         raise Open_Error with "Unable to create output file '" & Output & "'";
      end if;

      Mulog.Log (Msg => "Files successfuly opened, watch '"
                 & Output & "' for data");

      declare
         package Stream is new Muchannel
           (Element_Type => Data_Type,
            Elements     =>
              (Positive (Size)
               - Muchannel_Constants.Header_Size) / (Data_Type'Size / 8),
            Null_Element => Null_Data,
            Protocol     => 16#6d3a_cd5d_ced2_3445#);

         Channel : Stream.Channel_Type
         with
            Address => Addr;

         package Readers is new Stream.Readers;
         use type Readers.Result_Type;

         Rdr   : Readers.Reader_Type;
         Res   : Readers.Result_Type;
         Data  : Data_Type;
         Dummy : Integer;
      begin
         loop
            Readers.Read (Channel => Channel,
                          Reader  => Rdr,
                          Element => Data,
                          Result  => Res);

            case Res is
               when Readers.Incompatible_Interface =>
                  raise Open_Error with "Given file is no shmstream";
               when Readers.Epoch_Changed =>
                  Mulog.Log (Msg => "Epoch changed");
               when Readers.No_Data =>
                  delay 2.0;
               when Readers.Overrun_Detected =>
                  Mulog.Log
                    (Msg => "Overrrun detected, some data might be lost");
               when Readers.Inactive =>
                  Mulog.Log (Msg => "Channel inactive, waiting");
                  delay 2.0;
               when Readers.Success =>
                  Dummy := GNAT.OS_Lib.Write
                    (FD => Fd_O,
                     A  => Data'Address,
                     N  => Data_Type'Size / 8);
            end case;
         end loop;
      end;

   exception
      when others =>
         GNAT.OS_Lib.Close (FD => Fd_I);
         GNAT.OS_Lib.Close (FD => Fd_O);
         raise;
   end Read;

end Shmemlog;
