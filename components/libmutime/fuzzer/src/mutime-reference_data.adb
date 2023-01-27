--
--  Copyright (C) 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--    * Redistributions of source code must retain the above copyright notice,
--      this list of conditions and the following disclaimer.
--
--    * Redistributions in binary form must reproduce the above copyright
--      notice, this list of conditions and the following disclaimer in the
--      documentation and/or other materials provided with the distribution.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
--  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
--  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
--  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
--  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
--  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
--  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
--  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--  POSSIBILITY OF SUCH DAMAGE.
--

with Ada.Sequential_IO;
with Ada.Numerics.Discrete_Random;

with GNAT.OS_Lib;
with GNAT.Expect;
with GNAT.String_Split;

package body Mutime.Reference_Data
is

   Random_Source : constant String := "/dev/urandom";

   package S_IO is new Ada.Sequential_IO
     (Element_Type => Integer);

   subtype Random_T_Type is
     Timestamp_Type range 0 .. Timestamp_Type'Last / 10 ** 6;

   package Random_Timestamps is new Ada.Numerics.Discrete_Random
     (Result_Subtype => Random_T_Type);

   Generator : Random_Timestamps.Generator;

   Random_Source_Error : exception;

   --  Return random integer value.
   function Get_Random return Integer;

   --  Execute the date command using the given timestamp.
   function Exec_Date_Cmd (T : Random_T_Type) return String;

   --  Convert given string to date/time.
   function To_Date_Time (Str : String) return Date_Time_Type;

   -------------------------------------------------------------------------

   function Exec_Date_Cmd (T : Random_T_Type) return String
   is
      Args : GNAT.OS_Lib.Argument_List (1 .. 4);
      Res  : aliased Integer;
   begin
      Args (1) := new String'("-u");
      Args (2) := new String'("+%Y;%m;%d;%H;%M;%S");
      Args (3) := new String'("-d");
      Args (4) := new String'("@" & T'Img);

      declare
         Cmd_Output : constant String
           := GNAT.Expect.Get_Command_Output
             (Command    => "date",
              Arguments  => Args,
              Input      => "",
              Status     => Res'Access,
              Err_To_Out => True);
      begin
         for A in Args'Range loop
            GNAT.OS_Lib.Free (X => Args (A));
         end loop;

         return Cmd_Output;
      end;
   end Exec_Date_Cmd;

   -------------------------------------------------------------------------

   procedure Generate
     (Timestamp : out Timestamp_Type;
      Date_Time : out Date_Time_Type)
   is
      T : constant Random_T_Type
        := Random_Timestamps.Random (Gen => Generator);
   begin
      Timestamp := T * 10 ** 6;
      Date_Time := To_Date_Time (Str => Exec_Date_Cmd (T => T));
   end Generate;

   -------------------------------------------------------------------------

   function Get_Random return Integer
   is
      use S_IO;

      Input_File : File_Type;
      Random     : Integer := 0;
   begin
      begin
         Open (File => Input_File,
               Mode => In_File,
               Name => Random_Source,
               Form => "shared=yes");

      exception
         when others =>
            raise Random_Source_Error with "Could not open random source "
              & Random_Source;
      end;

      Read (File => Input_File,
            Item => Random);

      Close (File => Input_File);

      return Random;
   end Get_Random;

   -------------------------------------------------------------------------

   function To_Date_Time (Str : String) return Date_Time_Type
   is
      Subs : GNAT.String_Split.Slice_Set;
   begin
      GNAT.String_Split.Create
        (S          => Subs,
         From       => Str,
         Separators => ";",
         Mode       => GNAT.String_Split.Multiple);

      return D : Date_Time_Type := Epoch do
         D.Year := Year_Type'Value
           (GNAT.String_Split.Slice (S     => Subs,
                                     Index => 1));
         D.Month := Month_Type'Value
           (GNAT.String_Split.Slice (S     => Subs,
                                     Index => 2));
         D.Day := Day_Type'Value
           (GNAT.String_Split.Slice (S     => Subs,
                                     Index => 3));
         D.Hour := Hour_Type'Value
           (GNAT.String_Split.Slice (S     => Subs,
                                     Index => 4));
         D.Minute := Minute_Type'Value
           (GNAT.String_Split.Slice (S     => Subs,
                                     Index => 5));
         D.Second := Second_Type'Value
           (GNAT.String_Split.Slice (S     => Subs,
                                     Index => 6));
      end return;
   end To_Date_Time;

begin
   Random_Timestamps.Reset
     (Gen       => Generator,
      Initiator => Get_Random);
end Mutime.Reference_Data;
