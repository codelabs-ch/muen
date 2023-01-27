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

with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Command_Line;

with Mutime.Reference_Data;

procedure Fuzzer
is
   use Mutime;

   procedure Print_Mismatch
     (Operation    : String;
      D_Ref, D_Res : Date_Time_Type;
      T_Ref, T_Res : Timestamp_Type);

   -------------------------------------------------------------------------

   procedure Print_Mismatch
     (Operation    : String;
      D_Ref, D_Res : Date_Time_Type;
      T_Ref, T_Res : Timestamp_Type)
   is

      function To_String (Date : Date_Time_Type) return String;

      ----------------------------------------------------------------------

      function To_String (Date : Date_Time_Type) return String
      is
         use Ada.Strings;

         Y   : constant String := Fixed.Trim
           (Source => Date.Year'Img,
            Side   => Ada.Strings.Left);
         M   : constant String := Fixed.Trim
           (Source => Date.Month'Img,
            Side   => Ada.Strings.Left);
         D   : constant String := Fixed.Trim
           (Source => Date.Day'Img,
            Side   => Ada.Strings.Left);
         H   : constant String := Fixed.Trim
           (Source => Date.Hour'Img,
            Side   => Ada.Strings.Left);
         Min : constant String := Fixed.Trim
           (Source => Date.Minute'Img,
            Side   => Ada.Strings.Left);
         Sec : constant String := Fixed.Trim
           (Source => Date.Second'Img,
            Side   => Ada.Strings.Left);
      begin
         return D & "." & M & "." & Y & "-" & H & ":" & Min & ":" & Sec;
      end To_String;
   begin
      Ada.Text_IO.Put_Line (Operation & " mismatch");
      Ada.Text_IO.Put_Line ("Reference" & T_Ref'Img
                            & " " & To_String (Date => D_Ref));
      Ada.Text_IO.Put_Line ("Result   " & T_Res'Img
                            & " " & To_String (Date => D_Res));
   end Print_Mismatch;

   T_Ref, T_Res : Timestamp_Type;
   D_Ref, D_Res : Date_Time_Type;
begin
   loop
      Reference_Data.Generate (Timestamp => T_Ref,
                               Date_Time => D_Ref);

      Split (Timestamp => T_Ref,
             Date_Time => D_Res);
      T_Res := Time_Of (Date_Time => D_Ref);

      if D_Res /= D_Ref then
         Print_Mismatch (Operation => "Split",
                         D_Ref     => D_Ref,
                         D_Res     => D_Res,
                         T_Ref     => T_Ref,
                         T_Res     => T_Res);
         Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
         return;
      end if;

      if T_Res /= T_Ref then
         Print_Mismatch (Operation => "Time_Of",
                         D_Ref     => D_Ref,
                         D_Res     => D_Res,
                         T_Ref     => T_Ref,
                         T_Res     => T_Res);
         Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
         return;
      end if;

      Ada.Text_IO.Put_Line ("OK :" & T_Res'Img);
   end loop;
end Fuzzer;
