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

package body Musinfo.Utils
is

   -------------------------------------------------------------------------

   function Concat (L, R : Name_Type) return Name_Type
   is
      LL     : constant Name_Size_Type := L.Length;
      LR     : constant Name_Size_Type := R.Length;
      Result : Name_Type
        := (Length    => LL + LR,
            Padding   => 0,
            Data      => (others => ASCII.NUL),
            Null_Term => ASCII.NUL);
   begin

      --  Avoid requirement for memmove by using explicit loops.

      for I in 1 .. LL loop
         Result.Data (Name_Index_Type (I)) := L.Data (Name_Index_Type (I));
      end loop;

      for I in LL + 1 .. LL + LR loop
         Result.Data (Name_Index_Type (I))
           := R.Data (Name_Index_Type (I - LL));
      end loop;

      return Result;
   end Concat;

   -------------------------------------------------------------------------

   function Create_Resource_Iterator
     (Container : Subject_Info_Type)
      return Resource_Iterator_Type
   is
   begin
      return I : Resource_Iterator_Type do

         --  Subject names are guaranteed to be unique, so link container and
         --  iterator via name data.

         I.Owner := Container.Name;
      end return;
   end Create_Resource_Iterator;

   -------------------------------------------------------------------------

   function Device_Memory_By_Name
     (Sinfo : Subject_Info_Type;
      Name  : Name_Type)
      return Device_Memory_Type
   is
      M : Device_Memory_Type := Null_Device_Memory;
   begin
      Search :
      for R of Sinfo.Resources loop
         if R.Kind = Musinfo.Res_Device_Memory
           and then Names_Equal
             (Left  => Name,
              Right => R.Name)
         then
            M := R.Dev_Mem_Data;
            exit Search;
         end if;
      end loop Search;

      return M;
   end Device_Memory_By_Name;

   -------------------------------------------------------------------------

   function Device_By_SID
     (Sinfo : Subject_Info_Type;
      SID   : SID_Type)
      return Device_Type
   is
      use type SID_Type;

      D : Device_Type := Null_Device;
   begin
      Search :
      for R of Sinfo.Resources loop
         if R.Kind = Musinfo.Res_Device and then R.Dev_Data.SID = SID then
            D := R.Dev_Data;
            exit Search;
         end if;
      end loop Search;

      return D;
   end Device_By_SID;

   -------------------------------------------------------------------------

   function Element
     (Container : Subject_Info_Type;
      Iter      : Resource_Iterator_Type)
      return Resource_Type
   is
   begin
      return Container.Resources (Iter.Resource_Idx);
   end Element;

   -------------------------------------------------------------------------

   function Name_Data_Equal (Left, Right : Name_Data_Type) return Boolean
   is
   begin
      Cmpbyte :
      for I in Left'Range loop
         if Left (I) /= Right (I) then
            return False;
         end if;

         pragma Loop_Invariant
           (for all J in Left'First .. I => Left (J) = Right (J));
      end loop Cmpbyte;

      return True;
   end Name_Data_Equal;

   -------------------------------------------------------------------------

   function Names_Equal (Left, Right : Name_Type) return Boolean
   is
   begin
      return (Left.Length = Right.Length
              and Left.Padding = Right.Padding
              and Name_Data_Equal (Left  => Left.Data,
                                   Right => Right.Data));
   end Names_Equal;

   -------------------------------------------------------------------------

   function Names_Match
     (N1, N2 : Name_Type;
      Count  : Name_Size_Type)
      return Boolean
   is
      Res : Boolean := True;
   begin
      if N1.Length < Count or else N2.Length < Count then
         Res := False;
      else
         if N1.Data (1 .. Natural (Count)) /= N2.Data (1 .. Natural (Count))
         then
            Res := False;
         end if;
      end if;

      return Res;
   end Names_Match;

   -------------------------------------------------------------------------

   function Is_Valid (Sinfo : Subject_Info_Type) return Boolean
   is
      use type Interfaces.Unsigned_64;
   begin
      return Sinfo.Magic = Muen_Subject_Info_Magic;
   end Is_Valid;

   -------------------------------------------------------------------------

   function Memory_By_Hash
     (Sinfo   : Subject_Info_Type;
      Hash    : Hash_Type;
      Content : Content_Type)
      return Memregion_Type
   is
      M : Memregion_Type := Null_Memregion;
   begin
      Search :
      for R of Sinfo.Resources loop
         if R.Kind = Musinfo.Res_Memory
           and then R.Mem_Data.Content = Content
         then
            declare

               --  Stack object required to avoid No_Implicit_Loops violation.

               H : constant Musinfo.Hash_Type
                 := R.Mem_Data.Hash;
            begin
               if H = Hash then
                  M := R.Mem_Data;
                  exit Search;
               end if;
            end;
         end if;
      end loop Search;

      return M;
   end Memory_By_Hash;

   -------------------------------------------------------------------------

   function Memory_By_Kind
     (Sinfo : Subject_Info_Type;
      Kind  : Memory_Kind)
      return Memregion_Type
   is
      M : Memregion_Type := Null_Memregion;
   begin
      Search :
      for R of Sinfo.Resources loop
         if R.Kind = Musinfo.Res_Memory and then R.Mem_Data.Kind = Kind then
            M := R.Mem_Data;
            exit Search;
         end if;
      end loop Search;

      return M;
   end Memory_By_Kind;

   -------------------------------------------------------------------------

   function Memory_By_Name
     (Sinfo : Subject_Info_Type;
      Name  : Name_Type)
      return Memregion_Type
   is
      M : Memregion_Type := Null_Memregion;
   begin
      Search :
      for R of Sinfo.Resources loop
         if R.Kind = Musinfo.Res_Memory
           and then Names_Equal
             (Left  => Name,
              Right => R.Name)
         then
            M := R.Mem_Data;
            exit Search;
         end if;
      end loop Search;

      return M;
   end Memory_By_Name;

   -------------------------------------------------------------------------

   function Memory_Starts_With
     (Sinfo : Subject_Info_Type;
      Name  : Name_Type)
      return Memregion_Type
   is
      M : Memregion_Type := Null_Memregion;
   begin
      Search :
      for R of Sinfo.Resources loop
         if R.Kind = Musinfo.Res_Memory
           and then Names_Match
             (N1    => R.Name,
              N2    => Name,
              Count => Name.Length)
         then
            M := R.Mem_Data;
            exit Search;
         end if;
      end loop Search;

      return M;
   end Memory_Starts_With;

   -------------------------------------------------------------------------

   procedure Next (Iter : in out Resource_Iterator_Type)
   is
   begin
      if Iter.Owner /= Null_Name and then
        Iter.Resource_Idx < Musinfo.Resource_Index_Type'Last
      then
         Iter.Resource_Idx := Iter.Resource_Idx + 1;
      else
         Iter.Done := True;
      end if;
   end Next;

   -------------------------------------------------------------------------

   function To_Name (Str : String) return Name_Type
   is
      N : Name_Type := (Length    => Str'Length,
                        Padding   => 0,
                        Data      => (others => ASCII.NUL),
                        Null_Term => ASCII.NUL);
   begin
      for I in 1 .. N.Length loop
         N.Data (Name_Index_Type (I)) := Str
           (Str'First + Name_Index_Type (I) - 1);
      end loop;

      return N;
   end To_Name;

end Musinfo.Utils;
