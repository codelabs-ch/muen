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

   function Concat (L , R : Name_Type) return Name_Type
   is
      LL     : constant Name_Size_Type := L.Length;
      LR     : constant Name_Size_Type := R.Length;
      Result : Name_Type
        := (Length  => LL + LR,
            Padding => 0,
            Data    => (others => ASCII.NUL));
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

   function Create_Memory_Iterator
     (Container : Subject_Info_Type)
      return Memory_Iterator_Type
   is
      I : Memory_Iterator_Type;
   begin
      if Container.Resource_Count > No_Resource then
         I.Resource_Idx := Resource_Index_Type'First;
      end if;

      --  Subject names are guaranteed to be unique, so link container and
      --  iterator via name data.

      I.Owner := Container.Name;

      return I;
   end Create_Memory_Iterator;

   -------------------------------------------------------------------------

   function Device_By_SID
     (Sinfo : Subject_Info_Type;
      SID   : SID_Type)
      return Dev_Info_Type
   is
      use type SID_Type;

      D : Dev_Info_Type := Null_Dev_Info;
   begin
      Search :
      for I in 1 .. Sinfo.Dev_Info_Count loop
         if Sinfo.Dev_Info (I).SID = SID then
            D := Sinfo.Dev_Info (I);
            exit Search;
         end if;
      end loop Search;

      return D;
   end Device_By_SID;

   -------------------------------------------------------------------------

   function Element
     (Container : Subject_Info_Type;
      Iter      : Memory_Iterator_Type)
      return Named_Memregion_Type
   is
      M : Named_Memregion_Type := Null_Named_Memregion;
   begin
      if Iter.Resource_Idx /= No_Resource then
         declare
            Resource : constant Resource_Type
              := Container.Resources (Iter.Resource_Idx);
         begin
            if Resource.Memregion_Idx /= No_Resource then
               M.Name := Resource.Name;
               M.Data := Container.Memregions (Resource.Memregion_Idx);
            end if;
         end;
      end if;

      return M;
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
      for I in 1 .. Sinfo.Memregion_Count loop
         if Sinfo.Memregions (I).Hash = Hash
           and then Sinfo.Memregions (I).Content = Content
         then
            M := Sinfo.Memregions (I);
            exit Search;
         end if;
      end loop Search;

      return M;
   end Memory_By_Hash;

   -------------------------------------------------------------------------

   function Memory_By_Name
     (Sinfo : Subject_Info_Type;
      Name  : Name_Type)
      return Memregion_Type
   is
      M : Memregion_Type := Null_Memregion;
   begin
      Search :
      for I in 1 .. Sinfo.Resource_Count loop
         if Names_Match
           (N1    => Sinfo.Resources (I).Name,
            N2    => Name,
            Count => Name.Length)
         then
            declare
               Idx : constant Resource_Count_Type
                 := Sinfo.Resources (I).Memregion_Idx;
            begin
               if Idx > 0 and then Idx <= Sinfo.Memregion_Count then
                  M := Sinfo.Memregions (Idx);
                  exit Search;
               end if;
            end;
         end if;
      end loop Search;

      return M;
   end Memory_By_Name;

   -------------------------------------------------------------------------

   procedure Next
     (Container :        Subject_Info_Type;
      Iter      : in out Memory_Iterator_Type)
   is
   begin
      if Iter.Resource_Idx < Container.Resource_Count then
         Iter.Resource_Idx := Iter.Resource_Idx + 1;
      else
         Iter.Resource_Idx := No_Resource;
      end if;
   end Next;

   -------------------------------------------------------------------------

   function To_Name (Str : String) return Name_Type
   is
      N : Name_Type := (Length  => Str'Length,
                        Padding => 0,
                        Data    => (others => ASCII.NUL));
   begin
      for I in 1 .. N.Length loop
         N.Data (Name_Index_Type (I)) := Str
           (Str'First + Name_Index_Type (I) - 1);
      end loop;

      return N;
   end To_Name;

end Musinfo.Utils;
