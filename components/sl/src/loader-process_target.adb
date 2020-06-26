--
--  Copyright (C) 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Interfaces;

with SK.Strings;

with Musinfo.Utils;

pragma $Release_Warnings (Off, "unit * is not referenced");
with Debuglog.Client;
pragma $Release_Warnings (On, "unit * is not referenced");

with Loader.Globals;
with Loader.Addrspace;

with Debug_Ops;

package body Loader.Process_Target
is

   --  Process given target subject memory region resource.
   procedure Process_Memregion
     (Resource :     Musinfo.Resource_Type;
      Success  : out Boolean)
   with
      Pre => Musinfo.Instance.Is_Valid and Resource.Kind = Musinfo.Res_Memory;

   -------------------------------------------------------------------------

   procedure Process_Memregion
     (Resource :     Musinfo.Resource_Type;
      Success  : out Boolean)
   is
      package IFA renames Interfaces;

      use type IFA.Unsigned_64;
      use type Musinfo.Hash_Type;
      use type Musinfo.Memregion_Type;

      Dst_Addr : constant IFA.Unsigned_64
        := Resource.Mem_Data.Address + Target_Sinfo_Offset;
   begin
      Success := False;

      if Dst_Addr not in Addrspace.Dst_Addr_Type then
         pragma Debug
           (Debuglog.Client.Put_Line
              (Item => "Error: Destination address not valid "
               & SK.Strings.Img (Dst_Addr)));
         return;
      end if;

      if Resource.Mem_Data.Size not in Addrspace.Size_Type then
         pragma Debug
           (Debuglog.Client.Put_Line
              (Item => "Error: Memregion size out of bounds "
               & SK.Strings.Img (Resource.Mem_Data.Size)));
         return;
      end if;

      if Resource.Mem_Data.Flags.Writable then
         pragma Debug (Debug_Ops.Put
                       (Msg  => "Examining writable memory region",
                        Name => Resource.Name));

         case Resource.Mem_Data.Content is
            when Musinfo.Content_File =>
               pragma Debug
                 (Debuglog.Client.Put_Line
                    (Item => "Error: Writable file region found at target "
                     & "address " & SK.Strings.Img
                       (Resource.Mem_Data.Address)));
               return;
            when Musinfo.Content_Fill =>
               pragma Debug
                 (Debuglog.Client.Put_Line
                    (Item => "Filling region at target address "
                     & SK.Strings.Img (Resource.Mem_Data.Address)
                     & " with pattern "
                     & SK.Strings.Img (IFA.Unsigned_8
                       (Resource.Mem_Data.Pattern))
                     & ", local address is " & SK.Strings.Img (Dst_Addr)));

               Addrspace.Memset
                 (Address => Dst_Addr,
                  Size    => Resource.Mem_Data.Size,
                  Pattern => IFA.Unsigned_8
                    (Resource.Mem_Data.Pattern));
            when Musinfo.Content_Uninitialized =>
               if Resource.Mem_Data.Hash /= Musinfo.No_Hash then
                  declare
                     Src_Region : constant Musinfo.Memregion_Type
                       := Musinfo.Instance.Memory_By_Hash
                         (Hash    => Resource.Mem_Data.Hash,
                          Content => Musinfo.Content_File);
                  begin
                     if Src_Region = Musinfo.Null_Memregion then
                        pragma Debug
                          (Debuglog.Client.Put_Line
                             (Item  => "Error: No source region for hash"));
                        return;
                     end if;

                     if Src_Region.Address not in Addrspace.Src_Addr_Type then
                        pragma Debug
                          (Debuglog.Client.Put_Line
                             (Item => "Error: Source address not valid "
                              & SK.Strings.Img (Src_Region.Address)));
                        return;
                     end if;

                     if Src_Region.Size not in Addrspace.Size_Type then
                        pragma Debug
                          (Debuglog.Client.Put_Line
                             (Item => "Error: Source size out of bounds "
                              & SK.Strings.Img (Src_Region.Size)));
                        return;
                     end if;

                     pragma Debug
                       (Debuglog.Client.Put_Line
                          (Item => "Copying content of source region"
                           & " at address "
                           & SK.Strings.Img (Src_Region.Address)
                           & " to destination region at address "
                           & SK.Strings.Img (Dst_Addr)));

                     Addrspace.Memcpy
                       (Dst_Address => Dst_Addr,
                        Src_Address => Src_Region.Address,
                        Size        => Src_Region.Size);
                  end;
               end if;
         end case;

         if Resource.Mem_Data.Hash /= Musinfo.No_Hash then
            declare
               Hash : constant Musinfo.Hash_Type
                 := Addrspace.Calculate_Hash
                   (Address => Dst_Addr,
                    Size    => Resource.Mem_Data.Size);
            begin
               if Resource.Mem_Data.Hash /= Hash then
                  pragma Debug
                    (Debug_Ops.Put
                       (Msg  => "Error: Hash invalid for memory region",
                        Name => Resource.Name));
                  pragma Debug (Debuglog.Client.Put (Item => "Expected "));
                  pragma Debug (Debug_Ops.Put_Hash
                                (Item => Resource.Mem_Data.Hash));
                  pragma Debug (Debuglog.Client.Put (Item => ", got "));
                  pragma Debug (Debug_Ops.Put_Hash  (Item => Hash));
                  pragma Debug (Debuglog.Client.New_Line);

                  return;
               end if;

               pragma Debug (Debug_Ops.Put
                             (Msg  => "Hash OK for memory region",
                              Name => Resource.Name));
            end;
         end if;
      end if;

      Success := True;
   end Process_Memregion;

   -------------------------------------------------------------------------

   procedure Process
     (Sinfo_Mem :     Musinfo.Resource_Type;
      Success   : out Boolean)
   is
      use type Interfaces.Unsigned_64;
   begin
      Success := False;
      if Sinfo_Mem.Mem_Data.Address /= Target_Sinfo_Address then
         pragma Debug (Debuglog.Client.Put_Line
                       (Item => "Error: Target sinfo address mismatch "
                        & SK.Strings.Img (Sinfo_Mem.Mem_Data.Address) & " /= "
                        & SK.Strings.Img
                          (Interfaces.Unsigned_64'(Target_Sinfo_Address))));
         return;
      end if;

      if not Musinfo.Utils.Is_Valid (Sinfo => Globals.Target_Sinfo) then
         pragma Debug (Debuglog.Client.Put_Line
                       (Item => "Error: Target sinfo not valid"));
         return;
      end if;

      pragma Debug (Debug_Ops.Put
                    (Msg  => "Processing subject",
                     Name => Globals.Target_Sinfo.Name));

      declare
         Target_Sinfo_Mem : constant Musinfo.Memregion_Type
           := Musinfo.Utils.Memory_By_Name
             (Sinfo => Globals.Target_Sinfo,
              Name  => Musinfo.Utils.To_Name (Str => "sinfo"));
         Offset           : constant Interfaces.Unsigned_64
           := Target_Sinfo_Address - Target_Sinfo_Mem.Address;
      begin
         if Offset /= Target_Sinfo_Offset then
            pragma Debug (Debuglog.Client.Put_Line
                          (Item => "Error: Target sinfo offset mismatch "
                           & SK.Strings.Img (Offset) & " /= "
                           & SK.Strings.Img
                             (Interfaces.Unsigned_64'(Target_Sinfo_Offset))));
            return;
         end if;

         declare
            Iter     : Musinfo.Utils.Resource_Iterator_Type
              := Musinfo.Utils.Create_Resource_Iterator
                (Container => Globals.Target_Sinfo);
            Mem_Succ : Boolean;
            Element  : Musinfo.Resource_Type;
         begin
            Process_Memregions :
            while Musinfo.Utils.Has_Element
              (Container => Globals.Target_Sinfo,
               Iter      => Iter)
            loop
               Element := Musinfo.Utils.Element
                 (Container => Globals.Target_Sinfo,
                  Iter      => Iter);

               if Element.Kind = Musinfo.Res_Memory then
                  Process_Memregion
                    (Resource => Element,
                     Success  => Mem_Succ);
                  if not Mem_Succ then
                     return;
                  end if;
               end if;
               Musinfo.Utils.Next (Iter => Iter);
               pragma Loop_Invariant
                 (Musinfo.Utils.Belongs_To (Container => Globals.Target_Sinfo,
                                            Iter      => Iter));
            end loop Process_Memregions;
         end;
      end;

      Success := True;
   end Process;

end Loader.Process_Target;
