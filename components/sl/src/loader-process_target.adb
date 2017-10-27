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

with System;

with Interfaces;

with SK.Bitops;
with SK.Strings;
with SK.Constants;

pragma $Release_Warnings (Off, "unit * is not referenced");
with Debuglog.Client;
pragma $Release_Warnings (On, "unit * is not referenced");

with Loader.Globals;
with Loader.Addrspace;

with Debug_Ops;

package body Loader.Process_Target
is

   --  Does the actual target sinfo processing.
   --
   --  Rationale: Overlays with non-volatile variables won't be legal SPARK for
   --  much longer, that is why we factor out the real processing in this extra
   --  function [PA24-008].
   procedure Process
     (Sinfo      :     Musinfo.Subject_Info_Type;
      Sinfo_Addr :     Interfaces.Unsigned_64;
      Success    : out Boolean)
   with
      Pre => Musinfo.Instance.Is_Valid;

   --  Process given target subject memory region.
   procedure Process_Memregion
     (Mem     :     Musinfo.Utils.Named_Memregion_Type;
      Success : out Boolean)
   with
      Pre => Musinfo.Instance.Is_Valid;

   --  Set CR4.VMXE bit in subject state at given address.
   procedure Set_VMXE_Bit (State_Addr : Interfaces.Unsigned_64);

   -------------------------------------------------------------------------

   procedure Set_VMXE_Bit (State_Addr : Interfaces.Unsigned_64)
   with
      SPARK_Mode => Off
   is
      State : SK.Subject_State_Type
      with
         Import,
         Address => System'To_Address (State_Addr);
   begin
      State.CR4 := SK.Bitops.Bit_Set (Value => State.CR4,
                                      Pos   => SK.Constants.CR4_VMXE_FLAG);
   end Set_VMXE_Bit;

   -------------------------------------------------------------------------

   procedure Process_Memregion
     (Mem     :     Musinfo.Utils.Named_Memregion_Type;
      Success : out Boolean)
   is
      package IFA renames Interfaces;

      use type IFA.Unsigned_64;
      use type Musinfo.Hash_Type;
      use type Musinfo.Memregion_Type;

      Dst_Addr : constant IFA.Unsigned_64
        := Mem.Data.Address + Globals.Get_Current_Sinfo_Offset;
   begin
      Success := False;

      if Dst_Addr not in Addrspace.Dst_Addr_Type then
         pragma Debug
           (Debuglog.Client.Put_Line
              (Item => "Error: Destination address not valid "
               & SK.Strings.Img (Dst_Addr)));
         return;
      end if;

      if Mem.Data.Size not in Addrspace.Size_Type then
         pragma Debug
           (Debuglog.Client.Put_Line
              (Item => "Error: Memregion size out of bounds "
               & SK.Strings.Img (Mem.Data.Size)));
         return;
      end if;

      if Mem.Data.Flags.Writable then
         pragma Debug (Debug_Ops.Put
                       (Msg  => "Examining writable memory region",
                        Name => Mem.Name));

         case Mem.Data.Content is
            when Musinfo.Content_File =>
               pragma Debug
                 (Debuglog.Client.Put_Line
                    (Item => "Error: Writable file region found at target "
                     & "address " & SK.Strings.Img (Mem.Data.Address)));
               return;
            when Musinfo.Content_Fill =>
               pragma Debug
                 (Debuglog.Client.Put_Line
                    (Item => "Filling region at target address "
                     & SK.Strings.Img (Mem.Data.Address)
                     & " with pattern "
                     & SK.Strings.Img (IFA.Unsigned_8 (Mem.Data.Pattern))
                     & ", local address is " & SK.Strings.Img (Dst_Addr)));

               Addrspace.Memset
                 (Address => Dst_Addr,
                  Size    => Mem.Data.Size,
                  Pattern => IFA.Unsigned_8
                    (Mem.Data.Pattern));
            when Musinfo.Content_Uninitialized =>
               if Mem.Data.Hash /= Musinfo.No_Hash then
                  declare
                     Src_Region : constant Musinfo.Memregion_Type
                       := Musinfo.Instance.Memory_By_Hash
                         (Hash    => Mem.Data.Hash,
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

         if Mem.Data.Hash /= Musinfo.No_Hash then
            declare

               --  The policy guarantees that memory region sizes must have
               --  page granularity.

               pragma Assume (Mem.Data.Size mod 64 = 0);

               Hash : constant Musinfo.Hash_Type
                 := Addrspace.Calculate_Hash (Address => Dst_Addr,
                                              Size    => Mem.Data.Size);
            begin
               if Mem.Data.Hash /= Hash then
                  pragma Debug
                    (Debug_Ops.Put
                       (Msg  => "Error: Hash invalid for memory region",
                        Name => Mem.Name));
                  pragma Debug (Debuglog.Client.Put (Item => "Expected "));
                  pragma Debug (Debug_Ops.Put_Hash  (Item => Mem.Data.Hash));
                  pragma Debug (Debuglog.Client.Put (Item => ", got "));
                  pragma Debug (Debug_Ops.Put_Hash  (Item => Hash));
                  pragma Debug (Debuglog.Client.New_Line);

                  return;
               end if;

               pragma Debug (Debug_Ops.Put
                             (Msg  => "Hash OK for memory region",
                              Name => Mem.Name));
            end;
         end if;
      end if;

      Success := True;
   end Process_Memregion;

   -------------------------------------------------------------------------

   procedure Process
     (Sinfo_Mem :     Musinfo.Utils.Named_Memregion_Type;
      Success   : out Boolean)
   with
      SPARK_Mode => Off
   is
      Target_Sinfo : Musinfo.Subject_Info_Type
      with
         Import,
         Address => System'To_Address (Sinfo_Mem.Data.Address);
   begin
      Process (Sinfo      => Target_Sinfo,
               Sinfo_Addr => Sinfo_Mem.Data.Address,
               Success    => Success);
   end Process;

   -------------------------------------------------------------------------

   procedure Process
     (Sinfo      :     Musinfo.Subject_Info_Type;
      Sinfo_Addr :     Interfaces.Unsigned_64;
      Success    : out Boolean)
   is
   begin
      Success := False;

      if not Musinfo.Utils.Is_Valid (Sinfo => Sinfo) then
         pragma Debug (Debuglog.Client.Put_Line
                       (Item => "Error: Target sinfo not valid at address "
                        & SK.Strings.Img (Sinfo_Addr)));
         return;
      end if;

      pragma Debug (Debug_Ops.Put
                    (Msg  => "Processing subject",
                     Name => Sinfo.Name));

      declare
         use type Interfaces.Unsigned_64;

         Target_Sinfo_Mem : constant Musinfo.Memregion_Type
           := Musinfo.Utils.Memory_By_Name
             (Sinfo => Sinfo,
              Name  => Musinfo.Utils.To_Name (Str => "sinfo"));
         Offset : constant Interfaces.Unsigned_64
           := Sinfo_Addr - Target_Sinfo_Mem.Address;
      begin
         pragma Debug (Debuglog.Client.Put_Line
                       (Item => "Setting current sinfo offset to "
                        & SK.Strings.Img (Offset)));
         Globals.Set_Current_Sinfo_Offset (O => Offset);

         declare
            Iter     : Musinfo.Utils.Memory_Iterator_Type
              := Musinfo.Utils.Create_Memory_Iterator (Container => Sinfo);
            Mem_Succ : Boolean;
         begin
            Process_Memregions :
            while Musinfo.Utils.Has_Element
              (Container => Sinfo,
               Iter      => Iter)
            loop
               Process_Memregion
                 (Mem     => Musinfo.Utils.Element
                    (Container => Sinfo,
                     Iter      => Iter),
                  Success => Mem_Succ);
               if not Mem_Succ then
                  return;
               end if;
               Musinfo.Utils.Next (Container => Sinfo,
                                   Iter      => Iter);
               pragma Loop_Invariant
                 (Musinfo.Utils.Belongs_To (Container => Sinfo,
                                            Iter      => Iter));
            end loop Process_Memregions;
         end;

         --  Reset state.

         declare
            use type Musinfo.Name_Size_Type;
            use type Musinfo.Memregion_Type;

            Subj_Name : constant Musinfo.Name_Type
              := Musinfo.Utils.Subject_Name (Sinfo => Sinfo);
            Selector  : constant Musinfo.Name_Type
              := Musinfo.Utils.To_Name (Str => "monitor_state_");
            State_Mem : Musinfo.Memregion_Type;
         begin
            if Subj_Name.Length + Selector.Length > Musinfo.Name_Size_Type'Last
            then
               pragma Debug
                 (Debuglog.Client.Put_Line
                    (Item => "Error: Unable to construct subject state "
                     & "selector, string too long"));
               return;
            end if;

            State_Mem := Musinfo.Instance.Memory_By_Name
              (Name => Musinfo.Utils.Concat
                 (L => Selector,
                  R => Subj_Name));
            if State_Mem = Musinfo.Null_Memregion then
               pragma Debug
                 (Debuglog.Client.Put_Line
                    (Item => "Error: Unable to retrieve state memory, check "
                     & "logical names"));
               return;
            end if;

            Set_VMXE_Bit (State_Addr => State_Mem.Address);
         end;
      end;

      Success := True;
   end Process;

end Loader.Process_Target;
