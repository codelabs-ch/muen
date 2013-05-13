with Ada.Strings.Unbounded;

with SK.Utils;

package body Skp.Validators
is

   use Ada.Strings.Unbounded;
   use type SK.Word64;

   -------------------------------------------------------------------------

   procedure Validate_Hardware (H : Hardware_Type)
   is
      type IRQ_Array is array (Natural range 0 .. 255) of Boolean;

      IRQs : IRQ_Array := (others => False);

      --  Validate device specification.
      procedure Validate_Device (D : Device_Type);

      ----------------------------------------------------------------------

      procedure Validate_Device (D : Device_Type)
      is
      begin
         Validate_Mem_Layout (L => D.Memory_Layout);

         if D.IRQ = -1 then
            return;
         end if;

         if IRQs (D.IRQ) then
            raise Validation_Error with "Device '" & To_String (D.Name)
              & "' IRQ" & D.IRQ'Img & " is not unique";
         else
            IRQs (D.IRQ) := True;
         end if;
      end Validate_Device;

      Pos : Devices_Package.Cursor := H.Devices.First;
   begin
      while Devices_Package.Has_Element (Position => Pos) loop
         Validate_Device (D => Devices_Package.Element (Position => Pos));
         Devices_Package.Next (Position => Pos);
      end loop;
   end Validate_Hardware;

   -------------------------------------------------------------------------

   procedure Validate_Kernel (K : Kernel_Type)
   is
   begin
      if K.Pml4_Address mod SK.Page_Size /= 0 then
         raise Validation_Error with "Invalid kernel PML4 address "
           & SK.Utils.To_Hex (Item => K.Pml4_Address)
           & " - address must be 4k aligned";
      end if;

      Validate_Mem_Layout (L => K.Memory_Layout);
   end Validate_Kernel;

   -------------------------------------------------------------------------

   procedure Validate_Mem_Layout (L : Memory_Layout_Type)
   is
      Pos : Memregion_Package.Cursor := L.First;
   begin
      while Memregion_Package.Has_Element (Position => Pos) loop
         Validate_Mem_Region (R => Memregion_Package.Element
                              (Position => Pos));
         Memregion_Package.Next (Position => Pos);
      end loop;
   end Validate_Mem_Layout;

   -------------------------------------------------------------------------

   procedure Validate_Mem_Region (R : Memory_Region_Type)
   is
   begin
      if R.Size mod R.Alignment /= 0 then
         raise Validation_Error with "Invalid memory region size "
           & SK.Utils.To_Hex (Item => R.Size)
           & " for specified alignment "
           & SK.Utils.To_Hex (Item => R.Alignment);
      end if;

      if R.Physical_Address mod R.Alignment /= 0 then
         raise Validation_Error with "Invalid memory region physical address "
           & SK.Utils.To_Hex (Item => R.Physical_Address)
           & " for specified alignment "
           & SK.Utils.To_Hex (Item => R.Alignment);
      end if;

      if R.Virtual_Address mod R.Alignment /= 0 then
         raise Validation_Error with "Invalid memory region virtual address "
           & SK.Utils.To_Hex (Item => R.Virtual_Address)
           & " for specified alignment "
           & SK.Utils.To_Hex (Item => R.Alignment);
      end if;
   end Validate_Mem_Region;

   -------------------------------------------------------------------------

   procedure Validate_Policy (P : Policy_Type)
   is
      One_Megabyte   : constant SK.Word64 := 16#100000#;
      VMCS_Area_Size : constant SK.Word64
        := SK.Word64 (P.Subjects.Length) * SK.Page_Size;
   begin
      if P.Vmxon_Address mod SK.Page_Size /= 0 then
         raise Validation_Error with "Invalid VMXON address "
           & SK.Utils.To_Hex (Item => P.Vmxon_Address)
           & " - address must be 4k aligned";
      end if;

      if P.Vmxon_Address > (One_Megabyte - SK.Page_Size) then
         raise Validation_Error with "Invalid VMXON address "
           & SK.Utils.To_Hex (Item => P.Vmxon_Address)
           & " - address must be below 1m";
      end if;

      if P.Vmcs_Start_Address mod SK.Page_Size /= 0 then
         raise Validation_Error with "Invalid VMCS start address "
           & SK.Utils.To_Hex (Item => P.Vmcs_Start_Address)
           & " - address must be 4k aligned";
      end if;

      if P.Vmcs_Start_Address + VMCS_Area_Size > One_Megabyte then
         raise Validation_Error with "Invalid VMCS start address "
           & SK.Utils.To_Hex (Item => P.Vmcs_Start_Address)
           & " - address must be below 1m - 4k *" & P.Subjects.Length'Img;
      end if;

      Validate_Hardware   (H => P.Hardware);
      Validate_Kernel     (K => P.Kernel);
      Validate_Subjects   (P => P);
      Validate_Scheduling (P => P);
   end Validate_Policy;

   -------------------------------------------------------------------------

   procedure Validate_Scheduling (P : Policy_Type)
   is

      --  Validate major frame.
      procedure Validate_Major_Frame (Pos : Major_Frames_Package.Cursor);

      ----------------------------------------------------------------------

      procedure Validate_Major_Frame (Pos : Major_Frames_Package.Cursor)
      is

         CPU_Ticks : Natural := 0;

         --  Validate CPU element.
         procedure Validate_CPU (Pos : CPU_Package.Cursor);

         -------------------------------------------------------------------

         procedure Validate_CPU (Pos : CPU_Package.Cursor)
         is
            CPU       : constant CPU_Type
              := CPU_Package.Element (Position => Pos);
            Minor_Idx : Minor_Frames_Package.Cursor := CPU.First;
            Ticks     : Natural                     := 0;
         begin
            while Minor_Frames_Package.Has_Element (Position => Minor_Idx)
            loop
               Ticks := Ticks + Minor_Frames_Package.Element
                 (Position => Minor_Idx).Ticks;
               Minor_Frames_Package.Next (Position => Minor_Idx);
            end loop;

            if CPU_Ticks = 0 then
               CPU_Ticks := Ticks;
            else
               if CPU_Ticks /= Ticks then
                  raise Validation_Error with "Invalid CPU elements in "
                    & "scheduling plan, tick counts differ";
               end if;
            end if;
         end Validate_CPU;

         Major : constant Major_Frame_Type := Major_Frames_Package.Element
           (Position => Pos);
      begin
         if Natural (Major.Length) /= P.Hardware.Processor.Logical_CPUs then
            raise Validation_Error with "Invalid CPU elements in scheduling "
              & "plan, logical CPU count differs";
         end if;

         Major.Iterate (Process => Validate_CPU'Access);
      end Validate_Major_Frame;
   begin
      P.Scheduling.Major_Frames.Iterate
        (Process => Validate_Major_Frame'Access);
   end Validate_Scheduling;

   -------------------------------------------------------------------------

   procedure Validate_Subjects (P : Policy_Type)
   is
      --  Validate given subject.
      procedure Validate_Subject (Pos : Subjects_Package.Cursor);

      ----------------------------------------------------------------------

      procedure Validate_Subject (Pos : Subjects_Package.Cursor)
      is
         use type Binary_Package.Cursor;

         S : constant Subject_Type := Subjects_Package.Element
           (Position => Pos);

         --  Validate given trap table entry.
         procedure Validate_Trap_Entry (Pos : Traps_Package.Cursor);

         --  Validate given signal table entry.
         procedure Validate_Signal_Entry (Pos : Signals_Package.Cursor);

         -------------------------------------------------------------------

         procedure Validate_Signal_Entry (Pos : Signals_Package.Cursor)
         is
            Sig : constant Signal_Table_Entry_Type
              := Signals_Package.Element (Position => Pos);
         begin
            if Sig.Dst_Subject = S.Name then
               raise Validation_Error with "Subject " & To_String (S.Name)
                 & ": Reference to self in signal table entry"
                 & Sig.Signal'Img;
            end if;

            if Get_Id (Subjects => P.Subjects,
                       Name     => Sig.Dst_Subject) = -1
            then
               raise Validation_Error with "Subject " & To_String (S.Name)
                 & ": Undefined destination subject '"
                 & To_String (Sig.Dst_Subject) & "' in signal table entry"
                 & Sig.Signal'Img;
            end if;

            if Sig.Kind /= Handover and then Sig.Dst_Vector = 256 then
               raise Validation_Error with "Subject " & To_String (S.Name)
                 & ": No destination vector given in signal table entry"
                 & Sig.Signal'Img;
            end if;
         end Validate_Signal_Entry;

         -------------------------------------------------------------------

         procedure Validate_Trap_Entry (Pos : Traps_Package.Cursor)
         is
            Trap  : constant Trap_Table_Entry_Type := Traps_Package.Element
              (Position => Pos);
         begin
            if Trap.Dst_Subject = S.Name then
               raise Validation_Error with "Subject " & To_String (S.Name)
                 & ": Reference to self in trap table entry " & Trap.Kind'Img;
            end if;

            if Get_Id (Subjects => P.Subjects,
                       Name     => Trap.Dst_Subject) = -1
            then
               raise Validation_Error with "Subject " & To_String (S.Name)
                 & ": Undefined destination subject '"
                 & To_String (Trap.Dst_Subject) & "' in trap table entry "
                 & Trap.Kind'Img;
            end if;
         end Validate_Trap_Entry;
      begin
         if S.Pml4_Address mod SK.Page_Size /= 0 then
            raise Validation_Error with "Subject " & To_String (S.Name)
              & ": Invalid PML4 address "
              & SK.Utils.To_Hex (Item => S.Pml4_Address)
              & " - address must be 4k aligned";
         end if;
         if S.IO_Bitmap_Address mod SK.Page_Size /= 0 then
            raise Validation_Error with "Subject " & To_String (S.Name)
              & ": Invalid I/O bitmap address "
              & SK.Utils.To_Hex (Item => S.IO_Bitmap_Address)
              & " - address must be 4k aligned";
         end if;

         Validate_Mem_Layout (L => S.Memory_Layout);

         if P.Binaries.Find
           (Key => S.Binary.Name) = Binary_Package.No_Element
         then
            raise Validation_Error with "Subject " & To_String (S.Name)
              & ": Referenced binary '" & To_String (S.Binary.Name)
              & "' not found in policy";
         end if;

         S.Trap_Table.Iterate (Process => Validate_Trap_Entry'Access);
         S.Signal_Table.Iterate (Process => Validate_Signal_Entry'Access);
      end Validate_Subject;
   begin
      P.Subjects.Iterate (Process => Validate_Subject'Access);
   end Validate_Subjects;

end Skp.Validators;
