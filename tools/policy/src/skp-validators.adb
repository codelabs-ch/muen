with SK.Utils;

package body Skp.Validators
is

   use type SK.Word64;

   -------------------------------------------------------------------------

   procedure Validate_Device (D : Device_Type)
   is
   begin
      Validate_Mem_Layout (L => D.Memory_Layout);
   end Validate_Device;

   -------------------------------------------------------------------------

   procedure Validate_Hardware (H : Hardware_Type)
   is
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
      Validate_Scheduling (S => P.Scheduling);
   end Validate_Policy;

   -------------------------------------------------------------------------

   procedure Validate_Scheduling (S : Scheduling_Type)
   is

      CPU_Ticks : Natural := 0;

      --  Validate major frame.
      procedure Validate_Major_Frame (Pos : Major_Frames_Package.Cursor);

      ----------------------------------------------------------------------

      procedure Validate_Major_Frame (Pos : Major_Frames_Package.Cursor)
      is

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
         Major.Iterate (Process => Validate_CPU'Access);
      end Validate_Major_Frame;
   begin
      S.Major_Frames.Iterate (Process => Validate_Major_Frame'Access);
   end Validate_Scheduling;

   -------------------------------------------------------------------------

   procedure Validate_Subjects (P : Policy_Type)
   is
      --  Validate given subject.
      procedure Validate_Subject (Pos : Subjects_Package.Cursor);

      ----------------------------------------------------------------------

      procedure Validate_Subject (Pos : Subjects_Package.Cursor)
      is
         S : constant Subject_Type := Subjects_Package.Element
           (Position => Pos);
      begin
         Validate_Mem_Layout (L => S.Memory_Layout);
      end Validate_Subject;
   begin
      P.Subjects.Iterate (Process => Validate_Subject'Access);
   end Validate_Subjects;

end Skp.Validators;
