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

with System;

with Skp.IOMMU;

with SK.Dump;
with SK.KC;
with SK.CPU;
with SK.VTd.Types;
with SK.VTd.Dump;
pragma $Release_Warnings (Off, "unit * is not referenced");
with SK.Apic;
with SK.Constants;
pragma $Release_Warnings (On, "unit * is not referenced");

package body SK.VTd
with
  Refined_State => (State => IOMMUs)
is

   --  Maximum number of busy-loops to perform when waiting for the hardware to
   --  set a status flag.
   Loop_Count_Max : constant := 10000;

   IOMMUs : Types.IOMMU_Array
     with
       Volatile,
       Async_Writers,
       Async_Readers,
       Effective_Writes,
       Address => System'To_Address (Skp.IOMMU.Base_Address);

   -------------------------------------------------------------------------

   --  Set command register fields based on given status register values.
   --  The current state of the global command register must be reconstructed
   --  from the global status register since command register values are
   --  *undefined* when read, see Intel VT-d spec, section 11.4.4.
   procedure Set_Command_From_Status
     (Command : out Types.Reg_Global_Command_Type;
      Status  :     Types.Reg_Global_Status_Type)
   with
      Depends => (Command => Status)
   is
   begin
      Command := (CFI      => Status.CFIS,
                  SIRTP    => Status.IRTPS,
                  IRE      => Status.IRES,
                  QIE      => Status.QIES,
                  WBF      => Status.WBFS,
                  EAFL     => Status.AFLS,
                  SFL      => Status.FLS,
                  SRTP     => Status.RTPS,
                  TE       => Status.TES,
                  Reserved => (others => 0));
   end Set_Command_From_Status;

   -------------------------------------------------------------------------

   --  Clears the Fault recording register and the Primary Fault Overflow flag
   --  of the specified IOMMU.
   procedure Clear_Fault_Record (IOMMU : Skp.IOMMU.IOMMU_Device_Range)
   with
      SPARK_Mode => $Complete_Proofs,  -- [N425-012]
      Global     => (In_Out => IOMMUs),
      Depends    => (IOMMUs =>+ IOMMU)
   is
      Fault_Recording : Types.Reg_Fault_Recording_Type;
      Fault_Status    : Types.Reg_Fault_Status_Type;
   begin
      Fault_Recording   := IOMMUs (IOMMU).Fault_Recording;
      Fault_Recording.F := 1;
      IOMMUs (IOMMU).Fault_Recording := Fault_Recording;

      Fault_Status     := IOMMUs (IOMMU).Fault_Status;
      Fault_Status.PFO := 1;
      IOMMUs (IOMMU).Fault_Status := Fault_Status;
   end Clear_Fault_Record;

   -------------------------------------------------------------------------

   --  Sets the fault event interrupt mask of the specified IOMMU to the given
   --  state. Setting the mask to true prohibits hardware from generating an
   --  interrupt when a fault event occurs.
   procedure Set_Fault_Event_Mask
     (IOMMU  : Skp.IOMMU.IOMMU_Device_Range;
      Enable : Boolean)
   with
      SPARK_Mode => $Complete_Proofs,  -- [N425-012]
      Global     => (In_Out => IOMMUs),
      Depends    => (IOMMUs =>+ (IOMMU, Enable))
   is
      Fault_Event_Control : Types.Reg_Fault_Event_Control_Type;
   begin
      Fault_Event_Control    := IOMMUs (IOMMU).Fault_Event_Control;
      Fault_Event_Control.IM := (if Enable then 1 else 0);
      IOMMUs (IOMMU).Fault_Event_Control := Fault_Event_Control;
   end Set_Fault_Event_Mask;

   -------------------------------------------------------------------------

   --  Sets the fault interrupt vector and destination APIC ID of the specified
   --  IOMMU to the given values.
   pragma $Release_Warnings (Off, "procedure * is not referenced");
   procedure Setup_Fault_Interrupt
     (IOMMU   : Skp.IOMMU.IOMMU_Device_Range;
      Vector  : SK.Byte;
      APIC_ID : SK.Byte)
   with
      SPARK_Mode => $Complete_Proofs,  -- [N425-012]
      Global     => (In_Out => IOMMUs),
      Depends    => (IOMMUs =>+ (IOMMU, Vector, APIC_ID))
   is
      Fault_Event_Addr : Types.Reg_Fault_Event_Address_Type;
      Fault_Event_Data : Types.Reg_Fault_Event_Data_Type;
   begin
      Fault_Event_Addr         := IOMMUs (IOMMU).Fault_Event_Address;
      Fault_Event_Addr.APIC_ID := APIC_ID;
      IOMMUs (IOMMU).Fault_Event_Address := Fault_Event_Addr;

      Fault_Event_Data.EIMD := 0;
      Fault_Event_Data.IMD  := SK.Word16 (Vector);
      IOMMUs (IOMMU).Fault_Event_Data := Fault_Event_Data;
   end Setup_Fault_Interrupt;
   pragma $Release_Warnings (On, "procedure * is not referenced");

   -------------------------------------------------------------------------

   procedure Process_Fault
   with
      SPARK_Mode     => $Complete_Proofs,  -- [N425-012]
      Refined_Global => (In_Out => IOMMUs)
   is
      use type SK.VTd.Types.Bit_Type;

      Status : Types.Reg_Fault_Status_Type;
   begin
      for I in Skp.IOMMU.IOMMU_Device_Range loop
         Status := IOMMUs (I).Fault_Status;

         if Status.PPF = 1 then
            declare
               Dummy : Types.Reg_Fault_Recording_Type;
            begin
               pragma $Prove_Warnings (Off, "unused assignment");
               Dummy := IOMMUs (I).Fault_Recording;
               pragma $Prove_Warnings (On, "unused assignment");
               pragma Debug (SK.VTd.Dump.Print_VTd_Fault
                             (IOMMU  => I,
                              Status => Status,
                              Fault  => Dummy));
            end;

            Clear_Fault_Record (IOMMU => I);
         end if;
      end loop;
   end Process_Fault;

   -------------------------------------------------------------------------

   --  Check capabilities of IOMMU given by index. Return False if capability
   --  requirements are not met.
   procedure Check_Capabilities
     (Idx    :     Skp.IOMMU.IOMMU_Device_Range;
      Result : out Boolean)
   with
      SPARK_Mode => $Complete_Proofs,  -- [N425-012]
      Global     => (Input  => IOMMUs),
      Depends    => (Result => (IOMMUs, Idx))
   is
      use type SK.VTd.Types.Bit_Type;
      use type SK.VTd.Types.Bit_3_Type;
      use type SK.VTd.Types.Bit_4_Type;
      use type SK.VTd.Types.Bit_10_Type;

      Version : Types.Reg_Version_Type;
      Caps    : Types.Reg_Capability_Type;
      Extcaps : Types.Reg_Extcapability_Type;

      Supported_Version, Nr_Domains, AGAW_39_Bit : Boolean;
      Matching_FRO, Matching_NFR, Matching_IRO   : Boolean;
   begin
      Version := IOMMUs (Idx).Version;
      Supported_Version := Version.MAX = 1 and then Version.MIN = 0;
      pragma Debug (not Supported_Version,
                    SK.Dump.Print_Message_16
                      (Msg  => "Unsupported IOMMU version",
                       Item => SK.Word16 (Version.MAX) * 2 ** 8 +
                           SK.Word16 (Version.MIN)));

      Caps := IOMMUs (Idx).Capability;

      Nr_Domains := Caps.ND >= 2;
      pragma Debug
        (not Nr_Domains,
         KC.Put_Line (Item => "IOMMU supports less than 256 domains"));

      AGAW_39_Bit := Caps.SAGAW (2) = 1;
      pragma Debug
        (not AGAW_39_Bit,
         KC.Put_Line (Item => "No support for 39-bit AGAW in IOMMU"));

      Matching_FRO := Caps.FRO * 16 = Types.FR_Offset;
      pragma Debug (not Matching_FRO,
                    SK.Dump.Print_Message_16
                      (Msg  => "Unsupported IOMMU FRO",
                       Item => SK.Word16 (Caps.FRO)));

      Matching_NFR := Caps.NFR = 0 ;
      pragma Debug (not Matching_NFR,
                    SK.Dump.Print_Message_8
                      (Msg  => "Unsupported IOMMU NFR",
                       Item => Caps.NFR));

      Extcaps := IOMMUs (Idx).Ext_Capability;

      Matching_IRO := Extcaps.IRO * 16 + 8 = Types.IOTLB_Offset;
      pragma Debug (not Matching_IRO,
                    SK.Dump.Print_Message_16
                      (Msg  => "Unsupported IOMMU IRO",
                       Item => SK.Word16 (Extcaps.IRO)));

      Result := Supported_Version and
        Nr_Domains                and
        AGAW_39_Bit               and
        Matching_FRO              and
        Matching_NFR              and
        Matching_IRO;
   end Check_Capabilities;

   -------------------------------------------------------------------------

   --  Set address of root table for IOMMU with given index.
   procedure Set_Root_Table_Address
     (IOMMU   :     Skp.IOMMU.IOMMU_Device_Range;
      Address :     SK.Word64;
      Success : out Boolean)
   with
      SPARK_Mode => $Complete_Proofs,  -- [N425-012]
      Global     => (In_Out => IOMMUs),
      Depends    => ((IOMMUs, Success) => (IOMMUs, IOMMU, Address))
   is
      use type SK.VTd.Types.Bit_Type;

      Global_Status  : Types.Reg_Global_Status_Type;
      Global_Command : Types.Reg_Global_Command_Type;
   begin
      IOMMUs (IOMMU).Root_Table_Address := Address;

      Global_Status := IOMMUs (IOMMU).Global_Status;
      Set_Command_From_Status (Command => Global_Command,
                               Status  => Global_Status);
      Global_Command.SRTP := 1;
      IOMMUs (IOMMU).Global_Command := Global_Command;

      for I in 1 .. Loop_Count_Max loop
         Global_Status := IOMMUs (IOMMU).Global_Status;
         exit when Global_Status.RTPS = 1;
      end loop;
      Success := Global_Status.RTPS = 1;
   end Set_Root_Table_Address;

   -------------------------------------------------------------------------

   --  Invalidate context cache of IOMMU with given index.
   procedure Invalidate_Context_Cache
     (IOMMU   :     Skp.IOMMU.IOMMU_Device_Range;
      Success : out Boolean)
   with
      SPARK_Mode => $Complete_Proofs,  -- [N425-012]
      Global     => (In_Out => IOMMUs),
      Depends    => ((IOMMUs, Success) => (IOMMUs, IOMMU))
   is
      use type SK.VTd.Types.Bit_Type;

      Context_Command : Types.Reg_Context_Command_Type;
   begin
      Context_Command      := IOMMUs (IOMMU).Context_Command;
      Context_Command.ICC  := 1;
      Context_Command.CIRG := 1;
      IOMMUs (IOMMU).Context_Command := Context_Command;

      for J in 1 .. Loop_Count_Max loop
         Context_Command := IOMMUs (IOMMU).Context_Command;
         exit when Context_Command.ICC = 0;
      end loop;
      Success := Context_Command.ICC = 0;
   end Invalidate_Context_Cache;

   -------------------------------------------------------------------------

   --  Flush IOTLB of IOMMU with given index.
   procedure Flush_IOTLB
     (IOMMU   :     Skp.IOMMU.IOMMU_Device_Range;
      Success : out Boolean)
   with
      SPARK_Mode => $Complete_Proofs,  -- [N425-012]
      Global     => (In_Out => IOMMUs),
      Depends    => ((IOMMUs, Success) => (IOMMUs, IOMMU))
   is
      use type SK.VTd.Types.Bit_Type;

      IOTLB_Invalidate : Types.Reg_IOTLB_Invalidate;
   begin
      IOTLB_Invalidate      := IOMMUs (IOMMU).IOTLB_Invalidate;
      IOTLB_Invalidate.IIRG := 1;
      IOTLB_Invalidate.IVT  := 1;
      IOMMUs (IOMMU).IOTLB_Invalidate := IOTLB_Invalidate;

      for J in 1 .. Loop_Count_Max loop
         IOTLB_Invalidate := IOMMUs (IOMMU).IOTLB_Invalidate;
         exit when IOTLB_Invalidate.IVT = 0;
      end loop;
      Success := IOTLB_Invalidate.IVT = 0;
   end Flush_IOTLB;

   -------------------------------------------------------------------------

   --  Enable address translation for IOMMU with given index.
   procedure Enable_Translation
     (IOMMU   :     Skp.IOMMU.IOMMU_Device_Range;
      Success : out Boolean)
   with
      SPARK_Mode => $Complete_Proofs,  -- [N425-012]
      Global     => (In_Out => IOMMUs),
      Depends    => ((IOMMUs, Success) => (IOMMUs, IOMMU))
   is
      use type SK.VTd.Types.Bit_Type;

      Global_Command : Types.Reg_Global_Command_Type;
      Global_Status  : Types.Reg_Global_Status_Type;
   begin
      Global_Status := IOMMUs (IOMMU).Global_Status;
      Set_Command_From_Status (Command => Global_Command,
                               Status  => Global_Status);
      Global_Command.TE := 1;
      IOMMUs (IOMMU).Global_Command := Global_Command;

      for J in 1 .. Loop_Count_Max loop
         Global_Status := IOMMUs (IOMMU).Global_Status;
         exit when Global_Status.TES = 1;
      end loop;
      Success := Global_Status.TES = 1;
   end Enable_Translation;

   -------------------------------------------------------------------------

   pragma $Prove_Warnings (Off, "unused variable ""IOMMU""");
   procedure VTd_Error
     (IOMMU   : Skp.IOMMU.IOMMU_Device_Range;
      Message : String)
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ null,
                  null         => (IOMMU, Message)),
      No_Return
   is
   begin
      pragma Debug (KC.Put_String (Item => "IOMMU "));
      pragma Debug (KC.Put_Byte   (Item => SK.Byte (IOMMU)));
      pragma Debug (KC.Put_String (Item => ": "));
      pragma Debug (KC.Put_Line   (Item => Message));

      pragma Assume (False);  --  Workaround for No_Return: Pre=>False
      CPU.Panic;
   end VTd_Error;
   pragma $Prove_Warnings (On, "unused variable ""IOMMU""");

   -------------------------------------------------------------------------

   procedure Initialize
   with
      SPARK_Mode      => $Complete_Proofs,  -- [N722-005]
      Refined_Global  => (In_Out => (X86_64.State, IOMMUs)),
      Refined_Depends => ((X86_64.State, IOMMUs) =>+ IOMMUs)
   is
      Needed_Caps_Present, Status : Boolean;
   begin
      for I in Skp.IOMMU.IOMMU_Device_Range loop
         Check_Capabilities (Idx    => I,
                             Result => Needed_Caps_Present);

         if not Needed_Caps_Present then
            pragma Assume (False);  --  Workaround for No_Return: Pre=>False
            if True then  --  Workaround for No_Return placement limitation
               VTd_Error (IOMMU   => I,
                          Message => "capability check failed");
            end if;
         end if;

         Set_Fault_Event_Mask (IOMMU  => I,
                               Enable => True);
         Clear_Fault_Record (IOMMU => I);
         pragma Debug (Setup_Fault_Interrupt
                       (IOMMU   => I,
                        Vector  => SK.Constants.VTd_Fault_Vector,
                        APIC_ID => SK.Apic.Get_ID));
         pragma Debug (Set_Fault_Event_Mask (IOMMU  => I,
                                             Enable => False));

         Set_Root_Table_Address
           (IOMMU   => I,
            Address => Skp.IOMMU.Root_Table_Address,
            Success => Status);
         if not Status then
            pragma Assume (False);  --  Workaround for No_Return: Pre=>False
            if True then  --  Workaround for No_Return placement limitation
               VTd_Error (IOMMU   => I,
                          Message => "unable to set root table address");
            end if;
         end if;

         Invalidate_Context_Cache
           (IOMMU   => I,
            Success => Status);
         if not Status then
            pragma Assume (False);  --  Workaround for No_Return: Pre=>False
            if True then  --  Workaround for No_Return placement limitation
               VTd_Error (IOMMU   => I,
                          Message => "unable to invalidate context cache");
            end if;
         end if;

         Flush_IOTLB (IOMMU   => I,
                      Success => Status);
         if not Status then
            pragma Assume (False);  --  Workaround for No_Return: Pre=>False
            if True then  --  Workaround for No_Return placement limitation
               VTd_Error (IOMMU   => I,
                          Message => "unable to flush IOTLB");
            end if;
         end if;

         Enable_Translation (IOMMU   => I,
                             Success => Status);
         if not Status then
            pragma Assume (False);  --  Workaround for No_Return: Pre=>False
            if True then  --  Workaround for No_Return placement limitation
               VTd_Error (IOMMU   => I,
                          Message => "error enabling translation");
            end if;
         end if;

         declare
            Dummy : Types.Reg_Global_Status_Type;
         begin
            pragma $Prove_Warnings (Off, "unused assignment");
            Dummy := IOMMUs (I).Global_Status;
            pragma $Prove_Warnings (On, "unused assignment");
            pragma Debug (VTd.Dump.Print_Global_Status
                          (IOMMU  => I,
                           Status => Dummy));
         end;
         pragma Debug
           (SK.Dump.Print_Message_8
              (Msg  => "VT-d DMA address translation enabled for IOMMU",
               Item => SK.Byte (I)));
      end loop;
   end Initialize;

end SK.VTd;
