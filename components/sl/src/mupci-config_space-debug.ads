--
--  Copyright (C) 2025  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2025  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package Mupci.Config_Space.Debug
is

   --  Print information about PCI device given by SID.
   procedure Print_PCI_Device_Info (SID : Musinfo.SID_Type);

   --  Print PCI capabilities of given device.
   procedure Print_PCI_Capabilities (SID : Musinfo.SID_Type);

   --  Print PCIe capability structure at given address.
   procedure Print_PCIe_Capability_Structure
     (Address : Interfaces.Unsigned_64);

   --  Return address of device PCI configuration space.
   function Mmconf_Address
     (SID : Musinfo.SID_Type)
      return Interfaces.Unsigned_64;

   --  Return address of device PCI configuration space register at
   --  given offset.
   function Mmconf_Register
     (SID    : Musinfo.SID_Type;
      Offset : Dev_Specific_Range)
      return Interfaces.Unsigned_64;

private

   function Mmconf_Address
     (SID : Musinfo.SID_Type)
      return Interfaces.Unsigned_64
   is (Mmconf_Base + Interfaces.Unsigned_64 (SID * SK.Page_Size));

   function Mmconf_Register
     (SID    : Musinfo.SID_Type;
      Offset : Dev_Specific_Range)
      return Interfaces.Unsigned_64
   is (Mmconf_Address (SID) + Interfaces.Unsigned_64 (Offset));

end Mupci.Config_Space.Debug;
