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

with Interfaces;

with DOM.Core;

with McKae.XML.XPath.XIA;

with Muxml;
with Mutools.Utils;

with Spec.Utils;
pragma Elaborate_All (Spec.Utils);

package body Utils_Tests
is

   use Ahven;
   use Spec;

   type Exceptions_Type is
     (DivideError,
      NMI,
      Breakpoint,
      Overflow,
      BOUNDRangeExceeded,
      InvalidOpcode,
      DeviceNotAvailable,
      DoubleFault,
      CoprocessorSegmentOverrun,
      InvalidTSS,
      SegmentNotPresent,
      StackSegmentFault,
      GeneralProtection,
      PageFault,
      x87FPUFloatingPointError,
      AlignmentCheck,
      MachineCheck,
      SIMDFloatingPointException);

   type Exceptions_Map_Type is array (Exceptions_Type)
     of Mutools.Utils.Unsigned_64_Pos;

   function Get_Exceptions is new Utils.To_Number
     (Bitfield_Type => Exceptions_Type,
      Mapping_Type  => Exceptions_Map_Type,
      Map           =>
        (DivideError                => 0,
         NMI                        => 2,
         Breakpoint                 => 3,
         Overflow                   => 4,
         BOUNDRangeExceeded         => 5,
         InvalidOpcode              => 6,
         DeviceNotAvailable         => 7,
         DoubleFault                => 8,
         CoprocessorSegmentOverrun  => 9,
         InvalidTSS                 => 10,
         SegmentNotPresent          => 11,
         StackSegmentFault          => 12,
         GeneralProtection          => 13,
         PageFault                  => 14,
         x87FPUFloatingPointError   => 16,
         AlignmentCheck             => 17,
         MachineCheck               => 18,
         SIMDFloatingPointException => 19));

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Utils tests");
      T.Add_Test_Routine
        (Routine => To_Number'Access,
         Name    => "Convert flags to number");
   end Initialize;

   -------------------------------------------------------------------------

   procedure To_Number
   is
      use type Interfaces.Unsigned_64;

      Policy     : Muxml.XML_Data_Type;
      Exceptions : DOM.Core.Node_List;
   begin
      Muxml.Parse (Data => Policy,
                   File => "data/test_policy.xml");
      Exceptions := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/subjects/subject[@name='subject1']"
         & "/vcpu/vmx/masks/exception/*");

      Assert (Condition => Get_Exceptions (Fields => Exceptions) = 0,
              Message   => "Result mismatch");
   end To_Number;

end Utils_Tests;
