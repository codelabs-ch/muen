--
--  Copyright (C) 2019  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2019  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with DOM.Core;
with McKae.XML.XPath.XIA;

with Cmd_Stream.Utils;
with Cmd_Stream.Memblocks;
with Cmd_Stream.Processors;
with Cmd_Stream.Devices;
with Cmd_Stream.Roots.Device_Domains;
with Cmd_Stream.Roots.Kernels;
with Cmd_Stream.Roots.Memory;
with Cmd_Stream.Roots.Subjects;
with Cmd_Stream.Constants;

package body Cmd_Stream
is

   -------------------------------------------------------------------------

   procedure Run
     (Policy      : in out Muxml.XML_Data_Type;
      Output_File :        String)
   is
      Stream_Doc : Utils.Stream_Document_Type;

      Physical_Mem : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/memory/memory");
      Physical_Devs : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/hardware/devices/device");
   begin
      Utils.Create
        (Stream_Doc => Stream_Doc,
         Filename   => Output_File);

      Memblocks.Create_Memory_Blocks
        (Policy     => Policy,
         Stream_Doc => Stream_Doc);

      Processors.Create_Processors
        (Policy     => Policy,
         Stream_Doc => Stream_Doc);

      Devices.Create_Physical_Legacy_Devices
        (Policy     => Policy,
         Stream_Doc => Stream_Doc);
      Devices.Create_Physical_PCI_Devices
        (Policy     => Policy,
         Stream_Doc => Stream_Doc);
      Devices.Create_VTd_Tables
        (Policy     => Policy,
         Stream_Doc => Stream_Doc);

      Utils.Append_Command
        (Stream_Doc => Stream_Doc,
         Name       => "activateTau0");

      Roots.Memory.Create_Memory_Regions
        (Stream_Doc  => Stream_Doc,
         Phys_Memory => Physical_Mem);

      Roots.Kernels.Create_Per_CPU_Kernel
        (Policy     => Policy,
         Stream_Doc => Stream_Doc);

      Roots.Subjects.Create_Subjects
        (Policy     => Policy,
         Stream_Doc => Stream_Doc,
         Phys_Mem   => Physical_Mem,
         Phys_Devs  => Physical_Devs);

      Roots.Device_Domains.Create
        (Policy     => Policy,
         Stream_Doc => Stream_Doc,
         Phys_Mem   => Physical_Mem,
         Phys_Devs  => Physical_Devs);

      Utils.Append_Command
        (Stream_Doc => Stream_Doc,
         Name       => "writeImage");
      Utils.Append_Command
        (Stream_Doc => Stream_Doc,
         Name       => "writeImageCmdStream",
         Attrs      =>
           (1 => (Attr  => U ("entryPoint"),
                  Value => U (Constants.Kernel_Entry_Point))));

      Utils.Close (Stream_Doc => Stream_Doc);
   end Run;

end Cmd_Stream;
