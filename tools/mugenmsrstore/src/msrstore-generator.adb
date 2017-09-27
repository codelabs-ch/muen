--
--  Copyright (C) 2014, 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014, 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Interfaces;

with Mulog;
with Muxml.Utils;
with Mutools.Files;
with Mutools.Utils;
with Mutools.XML_Utils;

with Msrstore.Tables;

package body Msrstore.Generator
is

   function U
     (Source : String)
      return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   --  Generate MSR store for given registers with specified control flags and
   --  write to specified file.
   procedure Write_MSR_Store
     (Registers              : DOM.Core.Node_List;
      Register_Count         : Positive;
      Filename               : String;
      DEBUGCTL_Control       : Boolean;
      PAT_Control            : Boolean;
      PERFGLOBALCTRL_Control : Boolean;
      EFER_Control           : Boolean);

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Phys_Mem :  constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/memory/memory");
      Subjects : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/subjects/subject[vcpu/registers/msrs/msr]");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            Cur_Subj   : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subjects,
                 Index => I);
            Subj_Name  : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Cur_Subj,
                 Name => "name");
            Registers  : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Cur_Subj,
                 XPath => "vcpu/registers/msrs/msr[@mode='w' or @mode='rw']");
            Ctrls_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Cur_Subj,
                 XPath => "vcpu/vmx/controls");
            Debug_Ctrl : constant Boolean
              := Mutools.XML_Utils.Has_Managed_DEBUGCTL
                (Controls => Ctrls_Node);
            PERF_Ctrl  : constant Boolean
              := Mutools.XML_Utils.Has_Managed_PERFGLOBALCTRL
                (Controls => Ctrls_Node);
            PAT_Ctrl   : constant Boolean
              := Mutools.XML_Utils.Has_Managed_PAT (Controls => Ctrls_Node);
            EFER_Ctrl  : constant Boolean
              := Mutools.XML_Utils.Has_Managed_EFER (Controls => Ctrls_Node);
            MSR_Count  : constant Natural
              := Mutools.XML_Utils.Calculate_MSR_Count
                (MSRs                   => Registers,
                 DEBUGCTL_Control       => Debug_Ctrl,
                 PAT_Control            => PAT_Ctrl,
                 PERFGLOBALCTRL_Control => PERF_Ctrl,
                 EFER_Control           => EFER_Ctrl);
         begin
            if MSR_Count > 0 then
               declare
                  Msrstore : constant DOM.Core.Node
                    := Muxml.Utils.Get_Element
                      (Nodes => Phys_Mem,
                       Refs  => ((Name  => U ("type"),
                                  Value => U ("kernel_msrstore")),
                                 (Name  => U ("name"),
                                  Value => U (Subj_Name & "|msrstore"))));
                  Filename : constant String
                    := Output_Dir & "/" & Muxml.Utils.Get_Attribute
                      (Doc   => Msrstore,
                       XPath => "file",
                       Name  => "filename");
               begin
                  Mulog.Log
                    (Msg => "Writing MSR store with" & MSR_Count'Img
                     & " registers for subject '" & Subj_Name & "' to '"
                     & Filename & "'");
                  Write_MSR_Store
                    (Registers              => Registers,
                     Register_Count         => MSR_Count,
                     Filename               => Filename,
                     DEBUGCTL_Control       => Debug_Ctrl,
                     PAT_Control            => PAT_Ctrl,
                     PERFGLOBALCTRL_Control => PERF_Ctrl,
                     EFER_Control           => EFER_Ctrl);
               end;
            end if;
         end;
      end loop;
   end Write;

   -------------------------------------------------------------------------

   procedure Write_MSR_Store
     (Registers              : DOM.Core.Node_List;
      Register_Count         : Positive;
      Filename               : String;
      DEBUGCTL_Control       : Boolean;
      PAT_Control            : Boolean;
      PERFGLOBALCTRL_Control : Boolean;
      EFER_Control           : Boolean)
   is
      File  : Ada.Streams.Stream_IO.File_Type;
      Store : Tables.MSR_Store_Type
        (Size => Tables.MSR_Store_Size (Register_Count));
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Registers) - 1 loop
         declare
            Cur_MSR     : constant DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => Registers,
               Index => I);
            Start_Addr  : constant Interfaces.Unsigned_32
              := Interfaces.Unsigned_32'Value
                (DOM.Core.Elements.Get_Attribute (Elem => Cur_MSR,
                                                  Name => "start"));
            End_Addr    : constant Interfaces.Unsigned_32
              := Interfaces.Unsigned_32'Value
                (DOM.Core.Elements.Get_Attribute (Elem => Cur_MSR,
                                                  Name => "end"));
         begin
            for Reg in Start_Addr .. End_Addr loop
               if not Mutools.Utils.Is_Managed_By_VMX
                 (MSR                    => Interfaces.Unsigned_64 (Reg),
                  DEBUGCTL_Control       => DEBUGCTL_Control,
                  PAT_Control            => PAT_Control,
                  PERFGLOBALCTRL_Control => PERFGLOBALCTRL_Control,
                  EFER_Control           => EFER_Control)
               then
                  Tables.Append_Entry (Store => Store,
                                       Index => Reg,
                                       Data  => 0);
               end if;
            end loop;
         end;
      end loop;

      Mutools.Files.Open (Filename => Filename,
                          File     => File);
      Ada.Streams.Stream_IO.Write
        (File => File,
         Item => Tables.To_Stream (Store => Store));
      Ada.Streams.Stream_IO.Close (File => File);
   end Write_MSR_Store;

end Msrstore.Generator;
