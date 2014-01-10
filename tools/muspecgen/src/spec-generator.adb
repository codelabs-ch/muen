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

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Long_Long_Integer_Text_IO;

with DOM.Core.Elements;
with DOM.Core.Nodes;

with McKae.XML.XPath.XIA;

with Spec.Templates;
with Mulog;

package body Spec.Generator
is

   use Ada.Strings.Unbounded;

   --  Searches the element specified by an XPath in the given document and
   --  returns the attribute given by name as string. If no such attribute or
   --  element exists, an empty string is returned.
   function Get_Attribute
     (Doc   : DOM.Core.Node;
      XPath : String;
      Name  : String)
      return String;

   --  Return N number of indentation spaces.
   function Indent (N : Positive := 1) return String;

   --  Return hexadecimal representation of given number. If prefix is True,
   --  the returned string includes the base (16#..#).
   function To_Hex
     (Number : Long_Long_Integer;
      Prefix : Boolean := True)
      return String;

   --  Write interrupt policy file to specified output directory.
   procedure Write_Interrupts
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type);

   --  Write kernel-related policy files to specified output directory.
   procedure Write_Kernel
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type);

   --  Write scheduling-related policy file to specified output directory.
   procedure Write_Scheduling
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type);

   -------------------------------------------------------------------------

   function Get_Attribute
     (Doc   : DOM.Core.Node;
      XPath : String;
      Name  : String)
      return String
   is
      Node : constant DOM.Core.Node := DOM.Core.Nodes.Item
        (List  => McKae.XML.XPath.XIA.XPath_Query
           (N     => Doc,
            XPath => XPath),
         Index => 0);
   begin
      return DOM.Core.Elements.Get_Attribute
        (Elem => Node,
         Name => Name);
   end Get_Attribute;

   -------------------------------------------------------------------------

   function Indent (N : Positive := 1) return String
   is
      Indent : constant String := "   ";
      Result : Unbounded_String;
   begin
      for I in Positive range 1 .. N loop
         Result := Result & Indent;
      end loop;

      return To_String (Result);
   end Indent;

   -------------------------------------------------------------------------

   function To_Hex
     (Number : Long_Long_Integer;
      Prefix : Boolean := True)
      return String
   is
      Num_Str : String (1 .. 20);
   begin
      Ada.Long_Long_Integer_Text_IO.Put
        (To   => Num_Str,
         Item => Number,
         Base => 16);

      declare
         Trimmed : constant String := Ada.Strings.Fixed.Trim
           (Source => Num_Str,
            Side   => Ada.Strings.Left);
      begin
         if Prefix then
            return Trimmed;
         else
            return Trimmed (Trimmed'First + 3 .. Trimmed'Last - 1);
         end if;
      end;
   end To_Hex;

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
   begin
      Write_Scheduling (Output_Dir => Output_Dir,
                        Policy     => Policy);
      Write_Interrupts (Output_Dir => Output_Dir,
                        Policy     => Policy);
      Write_Kernel (Output_Dir => Output_Dir,
                    Policy     => Policy);
   end Write;

   -------------------------------------------------------------------------

   procedure Write_Interrupts
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is

      --  IRQ to host Vector offset.
      Vector_Offset : constant := 48;

      Subjects  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/subjects/subject[count(devices/device/irq)>0]");
      IRQ_Count : constant Natural := DOM.Core.Nodes.Length
        (List => McKae.XML.XPath.XIA.XPath_Query
           (N     => Policy.Doc,
            XPath => "/system/subjects/subject/devices/device/irq"));

      Cur_IRQ : Positive := 1;

      IRQ_Buffer, Vector_Buffer : Unbounded_String;

      --  Write IRQ information to interrupts spec.
      procedure Write_Interrupt
        (IRQ   : DOM.Core.Node;
         Owner : DOM.Core.Node;
         Index : Natural);

      ----------------------------------------------------------------------

      procedure Write_Interrupt
        (IRQ   : DOM.Core.Node;
         Owner : DOM.Core.Node;
         Index : Natural)
      is
         IRQ_Nr : constant Natural := Natural'Value
           (DOM.Core.Elements.Get_Attribute
              (Elem => IRQ,
               Name => "number"));
         CPU    : constant Natural := Natural'Value
           (DOM.Core.Elements.Get_Attribute
              (Elem => Owner,
               Name => "cpu"));
      begin

         --  IRQ routing table.

         IRQ_Buffer := IRQ_Buffer & Indent (N => 2)
           & Index'Img & " => IRQ_Route_Type'("
           & ASCII.LF
           & Indent (N => 3) & "CPU    =>" & CPU'Img
           & "," & ASCII.LF
           & Indent (N => 3) & "IRQ    =>" & IRQ_Nr'Img
           & "," & ASCII.LF
           & Indent (N => 3) & "Vector =>"
           & Positive'Image (Vector_Offset + IRQ_Nr) & ")";

         --  Vector -> subject routing table.

         Vector_Buffer := Vector_Buffer & Indent (N => 2)
           & Positive'Image (Vector_Offset + IRQ_Nr) & " => "
           & DOM.Core.Elements.Get_Attribute (Elem => Owner,
                                              Name => "id");
      end Write_Interrupt;

      Tmpl : Templates.Template_Type;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            Subject : constant DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => Subjects,
               Index => I);
            IRQs    : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Subject,
                 XPath => "devices/device/irq");
         begin
            for IRQ in 1 .. DOM.Core.Nodes.Length (List => IRQs) loop
               Write_Interrupt
                 (IRQ   => DOM.Core.Nodes.Item (List  => IRQs,
                                                Index => IRQ - 1),
                  Owner => Subject,
                  Index => Cur_IRQ);

               if Cur_IRQ /= IRQ_Count then
                  IRQ_Buffer    := IRQ_Buffer    & "," & ASCII.LF;
                  Vector_Buffer := Vector_Buffer & "," & ASCII.LF;
               end if;

               Cur_IRQ := Cur_IRQ + 1;
            end loop;
         end;
      end loop;

      Vector_Buffer := Vector_Buffer & ","
        & ASCII.LF & Indent (N => 2) & " others => Skp.Invalid_Subject";

      Tmpl := Templates.Load (Filename => "skp-interrupts.ads");
      Templates.Replace (Template => Tmpl,
                         Pattern  => "__routing_range__",
                         Content  => "1 .." & IRQ_Count'Img);
      Templates.Replace (Template => Tmpl,
                         Pattern  => "__irq_routing_table__",
                         Content  => To_String (IRQ_Buffer));
      Templates.Replace (Template => Tmpl,
                         Pattern  => "__vector_routing_table__",
                         Content  => To_String (Vector_Buffer));

      Mulog.Log (Msg => "Writing interrupt routing spec to '"
                 & Output_Dir & "/skp-interrupts.ads'");

      Templates.Write (Template => Tmpl,
                       Filename => Output_Dir & "/skp-interrupts.ads");
   end Write_Interrupts;

   -------------------------------------------------------------------------

   procedure Write_Kernel
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Stack_Size : constant Long_Long_Integer := 2 * 4096;
      Stack_Addr : constant Long_Long_Integer := Long_Long_Integer'Value
        (Get_Attribute
           (Doc   => Policy.Doc,
            XPath => "/system/kernel/memory/cpu[@id='0']/"
            & "memory[@logical='stack']",
            Name  => "virtualAddress")) + Stack_Size;

      CPU_Store_Addr : constant Long_Long_Integer := Long_Long_Integer'Value
        (Get_Attribute
           (Doc   => Policy.Doc,
            XPath => "/system/kernel/memory/cpu[@id='0']/"
            & "memory[@logical='store']",
            Name  => "virtualAddress"));

      Addr_Str : String (1 .. 20);
      Tmpl     : Templates.Template_Type;
   begin
      Tmpl := Templates.Load (Filename => "skp-kernel.ads");

      Ada.Long_Long_Integer_Text_IO.Put
        (To   => Addr_Str,
         Item => Stack_Addr,
         Base => 16);
      Templates.Replace
        (Template => Tmpl,
         Pattern  => "__stack_addr__",
         Content  => Ada.Strings.Fixed.Trim (Source => Addr_Str,
                                             Side   => Ada.Strings.Left));

      Ada.Long_Long_Integer_Text_IO.Put
        (To   => Addr_Str,
         Item => CPU_Store_Addr,
         Base => 16);
      Templates.Replace
        (Template => Tmpl,
         Pattern  => "__cpu_store_addr__",
         Content  => Ada.Strings.Fixed.Trim (Source => Addr_Str,
                                             Side   => Ada.Strings.Left));

      Mulog.Log (Msg => "Writing kernel spec to '"
                 & Output_Dir & "/skp-kernel.ads'");

      Templates.Write (Template => Tmpl,
                       Filename => Output_Dir & "/skp-kernel.ads");
   end Write_Kernel;

   -------------------------------------------------------------------------

   procedure Write_Scheduling
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Scheduling   : constant DOM.Core.Node := DOM.Core.Nodes.Item
        (List => McKae.XML.XPath.XIA.XPath_Query
           (N     => Policy.Doc,
            XPath => "/system/scheduling"),
         Index => 0);
      Processor    : constant DOM.Core.Node := DOM.Core.Nodes.Item
        (List => McKae.XML.XPath.XIA.XPath_Query
           (N     => Policy.Doc,
            XPath => "/system/platform/processor"),
         Index => 0);
      CPU_Speed_Hz : constant Long_Integer :=  1_000_000 * Long_Integer'Value
        (DOM.Core.Elements.Get_Attribute (Elem => Processor,
                                          Name => "speed"));
      Timer_Rate   : constant Long_Integer := 2 ** Natural'Value
        (DOM.Core.Elements.Get_Attribute (Elem => Processor,
                                          Name => "vmxTimerRate"));
      Timer_Factor : constant Long_Integer := CPU_Speed_Hz /
        (Timer_Rate * Long_Integer'Value (DOM.Core.Elements.Get_Attribute
         (Elem => Scheduling,
          Name => "tickRate")));
      CPU_Count    : constant Natural      := Natural'Value
        (DOM.Core.Elements.Get_Attribute (Elem => Processor,
                                          Name => "logicalCpus"));

      Major_Count     : Positive;
      Max_Minor_Count : Positive;
      Majors          : DOM.Core.Node_List;
      Buffer          : Unbounded_String;
      Tmpl            : Templates.Template_Type;

      --  Returns the maximum count of minor frames per major frame.
      function Get_Max_Minor_Count (Schedule : DOM.Core.Node) return Positive;

      --  Write major frame with given index and minor frames to buffer.
      procedure Write_Major_Frame
        (Index  : Natural;
         Minors : DOM.Core.Node_List);

      ----------------------------------------------------------------------

      function Get_Max_Minor_Count (Schedule : DOM.Core.Node) return Positive
      is
         CPUs   : DOM.Core.Node_List;
         Minors : DOM.Core.Node_List;
         Count  : Positive := 1;
      begin
         CPUs := McKae.XML.XPath.XIA.XPath_Query
           (N     => Schedule,
            XPath => "majorFrame/cpu");

         for I in 0 .. DOM.Core.Nodes.Length (List => CPUs) - 1 loop
            Minors := McKae.XML.XPath.XIA.XPath_Query
              (N     => DOM.Core.Nodes.Item (List  => CPUs,
                                             Index => I),
               XPath => "minorFrame");

            if DOM.Core.Nodes.Length (List => Minors) > Count then
               Count := DOM.Core.Nodes.Length (List => Minors);
            end if;
         end loop;

         return Count;
      end Get_Max_Minor_Count;

      ----------------------------------------------------------------------

      procedure Write_Major_Frame
        (Index  : Natural;
         Minors : DOM.Core.Node_List)
      is
         Minor_Count : constant Positive := DOM.Core.Nodes.Length
           (List => Minors);

         --  Write minor frame with given index to buffer.
         procedure Write_Minor_Frame
           (Minor : DOM.Core.Node;
            Index : Natural);

         -------------------------------------------------------------------

         procedure Write_Minor_Frame
           (Minor : DOM.Core.Node;
            Index : Natural)
         is
            Ticks : constant Long_Integer := Timer_Factor * Long_Integer'Value
              (DOM.Core.Elements.Get_Attribute
                 (Elem => Minor,
                  Name => "ticks"));

            Subject    : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Minor,
               Name => "subject");
            Subject_Id : constant String := Get_Attribute
              (Doc   => Policy.Doc,
               XPath => "/system/subjects/subject[@name='" & Subject & "']",
               Name  => "id");
         begin
            Buffer := Buffer & Indent (N => 4) & Index'Img
              & " => Minor_Frame_Type'(Subject_Id => " & Subject_Id
              & ", Ticks =>" & Ticks'Img & ")";
         end Write_Minor_Frame;
      begin
         Buffer := Buffer & Indent (N => 2)
           & Index'Img & " => Major_Frame_Type'"
           & ASCII.LF & Indent (N => 3)
           & "(Length       =>" & Minor_Count'Img & ","
           & ASCII.LF & Indent (N => 3)
           & " Minor_Frames => Minor_Frame_Array'("
           & ASCII.LF;

         for I in 1 .. Minor_Count loop
            Write_Minor_Frame (Minor => DOM.Core.Nodes.Item
                               (List  => Minors,
                                Index => I - 1),
                               Index => I);

            if I < Minor_Count then
               Buffer := Buffer & "," & ASCII.LF;
            end if;
         end loop;

         if Minor_Count < Max_Minor_Count then
            Buffer := Buffer & "," & ASCII.LF & Indent (N => 3)
              & Indent & " others => Null_Minor_Frame";
         end if;

         Buffer := Buffer & "))";
      end Write_Major_Frame;
   begin
      Majors := McKae.XML.XPath.XIA.XPath_Query
        (N     => Scheduling,
         XPath => "majorFrame");

      Major_Count     := DOM.Core.Nodes.Length (List => Majors);
      Max_Minor_Count := Get_Max_Minor_Count (Schedule => Scheduling);

      for CPU in 0 .. CPU_Count - 1 loop
         Buffer    := Buffer & Indent
           & " " & CPU'Img & " => Major_Frame_Array'("
           & ASCII.LF;

         for I in 0 .. Major_Count - 1 loop
            declare
               Major      : constant DOM.Core.Node := DOM.Core.Nodes.Item
                 (List  => Majors,
                  Index => I);
               Major_CPUs : constant DOM.Core.Node_List
                 := McKae.XML.XPath.XIA.XPath_Query
                   (N     => Major,
                    XPath => "cpu");
               Minors     : constant DOM.Core.Node_List
                 := McKae.XML.XPath.XIA.XPath_Query
                   (N     => DOM.Core.Nodes.Item
                        (List  => Major_CPUs,
                         Index => CPU),
                    XPath => "minorFrame");
            begin
               Write_Major_Frame (Minors => Minors,
                                  Index  => I);

               if I < Major_Count - 1 then
                  Buffer := Buffer & "," & ASCII.LF;
               end if;
            end;
         end loop;

         Buffer := Buffer & ")";

         if CPU < CPU_Count - 1 then
            Buffer := Buffer & "," & ASCII.LF;
         end if;
      end loop;

      Tmpl := Templates.Load (Filename  => "skp-scheduling.ads");
      Templates.Replace (Template => Tmpl,
                         Pattern  => "__minor_range__",
                         Content  => "1 .." & Max_Minor_Count'Img);
      Templates.Replace (Template => Tmpl,
                         Pattern  => "__major_range__",
                         Content  => "0 .." & Natural'Image (Major_Count - 1));
      Templates.Replace (Template => Tmpl,
                         Pattern  => "__scheduling_plans__",
                         Content  => To_String (Buffer));

      Mulog.Log (Msg => "Writing scheduling spec for" & CPU_Count'Img
                 & " CPUs to '" & Output_Dir & "/skp-scheduling.ads'");

      Templates.Write (Template => Tmpl,
                       Filename => Output_Dir & "/skp-scheduling.ads");
   end Write_Scheduling;

end Spec.Generator;
