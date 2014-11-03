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

with DOM.Core.Elements;
with DOM.Core.Nodes;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.XML_Utils;
with Mutools.Templates;

with String_Templates;

package body Spec.Scheduling
is

   use Ada.Strings.Unbounded;

   -------------------------------------------------------------------------

   procedure Write_Spec_File
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Subjects     : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/subjects/subject");
      Scheduling   : constant DOM.Core.Node := Muxml.Utils.Get_Element
        (Doc   => Policy.Doc,
         XPath => "/system/scheduling");
      Processor    : constant DOM.Core.Node := Muxml.Utils.Get_Element
        (Doc   => Policy.Doc,
         XPath => "/system/platform/processor");
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
      CPU_Count    : constant Natural
        := Mutools.XML_Utils.Get_Active_CPU_Count (Data => Policy);

      Major_Count       : Positive;
      Max_Minor_Count   : Positive;
      Max_Barrier_Count : Natural;
      Majors            : DOM.Core.Node_List;
      Buffer            : Unbounded_String;
      Tmpl              : Mutools.Templates.Template_Type;

      --  Returns the maximum count of barriers per major frame.
      function Get_Max_Barrier_Count (Schedule : DOM.Core.Node) return Natural;

      --  Returns the maximum count of minor frames per major frame.
      function Get_Max_Minor_Count (Schedule : DOM.Core.Node) return Positive;

      --  Write major frame with given index and minor frames to buffer.
      procedure Write_Major_Frame
        (Index  : Natural;
         Minors : DOM.Core.Node_List);

      --  Write minor frame with given index to buffer.
      procedure Write_Minor_Frame
        (Minor : DOM.Core.Node;
         Index : Natural);

      ----------------------------------------------------------------------

      function Get_Max_Barrier_Count (Schedule : DOM.Core.Node) return Natural
      is
         Majors   : DOM.Core.Node_List;
         Barriers : DOM.Core.Node_List;
         Count    : Natural := 0;
      begin
         Majors := McKae.XML.XPath.XIA.XPath_Query
           (N     => Schedule,
            XPath => "majorFrame");

         for I in 0 .. DOM.Core.Nodes.Length (List => Majors) - 1 loop
            Barriers := McKae.XML.XPath.XIA.XPath_Query
              (N     => DOM.Core.Nodes.Item (List  => Majors,
                                             Index => I),
               XPath => "barriers/barrier");

            declare
               Cur_Count : constant Natural := DOM.Core.Nodes.Length
                 (List => Barriers);
            begin
               if Cur_Count > Count then
                  Count := Cur_Count;
               end if;
            end;
         end loop;

         return Count;
      end Get_Max_Barrier_Count;

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

      ----------------------------------------------------------------------

      procedure Write_Minor_Frame
        (Minor : DOM.Core.Node;
         Index : Natural)
      is
         Ticks   : constant Long_Integer := Timer_Factor * Long_Integer'Value
           (DOM.Core.Elements.Get_Attribute
              (Elem => Minor,
               Name => "ticks"));
         Barrier : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Minor,
              Name => "barrier");

         Subject    : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Minor,
            Name => "subject");
         Subject_Id : constant String := Muxml.Utils.Get_Attribute
           (Nodes     => Subjects,
            Ref_Attr  => "name",
            Ref_Value => Subject,
            Attr_Name => "id");
      begin
         Buffer := Buffer & Indent (N => 4) & Index'Img
           & " => Minor_Frame_Type'(Subject_Id => " & Subject_Id
           & ", Ticks =>" & Ticks'Img & "," & ASCII.LF;
         Buffer := Buffer & Indent (N => 12) & "Barrier    => "
           & (if Barrier = "none" then "No_Barrier" else Barrier) & ")";
      end Write_Minor_Frame;
   begin
      Mulog.Log (Msg => "Writing scheduling spec for" & CPU_Count'Img
                 & " CPUs to '" & Output_Dir & "/skp-scheduling.ads'");

      Majors := McKae.XML.XPath.XIA.XPath_Query
        (N     => Scheduling,
         XPath => "majorFrame");

      Major_Count       := DOM.Core.Nodes.Length (List => Majors);
      Max_Minor_Count   := Get_Max_Minor_Count (Schedule => Scheduling);
      Max_Barrier_Count := Get_Max_Barrier_Count (Schedule => Scheduling);

      for CPU in 0 .. CPU_Count - 1 loop
         Buffer := Buffer & Indent
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

      Tmpl := Mutools.Templates.Create
        (Content => String_Templates.skp_scheduling_ads);
      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__minor_range__",
         Content  => "1 .." & Max_Minor_Count'Img);
      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__major_range__",
         Content  => "0 .." & Natural'Image (Major_Count - 1));
      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__scheduling_plans__",
         Content  => To_String (Buffer));
      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__max_barrier_count__",
         Content  => Ada.Strings.Fixed.Trim
           (Source => Max_Barrier_Count'Img,
            Side   => Ada.Strings.Left));

      Mutools.Templates.Write
        (Template => Tmpl,
         Filename => Output_Dir & "/skp-scheduling.ads");
   end Write_Spec_File;

end Spec.Scheduling;
