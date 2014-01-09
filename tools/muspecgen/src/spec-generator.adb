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

with Ada.Strings.Unbounded;

with DOM.Core.Elements;
with DOM.Core.Nodes;

with McKae.XML.XPath.XIA;

with Spec.Templates;
with Mulog;

package body Spec.Generator
is

   use Ada.Strings.Unbounded;

   --  Return N number of indentation spaces.
   function Indent (N : Positive := 1) return String;

   --  Write scheduling-related policy file to specified output directory.
   procedure Write_Scheduling
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type);

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

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
   begin
      Write_Scheduling (Output_Dir => Output_Dir,
                        Policy     => Policy);
   end Write;

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
            Subject_Id : constant String := DOM.Core.Nodes.Node_Value
              (N => DOM.Core.Nodes.Item
                 (List  => McKae.XML.XPath.XIA.XPath_Query
                    (N     => Policy.Doc,
                     XPath => "/system/subjects/subject[@name='" & Subject
                     & "']/@id"),
                  Index => 0));
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
