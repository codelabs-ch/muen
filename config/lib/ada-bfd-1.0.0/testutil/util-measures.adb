-----------------------------------------------------------------------
--  measure -- Benchmark tools
--  Copyright (C) 2008, 2009, 2010, 2011 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------
with Ada.Task_Attributes;
with Ada.Strings.Hash;
with Ada.Unchecked_Deallocation;
with GNAT.Calendar.Time_IO;

with Util.Streams.Buffered;
package body Util.Measures is

   ISO_DATE_TIME : constant GNAT.Calendar.Time_IO.Picture_String := "%Y-%m-%d %H:%M:%S";

   procedure Free is
     new Ada.Unchecked_Deallocation (Buckets_Type, Buckets_Access);

   procedure Free is
     new Ada.Unchecked_Deallocation (Measure, Measure_Access);

   procedure Free is
     new Ada.Unchecked_Deallocation (String, String_Access);

   package Task_Context is new Ada.Task_Attributes
     (Measure_Set_Access, null);

   function Format (D : Duration) return String;

   --  ------------------------------
   --  Disable collecting measures on the measure set.
   --  ------------------------------
   procedure Disable (Measures : in out Measure_Set) is
   begin
      Measures.Enabled := False;
   end Disable;

   --  ------------------------------
   --  Enable collecting measures on the measure set.
   --  ------------------------------
   procedure Enable (Measures : in out Measure_Set) is
   begin
      Measures.Enabled := True;
   end Enable;

   --  ------------------------------
   --  Set the per-thread measure set.
   --  ------------------------------
   procedure Set_Current (Measures : in Measure_Set_Access) is
   begin
      Task_Context.Set_Value (Measures);
   end Set_Current;

   --  ------------------------------
   --  Get the per-thread measure set.
   --  ------------------------------
   function Get_Current return Measure_Set_Access is
   begin
      return Task_Context.Value;
   end Get_Current;

   --  ------------------------------
   --  Dump an XML result with the measures collected by the measure set.
   --  When writing the measures, the measure set is cleared.  It is safe
   --  to write measures while other measures are being collected.
   --  ------------------------------
   procedure Write (Measures : in out Measure_Set;
                    Title    : in String;
                    Stream   : in out Util.Streams.Texts.Print_Stream'Class) is

      use Util.Streams.Buffered;

      procedure Dump_XML (Item : in Measure_Access);

      procedure Dump_XML (Item : in Measure_Access) is
         Time  : constant String := Format (Item.Time);
      begin
         Stream.Write ("<time count=""");
         Stream.Write (Item.Count);
         Stream.Write (""" time=""");
         Stream.Write (Time (Time'First + 1 .. Time'Last));
         Stream.Write (""" title=""");
         Util.Streams.Texts.TR.Escape_Java_Script (Into => Buffered_Stream (Stream),
                                                   Content => Item.Name.all);
         Stream.Write ("""/>");
         Stream.Write (ASCII.LF);
      end Dump_XML;

      Buckets : Buckets_Access;
      TS, TE  : Ada.Calendar.Time;

   begin
      Measures.Data.Steal_Map (Buckets, TS, TE);
      Stream.Write ("<measures title=""");
      Util.Streams.Texts.TR.Escape_Java_Script (Into    => Buffered_Stream (Stream),
                                                Content => Title);
      Stream.Write (""" start=""");
      Stream.Write (TS, ISO_DATE_TIME);
      Stream.Write (""" end=""");
      Stream.Write (TE, ISO_DATE_TIME);
      Stream.Write (""">");
      if Buckets /= null then
         begin
            for I in Buckets'Range loop
               declare
                  Next : Measure_Access;
                  Node : Measure_Access := Buckets (I);
               begin
                  while Node /= null loop
                     Dump_XML (Node);
                     Free (Node.Name);
                     Next := Node.Next;
                     Free (Node);
                     Node := Next;
                  end loop;
               end;
            end loop;

         exception
            when others =>
               Free (Buckets);
               raise;
         end;
         Free (Buckets);
      end if;
      Stream.Write ("</measures>");
   end Write;

   --  ------------------------------
   --  Dump an XML result with the measures collected by the measure set.
   --  ------------------------------
   procedure Write (Measures : in out Measure_Set;
                    Title    : in String;
                    Stream   : in Ada.Text_IO.File_Type) is

      Buffer : aliased Util.Streams.Buffered.Buffered_Stream;
      Output : Util.Streams.Texts.Print_Stream;

   begin
      Buffer.Initialize (Size => 128 * 1024);
      Output.Initialize (To => Buffer'Unchecked_Access);
      Write (Measures, Title, Output);
      Output.Flush;
      Ada.Text_IO.Put_Line (Stream, Util.Streams.Texts.To_String (Buffer));
   end Write;

   --  ------------------------------
   --  Dump  an XML result with the measures in a file.
   --  ------------------------------
   procedure Write (Measures : in out Measure_Set;
                    Title    : in String;
                    Path     : in String) is
      File : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (File => File, Name => Path);
      Write (Measures, Title, File);
      Ada.Text_IO.Close (File);
   exception
      when others =>
         if Ada.Text_IO.Is_Open (File) then
            Ada.Text_IO.Close (File);
         end if;
         raise;
   end Write;

   --  ------------------------------
   --  Report the time spent between the stamp creation and this method call.
   --  Collect the result in the per-thread measure set under the given measure
   --  title.
   --  ------------------------------
   procedure Report (S     : in out Stamp;
                     Title : in String) is
      Measures : constant Measure_Set_Access := Task_Context.Value;
   begin
      if Measures /= null and then Measures.Enabled then
         Report (Measures.all, S, Title);
      end if;
   end Report;

   --  ------------------------------
   --  Report the time spent between the stamp creation and this method call.
   --  Collect the result in the measure set under the given measure title.
   --  ------------------------------
   procedure Report (Measures : in out Measure_Set;
                     S        : in out Stamp;
                     Title    : in String) is
      use Ada.Calendar;
   begin
      if Measures.Enabled then
         declare
            D : constant Duration := Ada.Calendar.Clock - S.Start;
         begin
            Measures.Data.Add (Title, D);
         end;
         S.Start := Ada.Calendar.Clock;
      end if;
   end Report;

   protected body Measure_Data is

      --  ------------------------------
      --  Get the measures and clear to start a new set of measures.
      --  Return in <b>Time_Start</b> and <b>Time_End</b> the period of time.
      --  ------------------------------
      procedure Steal_Map (Result     : out Buckets_Access;
                           Time_Start : out Ada.Calendar.Time;
                           Time_End   : out Ada.Calendar.Time) is
      begin
         Result     := Buckets;
         Time_Start := Start;
         Start      := Ada.Calendar.Clock;
         Time_End   := Start;
         Buckets    := null;
      end Steal_Map;

      --  ------------------------------
      --  Add the measure
      --  ------------------------------
      procedure Add (Title : String; D : Duration) is

         use Ada.Containers;
         use Ada.Calendar;

         Pos  : Hash_Type;
         Node : Measure_Access;
      begin
         if Buckets = null then
            Buckets := new Buckets_Type (0 .. 256);
         end if;
         Pos := Ada.Strings.Hash (Title) mod Buckets'Length;
         Node := Buckets (Pos);
         while Node /= null loop
            if Node.Name'Length = Title'Length
              and then Node.Name.all = Title then
               Node.Count := Node.Count + 1;
               Node.Time := Node.Time + D;
               return;
            end if;
            Node := Node.Next;
         end loop;
         Buckets (Pos) := new Measure '(Name => new String '(Title),
                                        Time  => D,
                                        Count => 1,
                                        Next  => Buckets (Pos));
      end Add;

   end Measure_Data;

   --  ------------------------------
   --  Format the duration in a time in 'ns', 'us', 'ms' or seconds.
   --  ------------------------------
   function Format (D : Duration) return String is
   begin
      if D < 0.000_001 then
         return Duration'Image (D * 1_000_000_000) & "ns";
      elsif D < 0.001 then
         return Duration'Image (D * 1_000_000) & "us";
      elsif D < 1.0 then
         return Duration'Image (D * 1_000) & "ms";
      else
         return Duration'Image (D) & "s";
      end if;
   end Format;

   --  ------------------------------
   --  Finalize the measures and release the storage.
   --  ------------------------------
   overriding
   procedure Finalize (Measures : in out Measure_Set) is
      Buckets : Buckets_Access;
      TS, TE  : Ada.Calendar.Time;
   begin
      --  When deleting the measure set, we have to release the buckets and measures
      --  that were allocated.  We could call <b>Write</b> but we don't know where
      --  the measures have to be written.
      Measures.Data.Steal_Map (Buckets, TS, TE);
      if Buckets /= null then
         for I in Buckets'Range loop
            declare
               Next : Measure_Access;
               Node : Measure_Access := Buckets (I);
            begin
               while Node /= null loop
                  Free (Node.Name);
                  Next := Node.Next;
                  Free (Node);
                  Node := Next;
               end loop;
            end;
         end loop;
         Free (Buckets);
      end if;
   end Finalize;

end Util.Measures;
