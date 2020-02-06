--
--  Copyright (C) 2017  secunet Security Networks AG
--  Copyright (C) 2020  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2020  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.Hypercall;

with Musinfo.Instance;
with Musinfo.Utils;

with Dbg.Buffers;
with Dbg.Byte_Queue.Format;
with Dbg.Channels;
with Dbg.String_Utils;
with Dbg.Subject_List;

with Dbgserver_Component.Channel_Arrays;
with Dbgserver_Component.Config;
with Dbgserver_Component.Events;

package body Dbg.Consoles
is

   package Cspecs renames Dbgserver_Component.Channel_Arrays;
   package Cspecs_Cfg renames Dbgserver_Component.Config;
   package Cspecs_Ev renames Dbgserver_Component.Events;

   Command_Prompt          : constant String := "$ ";
   Invalid_Musinfo_Message : constant String := "Error reading subject info";

   --  Returns True if the given address is part of a log channel.
   function Is_Log_Channel (Address : Interfaces.Unsigned_64) return Boolean;

   --  Computes the index for the subject_buffer with given memory address.
   function Get_Channel_ID
     (Address : Interfaces.Unsigned_64)
      return Subject_Buffer_Range
   with
      Pre => Is_Log_Channel (Address => Address);

   --  Enable output for log buffer with specified channel name.
   procedure Enable_Log_Buffer
     (Queue   : in out Byte_Queue.Queue_Type;
      Name    :        String;
      Success :    out Boolean);

   --  Parses the command in given buffer. Buffer_End is the index of one
   --  past the last command buffer character.
   function Parse_Command
     (Buffer     : Command_Buffer_Type;
      Buffer_End : Natural)
      return Command_Type;

   subtype Command_String_Length is Natural range 0 .. 2;
   subtype Command_String_Range  is Command_String_Length range
     Command_String_Length'First + 1 .. Command_String_Length'Last;
   subtype Command_String        is String (Command_String_Range);
   subtype Command_Description   is String (1 .. 30);

   type Command_Descriptor_Type is record
      Cmd_Str     : Command_String;
      Cmd_Len     : Command_String_Length;
      Command     : Command_Type;
      Has_Param   : Boolean;
      Description : Command_Description;
   end record;

   Null_Descr : constant Command_Description := (others => ASCII.NUL);

   Null_Command : constant Command_Descriptor_Type
     := (Cmd_Str     => (others => ASCII.NUL),
         Cmd_Len     => 0,
         Command     => Command_Type'(Kind => Failure),
         Has_Param   => False,
         Description => Null_Descr);

   --  Convert given string to command description.
   function To_Cmd_Descr (Descr : String) return Command_Description
   with
      Pre => Descr'Length <= Command_Description'Length
   is
      Result : Command_Description := (others => ASCII.NUL);
      Last   : constant Natural := Natural'Min (Descr'Length, Result'Last);
   begin
      for I in Result'First .. Last loop
         Result (I) := Descr (Descr'First - 1 + I);
      end loop;
      return Result;
   end To_Cmd_Descr;

   --  Description of all supported commands.
   Cmd_Descs : constant array (Natural range <>) of Command_Descriptor_Type
     := ((Cmd_Str     => ("  "),
          Cmd_Len     => 0,
          Command     => Command_Type'(Kind => Show_Prompt),
          Has_Param   => False,
          Description => Null_Descr),
         (Cmd_Str     => ("h "),
          Cmd_Len     => 1,
          Command     => Command_Type'(Kind => Print_Help),
          Has_Param   => False,
          Description => To_Cmd_Descr ("Print help message")),
         (Cmd_Str     => (ASCII.ESC, ' '),
          Cmd_Len     => 1,
          Command     => Command_Type'(Kind => Clear_Line),
          Has_Param   => False,
          Description => To_Cmd_Descr ("Clear line")),
         (Cmd_Str     => "rb",
          Cmd_Len     => 2,
          Command     => Command_Type'(Kind => Reboot),
          Has_Param   => False,
          Description => To_Cmd_Descr ("System Reboot")),
         (Cmd_Str     => "sd",
          Cmd_Len     => 2,
          Command     => Command_Type'(Kind => Shutdown),
          Has_Param   => False,
          Description => To_Cmd_Descr ("System Shutdown")),
         (Cmd_Str     => "st",
          Cmd_Len     => 2,
          Command     => Command_Type'(Kind => Print_Status),
          Has_Param   => False,
          Description => To_Cmd_Descr ("Print debug server status")),
         (Cmd_Str     => "lc",
          Cmd_Len     => 2,
          Command     => Command_Type'(Kind => List_Channels),
          Has_Param   => False,
          Description => To_Cmd_Descr ("List channels")),
         (Cmd_Str     => "ls",
          Cmd_Len     => 2,
          Command     => Command_Type'(Kind => List_Subjects),
          Has_Param   => False,
          Description => To_Cmd_Descr ("List subjects")),
         (Cmd_Str     => "sr",
          Cmd_Len     => 2,
          Command     => Command_Type'(Kind => Stream_Reset),
          Has_Param   => False,
          Description => To_Cmd_Descr ("Reset log streams")),
         (Cmd_Str     => "la",
          Cmd_Len     => 2,
          Command     => Command_Type'(Kind => Log_All),
          Has_Param   => False,
          Description => To_Cmd_Descr ("Enable all log channels")),
         (Cmd_Str     => "ln",
          Cmd_Len     => 2,
          Command     => Command_Type'(Kind => Log_None),
          Has_Param   => False,
          Description => To_Cmd_Descr ("Disable all log channels")),
         (Cmd_Str     => "lt",
          Cmd_Len     => 2,
          Command     => Command_Type'(Kind           => Log_Toggle,
                                       Channel_Number => 1),
          Has_Param   => True,
          Description => To_Cmd_Descr ("Toggle logging of channel")),
         (Cmd_Str     => "te",
          Cmd_Len     => 2,
          Command     => Command_Type'(Kind         => Trigger_Event,
                                       Event_Number => 0),
          Has_Param   => True,
          Description => To_Cmd_Descr ("Trigger event")),
         Null_Command);

   -------------------------------------------------------------------------

   procedure Enable_Log_Buffer
     (Queue   : in out Byte_Queue.Queue_Type;
      Name    :        String;
      Success :    out Boolean)
   is
      use type Musinfo.Memregion_Type;

      Region : Musinfo.Memregion_Type;
   begin
      Success := False;
      if Name'Length > Musinfo.Name_Index_Type'Last
        or Name'First > Positive'Last - Name'Length
      then
         return;
      end if;

      if not Musinfo.Instance.Is_Valid then
         Byte_Queue.Format.Append_Line
           (Queue => Queue,
            Item  => Invalid_Musinfo_Message);
         return;
      end if;

      Region := Musinfo.Instance.Memory_By_Name
        (Name => Musinfo.Utils.To_Name (Str => Name));
      if Region /= Musinfo.Null_Memregion and then Region.Flags.Channel
        and then Is_Log_Channel (Address => Region.Address)
      then
         for Channel in Channels.Debug_Interfaces_Type loop
            Buffers.Set_Log_Buffer_State
              (Buffer  => Channels.Instance (Channel).Buffer,
               ID      => Get_Channel_ID (Address => Region.Address),
               Enabled => True);
         end loop;

         Success := True;
      end if;
   end Enable_Log_Buffer;

   -------------------------------------------------------------------------

   procedure Setup_Log_Buffers (Console : in out Console_Type)
   is
      Start_Idx : Natural := Cspecs_Cfg.Enabled_Channels_Override'First;
      Comma_Idx : Natural;
      Result    : Boolean;
   begin
      for Channel in Channels.Debug_Interfaces_Type loop
         Buffers.Set_All_Log_Buffer_State
           (Buffer  => Channels.Instance (Channel).Buffer,
            Enabled => Cspecs_Cfg.Default_Channel_Enabled_State);
      end loop;

      loop
         exit when Start_Idx > Cspecs_Cfg.Enabled_Channels_Override'Last;

         Comma_Idx := String_Utils.Index
           (Source => Cspecs_Cfg.Enabled_Channels_Override,
            From   => Start_Idx,
            Char   => ',');

         Enable_Log_Buffer
           (Queue   => Console.Output_Queue,
            Name    => Cspecs_Cfg.Enabled_Channels_Override
              (Start_Idx .. Comma_Idx - 1),
            Success => Result);

         Byte_Queue.Format.Append_New_Line (Queue => Console.Output_Queue);
         if Result then
            Byte_Queue.Format.Append_String
              (Queue => Console.Output_Queue,
               Item  => "Enabled channel '");
            Byte_Queue.Format.Append_String
              (Queue => Console.Output_Queue,
               Item => Cspecs_Cfg.Enabled_Channels_Override
                 (Start_Idx .. Comma_Idx - 1));
            Byte_Queue.Format.Append_String
              (Queue => Console.Output_Queue,
               Item  => "'");
         else
            Byte_Queue.Format.Append_String
              (Queue => Console.Output_Queue,
               Item  => "Error enabling channel '");
            Byte_Queue.Format.Append_String
              (Queue => Console.Output_Queue,
               Item => Cspecs_Cfg.Enabled_Channels_Override
                 (Start_Idx .. Comma_Idx - 1));
            Byte_Queue.Format.Append_Line
              (Queue => Console.Output_Queue,
               Item  => "'");
         end if;

         Start_Idx := Comma_Idx + 1;
      end loop;
   end Setup_Log_Buffers;

   -------------------------------------------------------------------------

   procedure Clean_Command_Buffer (Console : in out Console_Type)
   is
   begin
      Console.Command_Buffer     := Empty_Command_Buffer;
      Console.Command_Buffer_Pos := Console.Command_Buffer'First;
   end Clean_Command_Buffer;

   -------------------------------------------------------------------------

   procedure Initialize (Console : out Console_Type)
   is
   begin
      Console.Command_Buffer     := Empty_Command_Buffer;
      Console.Command_Buffer_Pos := Console.Command_Buffer'First;
      Byte_Queue.Initialize (Queue => Console.Output_Queue);
      Setup_Log_Buffers (Console => Console);
   end Initialize;

   -------------------------------------------------------------------------

   procedure Print_Help (Queue : in out Byte_Queue.Queue_Type)
   is
      Header : constant String := "Available Commands:" & ASCII.CR & ASCII.LF;
      Indent : constant String := "  ";
   begin
      Byte_Queue.Format.Append_String (Queue => Queue,
                                       Item  => Header);

      for C of Cmd_Descs loop
         if C.Description /= Null_Descr then
            Byte_Queue.Format.Append_String
              (Queue => Queue,
               Item  => Indent);
            if String_Utils.Is_Control (Char => C.Cmd_Str (C.Cmd_Str'First))
            then
               case C.Cmd_Str (C.Cmd_Str'First) is
                  when ASCII.ESC =>
                     Byte_Queue.Format.Append_String
                       (Queue => Queue,
                        Item  => "<ESC> ");
                  when others =>
                     Byte_Queue.Format.Append_String
                       (Queue => Queue,
                        Item  => "<???> ");
               end case;
            else
               Byte_Queue.Format.Append_String
                 (Queue => Queue,
                  Item  => C.Cmd_Str);
               Byte_Queue.Format.Append_String
                 (Queue => Queue,
                  Item  => "    ");
            end if;

            if C.Has_Param then
               Byte_Queue.Format.Append_String
                 (Queue => Queue,
                  Item  => "<num>");
            else
               Byte_Queue.Format.Append_String
                 (Queue => Queue,
                  Item  => "     ");
            end if;

            Byte_Queue.Format.Append_String
              (Queue => Queue,
               Item  => "         ");
            Byte_Queue.Format.Append_Line
              (Queue => Queue,
               Item  => C.Description);
         end if;
      end loop;
   end Print_Help;

   -------------------------------------------------------------------------

   function Is_Log_Channel (Address : Interfaces.Unsigned_64) return Boolean
   is
      use type Interfaces.Unsigned_64;
   begin
      return Address >= Cspecs.Log_Channels_Address_Base and
        Address < Cspecs.Log_Channels_Address_Base +
          Cspecs.Log_Channels_Element_Count *
            Cspecs.Log_Channels_Element_Size;
   end Is_Log_Channel;

   -------------------------------------------------------------------------

   function Get_Channel_ID
     (Address : Interfaces.Unsigned_64)
      return Subject_Buffer_Range
   is
      use type Interfaces.Unsigned_64;
   begin
      return Subject_Buffer_Range
        ((Address - Cspecs.Log_Channels_Address_Base) /
         (Cspecs.Log_Channels_Element_Size) + 1);
   end Get_Channel_ID;

   -------------------------------------------------------------------------

   procedure List_Channels
     (Queue   : in out Byte_Queue.Queue_Type;
      Success :    out Boolean)
   is
      Header : constant String := "|        ID | Channel";
      H_Rule : constant String := "|-----------+--------------------";

      Iter : Musinfo.Utils.Resource_Iterator_Type;
   begin
      if not Musinfo.Instance.Is_Valid then
         Byte_Queue.Format.Append_Line
           (Queue => Queue,
            Item  => Invalid_Musinfo_Message);
         Success := False;
      else
         Byte_Queue.Format.Append_Line
           (Queue => Queue,
            Item  => Header);
         Byte_Queue.Format.Append_Line
           (Queue => Queue,
            Item  => H_Rule);

         Iter := Musinfo.Instance.Create_Resource_Iterator;
         while Musinfo.Instance.Has_Element (Iter => Iter) loop
            pragma Loop_Invariant (Musinfo.Instance.Belongs_To (Iter => Iter));
            declare
               use type Musinfo.Resource_Kind;
               use type Musinfo.Name_Size_Type;

               Resource : constant Musinfo.Resource_Type :=
                 Musinfo.Instance.Element (Iter => Iter);
            begin
               if Resource.Kind = Musinfo.Res_Memory
                 and then Is_Log_Channel (Address => Resource.Mem_Data.Address)
               then
                  Byte_Queue.Format.Append_Character
                    (Queue => Queue,
                     Item  => '|');
                  Byte_Queue.Format.Append_Natural
                    (Queue      => Queue,
                     Item       => Natural (Get_Channel_ID
                       (Address => Resource.Mem_Data.Address)),
                     Left_Align => False);
                  Byte_Queue.Format.Append_String
                    (Queue => Queue,
                     Item  => " | ");
                  if Resource.Name.Length > 0 then
                     Byte_Queue.Format.Append_Line
                       (Queue => Queue,
                        Item  => String (Resource.Name.Data
                          (Resource.Name.Data'First .. Musinfo.Name_Index_Type
                               (Resource.Name.Length))));
                  else
                     Byte_Queue.Format.Append_New_Line (Queue => Queue);
                  end if;
               end if;
            end;
            Musinfo.Instance.Next (Iter => Iter);
         end loop;

         Byte_Queue.Format.Append_Line
           (Queue => Queue,
            Item  => H_Rule);
         Success := True;
      end if;
   end List_Channels;

   -------------------------------------------------------------------------

   procedure List_Subjects (Queue : in out Byte_Queue.Queue_Type)
   is
      Header    : constant String := "|        ID | Subject";
      Subj_Rule : constant Subject_List.Name_Type := (others => '-');
      H_Rule    : constant String := "|-----------+--" & Subj_Rule;
   begin
      Byte_Queue.Format.Append_Line
        (Queue => Queue,
         Item  => Header);
      Byte_Queue.Format.Append_Line
        (Queue => Queue,
         Item  => H_Rule);

      for I in Subject_List.Subject_Names'Range loop
         Byte_Queue.Format.Append_Character
           (Queue => Queue,
            Item  => '|');
         Byte_Queue.Format.Append_Natural
           (Queue      => Queue,
            Item       => Natural (I),
            Left_Align => False);
         Byte_Queue.Format.Append_String
           (Queue => Queue,
            Item  => " | ");
         Byte_Queue.Format.Append_Line
           (Queue => Queue,
            Item  => Subject_List.Subject_Names (I));
      end loop;

      Byte_Queue.Format.Append_Line
        (Queue => Queue,
         Item  => H_Rule);
   end List_Subjects;

   -------------------------------------------------------------------------

   procedure Stream_Reset
   is
   begin
      for Channel in Channels.Debug_Interfaces_Type loop
         Buffers.Reset_Readers (Buffer => Channels.Instance (Channel).Buffer);
      end loop;
   end Stream_Reset;

   -------------------------------------------------------------------------

   procedure Log_All
   is
   begin
      for Channel in Channels.Debug_Interfaces_Type loop
         Buffers.Set_All_Log_Buffer_State
           (Buffer  => Channels.Instance (Channel).Buffer,
            Enabled => True);
      end loop;
   end Log_All;

   -------------------------------------------------------------------------

   procedure Log_None
   is
   begin
      for Channel in Channels.Debug_Interfaces_Type loop
         Buffers.Set_All_Log_Buffer_State
           (Buffer  => Channels.Instance (Channel).Buffer,
            Enabled => False);
      end loop;
   end Log_None;

   -------------------------------------------------------------------------

   procedure Log_Toggle
     (Number  :     Natural;
      Success : out Boolean)
   is
   begin
      if Number >= Natural (Subject_Buffer_Range'First)
        and then Number <= Natural (Subject_Buffer_Range'Last)
      then
         for Channel in Channels.Debug_Interfaces_Type loop
            Buffers.Toggle_Log_Buffer_State
              (Buffer => Channels.Instance (Channel).Buffer,
               ID     => Subject_Buffer_Range (Number));
         end loop;
         Success := True;
      else
         Success := False;
      end if;
   end Log_Toggle;

   -------------------------------------------------------------------------

   procedure Trigger_Event
     (Number  :     Natural;
      Success : out Boolean)
   is
   begin
      if Number <= Natural (Interfaces.Unsigned_8'Last) then
         SK.Hypercall.Trigger_Event (Number => Interfaces.Unsigned_8 (Number));
         Success := True;
      else
         Success := False;
      end if;
   end Trigger_Event;

   -------------------------------------------------------------------------

   procedure Write_Command_Buffer (Console : in out Console_Type)
   is
      Cmd_End_Idx : constant Natural := Console.Command_Buffer_Pos - 1;
   begin
      Byte_Queue.Format.Append_String (Queue => Console.Output_Queue,
                                       Item  => Command_Prompt);
      if Cmd_End_Idx in Console.Command_Buffer'Range then
         Byte_Queue.Format.Append_String
           (Queue => Console.Output_Queue,
            Item  => Console.Command_Buffer
              (Console.Command_Buffer'First .. Cmd_End_Idx));
      end if;
   end Write_Command_Buffer;

   -------------------------------------------------------------------------

   procedure Print_Status (Queue : in out Byte_Queue.Queue_Type)
   is
   begin
      Channels.Print_State (Queue => Queue);
   end Print_Status;

   -------------------------------------------------------------------------

   procedure Execute_Command
     (Command :        Command_Type;
      Queue   : in out Byte_Queue.Queue_Type;
      Success :    out Boolean)
   is
   begin
      Success := True;

      case Command.Kind is
         when Failure =>
            Success := False;
         when Show_Prompt =>
            null;
         when Clear_Line =>
            null;
         when Shutdown =>
            SK.Hypercall.Trigger_Event (Number => Cspecs_Ev.Shutdown_ID);
         when Reboot =>
            SK.Hypercall.Trigger_Event (Number => Cspecs_Ev.Reboot_ID);
         when Log_All =>
            Log_All;
         when Log_None =>
            Log_None;
         when Log_Toggle =>
            Log_Toggle (Number  => Command.Channel_Number,
                        Success => Success);
         when Trigger_Event =>
            Trigger_Event (Number  => Command.Event_Number,
                           Success => Success);
         when Print_Help =>
            Print_Help (Queue => Queue);
         when List_Channels =>
            List_Channels (Queue   => Queue,
                           Success => Success);
         when List_Subjects =>
            List_Subjects (Queue => Queue);
         when Stream_Reset =>
            Stream_Reset;
         when Print_Status =>
            Print_Status (Queue => Queue);
      end case;
   end Execute_Command;

   -------------------------------------------------------------------------

   function Parse_Command
     (Buffer     : Command_Buffer_Type;
      Buffer_End : Natural)
      return Command_Type
   is
      Cmd_End : constant Natural := Buffer_End - 1;
      Cur_Cmd : Natural := Cmd_Descs'Last;
   begin
      Lookup_Command :
      for I in Cmd_Descs'Range loop
         if ((Cmd_End = Cmd_Descs (I).Cmd_Len and not Cmd_Descs (I).Has_Param)
             or (Cmd_End > Cmd_Descs (I).Cmd_Len and Cmd_Descs (I).Has_Param))
           and then Cmd_Descs (I).Cmd_Str
           (Cmd_Descs (I).Cmd_Str'First .. Cmd_Descs (I).Cmd_Len)
             = Buffer (Buffer'First .. Cmd_Descs (I).Cmd_Len)
         then
            Cur_Cmd := I;
            exit Lookup_Command;
         end if;
      end loop Lookup_Command;

      declare
         Command : Command_Type := Cmd_Descs (Cur_Cmd).Command;
      begin
         if Cmd_Descs (Cur_Cmd).Has_Param then
            declare
               Number  : Natural;
               Cur_Idx : Natural := Buffer'First + Cmd_Descs (Cur_Cmd).Cmd_Len;
            begin
               loop

                  pragma Loop_Invariant
                    (Cur_Idx >= Buffer'First + Cmd_Descs (Cur_Cmd).Cmd_Len);
                  --  Skip spaces.

                  exit when Cur_Idx > Cmd_End or else Buffer (Cur_Idx) /= ' ';
                  Cur_Idx := Cur_Idx + 1;
               end loop;

               String_Utils.Consume_Integer
                 (Text       => Buffer,
                  Text_Start => Cur_Idx,
                  Text_End   => Cmd_End,
                  Number     => Number);

               if Cur_Idx = Buffer_End then

                  --  Entire buffer was consumed.

                  if Command.Kind = Trigger_Event then
                     Command.Event_Number := Number;
                  elsif Command.Kind = Log_Toggle then
                     Command.Channel_Number := Number;
                  end if;
               else
                  return Command_Type'(Kind => Failure);
               end if;
            end;
         end if;
         return Command;
      end;
   end Parse_Command;

   -------------------------------------------------------------------------

   procedure Run
     (Console     : in out Console_Type;
      Input_Queue : in out Byte_Queue.Queue_Type)
   is

      --  Overwrites the previous character in the command buffer with a space
      --  and decrements the buffer position.
      procedure Clear_Character
      is
      begin
         Console.Command_Buffer_Pos := Console.Command_Buffer_Pos - 1;
         Console.Command_Buffer (Console.Command_Buffer_Pos) := ' ';
         Byte_Queue.Format.Append_Character
           (Queue => Console.Output_Queue,
            Item  => ASCII.BS);
         Byte_Queue.Format.Append_Character
           (Queue => Console.Output_Queue,
            Item  => ' ');
         Byte_Queue.Format.Append_Character
           (Queue => Console.Output_Queue,
            Item  => ASCII.BS);
      end Clear_Character;

      ----------------------------------------------------------------------

      --  Removes the last character in the console command buffer, if it
      --  exists, and prints the resulting command buffer.
      procedure Handle_Backspace
      is
      begin
         if Console.Command_Buffer_Pos > Console.Command_Buffer'First then
            Clear_Character;
         end if;
      end Handle_Backspace;

      ----------------------------------------------------------------------

      --  Clears the entire command buffer.
      procedure Handle_Escape
      is
      begin
         while Console.Command_Buffer_Pos > Console.Command_Buffer'First loop
            Clear_Character;
         end loop;
      end Handle_Escape;

      ----------------------------------------------------------------------

      --  Adds the character to the command buffers and writes the character
      --  to Direct_Output_Queue.
      procedure Handle_Normal_Input (Char : Character)
      is
      begin
         if Console.Command_Buffer_Pos <= Console.Command_Buffer'Last then
            Console.Command_Buffer (Console.Command_Buffer_Pos) := Char;
            Console.Command_Buffer_Pos := Console.Command_Buffer_Pos + 1;
            Byte_Queue.Format.Append_Character
              (Queue => Console.Output_Queue,
               Item  => Char);
         end if;
      end Handle_Normal_Input;

      ----------------------------------------------------------------------

      --  Tries to parse the command buffer and executes the command.
      procedure Handle_Return
      is
         Command         : constant Command_Type := Parse_Command
           (Buffer     => Console.Command_Buffer,
            Buffer_End => Console.Command_Buffer_Pos);
         Success         : Boolean;
         Failure_Message : constant String := "Command failed, try 'h'";
      begin
         Byte_Queue.Format.Append_New_Line (Queue => Console.Output_Queue);
         Execute_Command (Command => Command,
                          Queue   => Console.Output_Queue,
                          Success => Success);

         if not Success then
            Byte_Queue.Format.Append_Line
              (Queue => Console.Output_Queue,
               Item  => Failure_Message);
         end if;

         Clean_Command_Buffer (Console => Console);
         Write_Command_Buffer (Console => Console);
      end Handle_Return;

      Input_Element : Interfaces.Unsigned_8;
   begin
      while Byte_Queue.Bytes_Used (Queue => Input_Queue) > 0 loop
         Byte_Queue.Peek (Queue => Input_Queue,
                          Byte  => Input_Element);
         Byte_Queue.Drop_Bytes (Queue  => Input_Queue,
                                Length => 1);

         declare
            Input_Char : constant Character := Character'Val (Input_Element);
         begin
            case Input_Char is
               when ASCII.ESC => Handle_Escape;
               when ASCII.BS
                  | ASCII.DEL => Handle_Backspace;
               when ASCII.CR  => Handle_Return;
               when others    => Handle_Normal_Input (Char => Input_Char);
            end case;
         end;
      end loop;
   end Run;

end Dbg.Consoles;
