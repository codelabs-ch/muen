--
--  Copyright (C) 2014  secunet Security Networks AG
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

with Interfaces;

with SK.Strings;

with Dbg.Byte_Queue.Format;

package body Dbg.Buffers
is

   use type Debuglog.Stream.Reader.Result_Type;

   --  Log channels for subjects defined in the active system policy.
   type Log_Context_Type is array (Subject_Buffer_Range)
     of Debuglog.Stream.Channel_Type
       with
         Component_Size => 8 * Cspecs.Log_Channels_Element_Size;

   Log_Context : Log_Context_Type
     with
       Size    => 8 * Cspecs.Log_Channels_Element_Count
         * Cspecs.Log_Channels_Element_Size,
       Address => System'To_Address (Cspecs.Log_Channels_Address_Base);

   Timestamp_Invalid : constant Interfaces.Unsigned_64
     := Interfaces.Unsigned_64'Last;

   --  Required bytes in output buffer to store a new message
   --  (CR/LF + line prefix + actual msg data).
   Required_Msg_Bytes : constant Positive
     := 2 + 9 + Debuglog.Types.Message_Index'Last;

   --  Check if a message is present in the given subject buffer.
   function Is_Message_Present
     (Subject_Buffer : Subject_Buffer_Type)
      return Boolean;

   --  Mark given subject buffer as logged.
   procedure Mark_As_Logged (Subject_Buffer : in out Subject_Buffer_Type);

   --  Check all subject buffers for new messages.
   procedure Update_Message_Buffers (Buffer : in out Buffer_Type);

   --  Find subject with oldest message.
   procedure Find_Oldest_Message
     (Buffer         :     Buffer_Type;
      Oldest_Subject : out Subject_Buffer_Range;
      Found          : out Boolean);

   --  Add log line prefix for given subject to output queue.
   procedure Add_Line_Prefix
     (Subject      :        Subject_Buffer_Range;
      Overrun      :        Boolean;
      New_Epoch    :        Boolean;
      Continuation :        Boolean;
      Output_Queue : in out Byte_Queue.Queue_Type);

   --  Add message in subject buffer to output queue.
   procedure Add_Message
     (Subject_Buffer : in out Subject_Buffer_Type;
      Output_Queue   : in out Byte_Queue.Queue_Type);

   --  Move message of given subject to output queue.
   procedure Log_Message
     (Buffer       : in out Buffer_Type;
      Subject      :        Subject_Buffer_Range;
      Output_Queue : in out Byte_Queue.Queue_Type);

   --  Find oldest message and log it.
   procedure Log_Oldest_Message
     (Buffer       : in out Buffer_Type;
      Output_Queue : in out Byte_Queue.Queue_Type);

   --  Append idle mark to output queue.
   procedure Idle_Mark
     (Buffer       : in out Buffer_Type;
      Output_Queue : in out Byte_Queue.Queue_Type);

   -------------------------------------------------------------------------

   procedure Add_Line_Prefix
     (Subject      :        Subject_Buffer_Range;
      Overrun      :        Boolean;
      New_Epoch    :        Boolean;
      Continuation :        Boolean;
      Output_Queue : in out Byte_Queue.Queue_Type)
   is
   begin
      Byte_Queue.Format.Append_New_Line (Queue => Output_Queue);

      Byte_Queue.Format.Append_String
        (Queue => Output_Queue,
         Item  => SK.Strings.Img (Interfaces.Unsigned_16 (Subject)));

      if New_Epoch then
         Byte_Queue.Format.Append_Character
           (Queue => Output_Queue,
            Item  => '%');
      elsif Overrun then
         Byte_Queue.Format.Append_Character
           (Queue => Output_Queue,
            Item  => '#');
      elsif Continuation then
         Byte_Queue.Format.Append_Character
           (Queue => Output_Queue,
            Item  => '>');
      else
         Byte_Queue.Format.Append_Character
           (Queue => Output_Queue,
            Item  => '|');
      end if;
   end Add_Line_Prefix;

   -------------------------------------------------------------------------

   procedure Add_Message
     (Subject_Buffer : in out Subject_Buffer_Type;
      Output_Queue   : in out Byte_Queue.Queue_Type)
   is
      Char : Character;
   begin
      for Index in Debuglog.Types.Message_Index loop
         Char := Subject_Buffer.Cache.Message (Index);

         exit when Char = ASCII.LF or Char = ASCII.NUL;

         Byte_Queue.Format.Append_Character
           (Queue => Output_Queue,
            Item  => Char);
      end loop;

      Subject_Buffer.Message_Incomplete := Char /= ASCII.LF;
   end Add_Message;

   -------------------------------------------------------------------------

   procedure Find_Oldest_Message
     (Buffer         :     Buffer_Type;
      Oldest_Subject : out Subject_Buffer_Range;
      Found          : out Boolean)
   is
      use type Interfaces.Unsigned_64;

      Candidate_Timestamp : Interfaces.Unsigned_64 := Timestamp_Invalid;
   begin
      Oldest_Subject := Subject_Buffer_Range'First;
      Found          := False;

      for Subject in Subject_Buffer_Range loop
         if Buffer.Subjects (Subject).Cache.Timestamp < Candidate_Timestamp
         then
            Candidate_Timestamp := Buffer.Subjects (Subject).Cache.Timestamp;
            Oldest_Subject      := Subject;
            Found               := True;
         end if;
      end loop;
   end Find_Oldest_Message;

   -------------------------------------------------------------------------

   procedure Idle_Mark
     (Buffer       : in out Buffer_Type;
      Output_Queue : in out Byte_Queue.Queue_Type)
   is
   begin
      if not Buffer.Is_Idle and then
        Byte_Queue.Bytes_Free (Queue => Output_Queue) >= 5
      then
         Byte_Queue.Format.Append_New_Line (Queue => Output_Queue);
         Byte_Queue.Format.Append_String (Queue => Output_Queue,
                                          Item  => "---");
         Buffer.Is_Idle := True;
      end if;
   end Idle_Mark;

   -------------------------------------------------------------------------

   procedure Initialize (Buffer : out Buffer_Type)
   is
      --  Initialize given subject buffer.
      procedure Initialize_Subject (Subject_Buffer : out Subject_Buffer_Type);

      ----------------------------------------------------------------------

      procedure Initialize_Subject (Subject_Buffer : out Subject_Buffer_Type)
      is
      begin
         Subject_Buffer.State := Debuglog.Stream.Reader.Null_Reader;
         Subject_Buffer.Cache := Debuglog.Types.Data_Type'
           (Timestamp => Timestamp_Invalid,
            Message   => Debuglog.Types.Null_Message);

         Subject_Buffer.Message_Incomplete := False;
         Subject_Buffer.Overrun_Occurred   := False;
         Subject_Buffer.New_Epoch_Occurred := False;
         Subject_Buffer.Enabled            := True;
      end Initialize_Subject;
   begin
      for Subject in Subject_Buffer_Range loop
         Initialize_Subject (Subject_Buffer => Buffer.Subjects (Subject));
      end loop;

      Buffer.Is_Idle      := False;
      Buffer.Last_Subject := Subject_Buffer_Range'First;
   end Initialize;

   -------------------------------------------------------------------------

   function Is_Message_Present
     (Subject_Buffer : Subject_Buffer_Type)
      return Boolean
   is
      use type Interfaces.Unsigned_64;
   begin
      return Subject_Buffer.Cache.Timestamp /= Timestamp_Invalid;
   end Is_Message_Present;

   -------------------------------------------------------------------------

   procedure Log_Message
     (Buffer       : in out Buffer_Type;
      Subject      :        Subject_Buffer_Range;
      Output_Queue : in out Byte_Queue.Queue_Type)
   is
   begin
      if Byte_Queue.Bytes_Free (Queue => Output_Queue) >= Required_Msg_Bytes
      then
         if not Buffer.Subjects (Subject).Message_Incomplete
           or Buffer.Last_Subject /= Subject
           or Buffer.Subjects (Subject).Overrun_Occurred
           or Buffer.Subjects (Subject).New_Epoch_Occurred
           or Buffer.Is_Idle
         then
            Add_Line_Prefix
              (Subject      => Subject,
               Overrun      => Buffer.Subjects (Subject).Overrun_Occurred,
               New_Epoch    => Buffer.Subjects (Subject).New_Epoch_Occurred,
               Continuation => Buffer.Subjects (Subject).Message_Incomplete,
               Output_Queue => Output_Queue);
         end if;

         Add_Message (Subject_Buffer => Buffer.Subjects (Subject),
                      Output_Queue   => Output_Queue);
         Mark_As_Logged (Subject_Buffer => Buffer.Subjects (Subject));
         Buffer.Last_Subject := Subject;

         Buffer.Subjects (Subject).Overrun_Occurred   := False;
         Buffer.Subjects (Subject).New_Epoch_Occurred := False;
         Buffer.Is_Idle := False;
      end if;
   end Log_Message;

   -------------------------------------------------------------------------

   procedure Log_Oldest_Message
     (Buffer       : in out Buffer_Type;
      Output_Queue : in out Byte_Queue.Queue_Type)
   is
      Subject         : Subject_Buffer_Range;
      Message_Present : Boolean;
   begin
      Find_Oldest_Message (Buffer         => Buffer,
                           Oldest_Subject => Subject,
                           Found          => Message_Present);

      if Message_Present then
         Log_Message (Buffer       => Buffer,
                      Subject      => Subject,
                      Output_Queue => Output_Queue);
      else
         Idle_Mark (Buffer       => Buffer,
                    Output_Queue => Output_Queue);
      end if;
   end Log_Oldest_Message;

   -------------------------------------------------------------------------

   procedure Mark_As_Logged (Subject_Buffer : in out Subject_Buffer_Type)
   is
   begin
      Subject_Buffer.Cache.Timestamp := Timestamp_Invalid;
   end Mark_As_Logged;

   -------------------------------------------------------------------------

   procedure Run
     (Buffer       : in out Buffer_Type;
      Output_Queue : in out Byte_Queue.Queue_Type)
   is
   begin
      Update_Message_Buffers (Buffer => Buffer);
      Log_Oldest_Message
        (Buffer       => Buffer,
         Output_Queue => Output_Queue);
   end Run;

   -------------------------------------------------------------------------

   procedure Update_Message_Buffers (Buffer : in out Buffer_Type)
   is
      --  Check given subject buffer for new message.
      procedure Update_Message_Buffer
        (Subject        :        Subject_Buffer_Range;
         Subject_Buffer : in out Subject_Buffer_Type);

      ----------------------------------------------------------------------

      procedure Update_Message_Buffer
        (Subject        :        Subject_Buffer_Range;
         Subject_Buffer : in out Subject_Buffer_Type)
      is
         Read_Result : Debuglog.Stream.Reader.Result_Type;
      begin
         if not Is_Message_Present (Subject_Buffer => Subject_Buffer) then
            Debuglog.Stream.Reader.Read
              (Channel => Log_Context (Subject),
               Reader  => Subject_Buffer.State,
               Element => Subject_Buffer.Cache,
               Result  => Read_Result);

            if Read_Result = Debuglog.Stream.Reader.Epoch_Changed then
               Subject_Buffer.New_Epoch_Occurred := True;
            elsif Read_Result = Debuglog.Stream.Reader.Overrun_Detected then
               Subject_Buffer.Overrun_Occurred := True;
            end if;

            if Read_Result /= Debuglog.Stream.Reader.Success then

               --  Mark cache as empty

               Subject_Buffer.Cache := Debuglog.Types.Data_Type'
                 (Timestamp => Timestamp_Invalid,
                  Message   => Debuglog.Types.Null_Message);
            end if;
         end if;
      end Update_Message_Buffer;
   begin
      for Subject in Subject_Buffer_Range loop
         if Buffer.Subjects (Subject).Enabled then
            Update_Message_Buffer
              (Subject        => Subject,
               Subject_Buffer => Buffer.Subjects (Subject));
         end if;
      end loop;
   end Update_Message_Buffers;

end Dbg.Buffers;
