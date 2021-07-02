--
--  Copyright (C) 2014  secunet Security Networks AG
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

with System;

with Debuglog.Types;
with Debuglog.Stream.Writer_Instance;

with Libmudebuglog_Component.Channels;

package body Debuglog.Sink
with
   Refined_State => (State => (Message_Channel, Message_Buffer, Message_Index))
is

   package Cspecs renames Libmudebuglog_Component.Channels;

   --  Workaround for [T611-019]: Size of channel cannot be set in generic.
   --  See also https://github.com/AdaCore/ada-spark-rfcs/issues/75
   subtype CT is Stream.Channel_Type
     with Object_Size => Cspecs.Debuglog_Size * 8;

   pragma Warnings
     (GNATprove, Off,
      "indirect writes to * through a potential alias are ignored",
      Reason => "All objects with address clause are mapped to external "
      & "interfaces. Non-overlap is checked during system build.");
   pragma Warnings
     (GNATprove, Off,
      "writing * is assumed to have no effects on other non-volatile objects",
      Reason => "All objects with address clause are mapped to external "
      & "interfaces. Non-overlap is checked during system build.");
   --D @Interface
   --D Shared memory channel to the debug server (dbgserver) subject.
   --D Implemented using the writer provided by the Libmuchannel library.
   --D Components may use Libmudebuglog Debuglog.Client.Put* operations to
   --D transfer logging information to the dbgserver.
   Message_Channel : CT
   with
      Address => System'To_Address (Cspecs.Debuglog_Address),
      Size    => Cspecs.Debuglog_Size * 8,
      Async_Readers;
   pragma Warnings
     (GNATprove, On,
      "writing * is assumed to have no effects on other non-volatile objects");
   pragma Warnings
     (GNATprove, On,
      "indirect writes to * through a potential alias are ignored");

   Message_Buffer : Types.Data_Type;
   Message_Index  : Types.Message_Index;

   -------------------------------------------------------------------------

   procedure Flush
   with
      Refined_Global  => (Input    => Musinfo.Instance.Scheduling_Info,
                          In_Out   => (Message_Buffer, Message_Channel),
                          Output   => Message_Index,
                          Proof_In => Musinfo.Instance.State),
      Refined_Depends =>
        (Message_Channel  =>+ (Message_Buffer,
                               Musinfo.Instance.Scheduling_Info),
         (Message_Buffer,
          Message_Index)  => null)
   is
   begin
      Message_Buffer.Timestamp := Musinfo.Instance.TSC_Schedule_Start;

      Stream.Writer_Instance.Write (Channel => Message_Channel,
                                    Element => Message_Buffer);

      Message_Index  := Types.Message_Index'First;
      Message_Buffer := Types.Null_Data;
   end Flush;

   -------------------------------------------------------------------------

   procedure Init (Epoch : Interfaces.Unsigned_64)
   with
      Refined_Global  => (Output => (Message_Buffer, Message_Channel,
                                     Message_Index)),
      Refined_Depends => (Message_Channel  => (Epoch),
                          (Message_Buffer,
                           Message_Index)  => null)
   is
   begin
      Message_Index  := Types.Message_Index'First;
      Message_Buffer := Types.Null_Data;
      Stream.Writer_Instance.Initialize
        (Channel => Message_Channel,
         Epoch   => Stream.Header_Field_Type (Epoch));
   end Init;

   -------------------------------------------------------------------------

   procedure Write_Character (Item : Character)
   with
      Refined_Global  => (Input    => Musinfo.Instance.Scheduling_Info,
                          In_Out   => (Message_Buffer, Message_Channel,
                                       Message_Index),
                          Proof_In => Musinfo.Instance.State),
      Refined_Depends =>
        (Message_Channel  =>+ (Item, Message_Buffer, Message_Index,
                               Musinfo.Instance.Scheduling_Info),
         (Message_Buffer,
          Message_Index)  =>+ (Message_Index, Item))
   is
   begin
      if Item /= ASCII.NUL and then Item /= ASCII.CR then
         Message_Buffer.Message (Message_Index) := Item;

         if Message_Index = Types.Message_Index'Last or else Item = ASCII.LF
         then
            Flush;
         else
            Message_Index := Message_Index + 1;
         end if;
      end if;
   end Write_Character;

end Debuglog.Sink;
