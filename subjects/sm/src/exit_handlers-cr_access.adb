--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK;

with Subject.Text_IO;

with Subject_Info;

package body Exit_Handlers.CR_Access
is

   use Subject_Info;

   -------------------------------------------------------------------------

   procedure Process (Halt : out Boolean)
   is
      use type SK.Word64;
   begin
      Halt := False;

      if (State.Exit_Qualification and 16#30#) = 0 then
         if (State.Exit_Qualification and 15) = 0 then
            if (State.Exit_Qualification and 16#f00#) = 0 then
               State.SHADOW_CR0 := State.Regs.RAX;
               State.CR0 := State.SHADOW_CR0 or 16#20#; -- CR0_FIXED0
               Subject.Text_IO.Put_String
                 (Item => "Accepting mov eax, cr0 at ");
               Subject.Text_IO.Put_Word64 (State.RIP);
               Subject.Text_IO.Put_String (Item => ". set to ");
               Subject.Text_IO.Put_Word64 (State.SHADOW_CR0);
               Subject.Text_IO.Put_String (Item => " and ");
               Subject.Text_IO.Put_Word64 (State.CR0);
               Subject.Text_IO.New_Line;
            else
               Subject.Text_IO.Put_String
                 (Item => "Unhandled MOV to CRx. unknown register #");
               Subject.Text_IO.Put_Word64
                 (State.Exit_Qualification and 16#f00#);
               Subject.Text_IO.New_Line;
               Halt := True;
            end if;
         else
            Subject.Text_IO.Put_String (Item => "Unhandled MOV to CRx");
            Subject.Text_IO.New_Line;
            Halt := True;
         end if;
      else
         Subject.Text_IO.Put_String (Item => "Unhandled CR access method");
         Subject.Text_IO.New_Line;
         Halt := True;
      end if;
   end Process;

end Exit_Handlers.CR_Access;
