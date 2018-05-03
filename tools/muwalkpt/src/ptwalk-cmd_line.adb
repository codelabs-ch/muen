--
--  Copyright (C) 2018  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2018  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with GNAT.Strings;

with Mutools.Cmd_Line;

with Ptwalk.Utils;

package body Ptwalk.Cmd_Line
is

   use Ada.Strings.Unbounded;

   function S
     (Source : Ada.Strings.Unbounded.Unbounded_String)
      return String
      renames Ada.Strings.Unbounded.To_String;

   function U
     (Source : String)
      return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   -------------------------------------------------------------------------

   function Get_PT_File return String
   is (S (PT_File));

   -------------------------------------------------------------------------

   function Get_PT_Type return Paging.Paging_Mode_Type
   is (PT_Type);

   -------------------------------------------------------------------------

   function Get_PT_Pointer return Interfaces.Unsigned_64
   is (PT_Pointer);

   -------------------------------------------------------------------------

   function Get_Virtual_Address return Interfaces.Unsigned_64
   is (Virtual_Addr);

   -------------------------------------------------------------------------

   procedure Init (Description : String)
   is
      Cmdline : Mutools.Cmd_Line.Config_Type;
      PT_Ptr  : aliased GNAT.Strings.String_Access;
      PT_Typ  : aliased GNAT.Strings.String_Access;
   begin
      GNAT.Command_Line.Set_Usage
        (Config => Cmdline.Data,
         Usage  => "[options] <virtual address> <pagetable file>",
         Help   => Description);
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Output      => PT_Ptr'Access,
         Switch      => "-p:",
         Long_Switch => "--pt-pointer:",
         Help        => "Page table pointer");
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Output      => PT_Typ'Access,
         Switch      => "-t:",
         Long_Switch => "--pt-type:",
         Help        => "Page table type [IA32e|EPT]");
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Switch      => "-h",
         Long_Switch => "--help",
         Help        => "Display usage and exit");

      begin
         GNAT.Command_Line.Getopt
           (Config => Cmdline.Data,
            Parser => Parser);
         begin
            PT_Pointer := Utils.To_Number (Str => (PT_Ptr.all));

         exception
            when others => raise GNAT.Command_Line.Invalid_Parameter;
         end;

         begin
            PT_Type := Paging.Paging_Mode_Type'Value (PT_Typ.all & "_mode");

         exception
            when others => raise GNAT.Command_Line.Invalid_Parameter;
         end;

         GNAT.Strings.Free (X => PT_Ptr);
         GNAT.Strings.Free (X => PT_Typ);

         begin
            Virtual_Addr := Utils.To_Number
              (Str => GNAT.Command_Line.Get_Argument (Parser => Parser));
            exception
               when others => raise GNAT.Command_Line.Invalid_Parameter;
         end;

         PT_File := U (GNAT.Command_Line.Get_Argument (Parser => Parser));
         if PT_File = Null_Unbounded_String then
            raise GNAT.Command_Line.Invalid_Parameter;
         end if;

      exception
         when GNAT.Command_Line.Invalid_Switch |
              GNAT.Command_Line.Exit_From_Command_Line =>
            raise Invalid_Cmd_Line;
         when GNAT.Command_Line.Invalid_Parameter =>
            GNAT.Command_Line.Display_Help (Config => Cmdline.Data);
            raise Invalid_Cmd_Line;
      end;
   end Init;

end Ptwalk.Cmd_Line;
