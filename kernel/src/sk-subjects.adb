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

with System;

package body SK.Subjects
--# own
--#    State is Descriptors;
is

   pragma Warnings (Off, "*padded by * bits");
   type Subject_State_Array is array
     (Skp.Subject_Id_Type) of SK.Subject_State_Type;
   for Subject_State_Array'Component_Size use Page_Size * 8;
   for Subject_State_Array'Alignment use Page_Size;
   pragma Warnings (On, "*padded by * bits");

   --  Descriptors used to manage subject states.
   --# accept Warning, 396, Descriptors, "Not an external variable";
   Descriptors : Subject_State_Array;
   for Descriptors'Address use System'To_Address (16#001e0000#);
   --# end accept;

   -------------------------------------------------------------------------

   function Get_RFLAGS (Id : Skp.Subject_Id_Type) return SK.Word64
   --# global
   --#    Descriptors;
   --# return
   --#    Descriptors (Id).RFLAGS;
   is
   begin
      return Descriptors (Id).RFLAGS;
   end Get_RFLAGS;

   -------------------------------------------------------------------------

   function Get_RIP (Id : Skp.Subject_Id_Type) return SK.Word64
   --# global
   --#    Descriptors;
   --# return
   --#    Descriptors (Id).RIP;
   is
   begin
      return Descriptors (Id).RIP;
   end Get_RIP;

   -------------------------------------------------------------------------

   function Get_State (Id : Skp.Subject_Id_Type) return SK.Subject_State_Type
   --# global
   --#    Descriptors;
   --# return
   --#    Descriptors (Id);
   is
   begin
      return Descriptors (Id);
   end Get_State;

   -------------------------------------------------------------------------

   procedure Set_CR0
     (Id    : Skp.Subject_Id_Type;
      Value : SK.Word64)
   --# global
   --#    Descriptors;
   --# derives
   --#    Descriptors from *, Id, Value;
   --# post
   --#    Descriptors (Id).CR0 = Value;
   is
   begin
      Descriptors (Id).CR0 := Value;
   end Set_CR0;

   -------------------------------------------------------------------------

   procedure Set_RIP
     (Id    : Skp.Subject_Id_Type;
      Value : SK.Word64)
   --# global
   --#    Descriptors;
   --# derives
   --#    Descriptors from *, Id, Value;
   --# post
   --#    Descriptors (Id).RIP = Value;
   is
   begin
      Descriptors (Id).RIP := Value;
   end Set_RIP;

   -------------------------------------------------------------------------

   procedure Set_RSP
     (Id    : Skp.Subject_Id_Type;
      Value : SK.Word64)
   --# global
   --#    Descriptors;
   --# derives
   --#    Descriptors from *, Id, Value;
   --# post
   --#    Descriptors (Id).RSP = Value;
   is
   begin
      Descriptors (Id).RSP := Value;
   end Set_RSP;

   -------------------------------------------------------------------------

   procedure Set_State
     (Id            : Skp.Subject_Id_Type;
      Subject_State : SK.Subject_State_Type)
   --# global
   --#    Descriptors;
   --# derives
   --#    Descriptors from *, Id, Subject_State;
   --# post
   --#    Descriptors (Id) = Subject_State;
   is
   begin
      Descriptors (Id) := Subject_State;
   end Set_State;

begin

   --# hide SK.Subjects;

   for D in Skp.Subject_Id_Type range Subject_State_Array'Range loop
      Descriptors (D) := SK.Null_Subject_State;
   end loop;
end SK.Subjects;
