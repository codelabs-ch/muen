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

with DOM.Core;

with Interfaces;

package Validators
is

   --  Register all known validators.
   procedure Register_All;

   Validation_Error : exception;

private

   use type Interfaces.Unsigned_64;

   type Test_Function is not null access function
     (A, B : Interfaces.Unsigned_64) return Boolean;

   function Equals
     (Left, Right : Interfaces.Unsigned_64)
      return Boolean
   is (Left = Right);

   function Not_Equals
     (Left, Right : Interfaces.Unsigned_64)
      return Boolean
   is (Left /= Right);

   function Less_Than
     (Left, Right : Interfaces.Unsigned_64)
      return Boolean
   is (Left < Right);

   function Less_Or_Equal
     (Left, Right : Interfaces.Unsigned_64)
      return Boolean
   is (Left <= Right);

   function Mod_Equal_Zero
     (Left, Right : Interfaces.Unsigned_64)
      return Boolean
   is (Left mod Right = 0);

   --  Check attribute value 'Attr' of given 'Node_Type' nodes using the
   --  specified test function and function parameter 'Right'. 'Name_Attr'
   --  defines the attribute used to query the name of a specific node. If the
   --  test fails, an exception with the given 'Error_Msg' string is raised.
   procedure Check_Attribute
     (Nodes     : DOM.Core.Node_List;
      Node_Type : String;
      Attr      : String;
      Name_Attr : String;
      Test      : Test_Function;
      Right     : Interfaces.Unsigned_64;
      Error_Msg : String);

end Validators;
