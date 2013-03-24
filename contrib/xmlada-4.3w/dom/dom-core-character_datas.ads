------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with DOM.Core.Nodes;

package DOM.Core.Character_Datas is

   function Data (N : Character_Data) return DOM_String
      renames DOM.Core.Nodes.Node_Value;
   --  Return the Data associated with the node.

   procedure Set_Data (N : Character_Data; Data : DOM_String)
      renames DOM.Core.Nodes.Set_Node_Value;
   --  Change the data of the node.

   function Length (N : Character_Data) return Natural;
   --  Return the length of the data stored in N

   function Substring_Data
     (N : Character_Data;
      Offset : Natural;
      Count : Natural) return DOM_String;
   --  Offset starts at 0, Count is the number of character (not necessarily
   --  bytes, depending on the encoding) in the returned string.
   --  Index_Size_Err raised if Offset is invalid

   procedure Append_Data (N : Character_Data; Arg : DOM_String);
   --  Append a string to N.

   procedure Insert_Data
     (N : Character_Data;
      Offset : Natural;
      Arg : DOM_String);
   --  Insert Arg at a specific index in N. Offset starts at 0.
   --  Index_Size_Err raised if Offset is invalid

   procedure Delete_Data
     (N : Character_Data; Offset : Natural; Count : Natural);
   --  Delete a specific range of Data.
   --  Index_Size_Err raised if Offset is invalid.

   procedure Replace_Data
     (N : Character_Data;
      Offset : Natural;
      Count : Natural;
      Arg : DOM_String);
   --  Replace a range in N by Arg.
   --  Index_Size_Err raised if Offset is invalid.

end DOM.Core.Character_Datas;
