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

with Sax.Encodings;      use Sax.Encodings;
with Unicode.CES;        use Unicode.CES;
with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Nodes;     use DOM.Core.Nodes;

package body DOM.Core.Texts is

   ----------------
   -- Split_Text --
   ----------------

   function Split_Text (Te : Text; Offset : Natural) return Text is
      Index : constant Integer := Index_From_Offset
        (Te.Text.all, Offset, Encoding);
      N : Node;
   begin
      if Index < 0 then
         raise Index_Size_Err;
      end if;

      N := Insert_Before
        (Parent_Node (Te),
         Create_Text_Node
           (Owner_Document (Te), Te.Text.all (Index .. Te.Text'Last)),
         Next_Sibling (Te));
      Set_Node_Value (Te, Te.Text (Te.Text'First .. Index - 1));
      return N;
   end Split_Text;

end DOM.Core.Texts;
