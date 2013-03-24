with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line;  use Ada.Command_Line;
with Ada.Text_IO;       use Ada.Text_IO;
with DOM.Core.Nodes;    use DOM.Core.Nodes;
with GNAT.OS_Lib;       use GNAT.OS_Lib;

package body Driver is

   function Special_Chars (Str : String) return String;
   --  Return Str, where special characters have been replaced for suitable
   --  comparison

   -------------------
   -- Special_Chars --
   -------------------

   function Special_Chars (Str : String) return String is
      Result : Unbounded_String;
   begin
      for S in Str'Range loop
         if Str (S) = ASCII.LF then
            Append (Result, "\n");
         else
            Append (Result, Str (S));
         end if;
      end loop;
      return To_String (Result); 
   end Special_Chars;

   -------------------
   -- Assert_Equals --
   -------------------

   procedure Assert_Equals
     (Expected    : DOM_String;
      Actual      : DOM_String;
      Ignore_Case : Boolean;
      File        : String;
      Id          : String)
   is
      Result : Boolean;
   begin
      if Ignore_Case then
         Result := Special_Chars (To_Lower (Expected)) =
            Special_Chars (To_Lower (Actual));
      else
         Result := Special_Chars (Expected) = Special_Chars (Actual);
      end if;

      if not Result then
         Put_Line ("Test failed: " & Id & " (in " & File & ")");
         Put_Line ("   Expected=" & Expected);
         Put_Line ("   Actual  =" & Actual);
         OS_Exit (1);
      end if;
   end Assert_Equals;

   -------------------
   -- Assert_Equals --
   -------------------

   procedure Assert_Equals
     (Expected    : DOM_String;
      Actual      : Unbounded_String;
      Ignore_Case : Boolean;
      File        : String;
      Id          : String)
   is
   begin
      Assert_Equals (Expected, To_String (Actual), Ignore_Case, File, Id);
   end Assert_Equals;

   -----------------
   -- Assert_Null --
   -----------------

   procedure Assert_Null
     (Actual      : Node;
      File        : String;
      Id          : String)
   is
   begin
     if Actual /= null then
        Put_Line ("Test failed " & Id & " (in " & File & ")");
        Put_Line ("   Expected null node");
        OS_Exit (1);
     end if;
   end Assert_Null;

   ---------------------
   -- Assert_Not_Null --
   ---------------------

   procedure Assert_Not_Null
     (Actual      : Node;
      File        : String;
      Id          : String)
   is
   begin
     if Actual = null then
        Put_Line ("Test failed " & Id & " (in " & File & ")");
        Put_Line ("   Expected not null node");
        OS_Exit (1);
     end if;
   end Assert_Not_Null;

   ---------------------
   -- Assert_Not_Null --
   ---------------------

   procedure Assert_Not_Null
     (Actual      : Named_Node_Map;
      File        : String;
      Id          : String)
   is
   begin
     if Length (Actual) = 0 then
        Put_Line ("Test failed " & Id & " (in " & File & ")");
        Put_Line ("   Expected non-empty node map");
        OS_Exit (1);
     end if;
   end Assert_Not_Null;

   -----------------
   -- Assert_True --
   -----------------

   procedure Assert_True
     (Actual      : Boolean;
      File        : String;
      Id          : String)
   is
   begin
     if not Actual then
        Put_Line ("Test failed " & Id & " (in " & File & ")");
        Put_Line ("   Expected True, got " & Boolean'Image (Actual));
        OS_Exit (1);
     end if;
   end Assert_True;

   ------------------
   -- Assert_False --
   ------------------

   procedure Assert_False
     (Actual      : Boolean;
      File        : String;
      Id          : String)
   is
   begin
     if Actual then
        Put_Line ("Test failed " & Id & " (in " & File & ")");
        Put_Line ("   Expected False, got " & Boolean'Image (Actual));
        OS_Exit (1);
     end if;
   end Assert_False;
end Driver;
