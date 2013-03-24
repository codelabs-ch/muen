--
-- Copyright (c) 2007, 2008 Tero Koskinen <tero.koskinen@iki.fi>
--
-- Permission to use, copy, modify, and distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--

with Ahven.SList;
with Ahven.AStrings;
with Ahven.Long_AStrings;

pragma Elaborate_All (Ahven.SList);

-- Like the name implies, the Results package is used for
-- storing the test results.
--
-- Result_Info holds one invidual result and
-- Result_Collection holds multiple Result_Infos.
--
package Ahven.Results is
   use Ahven.AStrings;

   type Result_Info is private;

   Empty_Result_Info : constant Result_Info;
   -- Result_Info object which holds no result. It can be used
   -- to initialize a new Result_Info object.

   procedure Set_Test_Name (Info : in out Result_Info;
                            Name :        Bounded_String);
   -- Set a test name for the result.

   procedure Set_Routine_Name (Info : in out Result_Info;
                               Name :        Bounded_String);
   -- Set a routine name for the result.

   procedure Set_Message (Info : in out Result_Info;
                          Message : Bounded_String);
   -- Set a message for the result.

   procedure Set_Test_Name (Info : in out Result_Info; Name : String);
   -- A helper function, which calls Set_Test_Name (.. ; Bounded_String)

   procedure Set_Routine_Name (Info : in out Result_Info; Name : String);
   -- A helper function, which calls Set_Routine_Name (.. ; Bounded_String)

   procedure Set_Message (Info : in out Result_Info; Message : String);
   -- A helper function, which calls Set_Message (.. ; Bounded_String)

   procedure Set_Long_Message (Info    : in out Result_Info;
                               Message :        Bounded_String);
   -- Set a long message for the result

   procedure Set_Long_Message
     (Info    : in out Result_Info;
      Message :        Long_AStrings.Bounded_String);
   -- Set a long message for the result

   procedure Set_Long_Message (Info : in out Result_Info; Message : String);
   -- A helper function, which calls Set_Long_Message (.. ; Bounded_String)

   procedure Set_Execution_Time (Info         : in out Result_Info;
                                 Elapsed_Time :        Duration);
   -- Set the execution time of the result info (test).

   procedure Set_Output_File (Info     : in out Result_Info;
                              Filename :        Bounded_String);
   -- Set the name of the test output file.

   procedure Set_Output_File (Info     : in out Result_Info;
                              Filename :        String);
   -- Set the name of the test output file.

   function Get_Test_Name (Info : Result_Info) return String;
   -- Return the test name of the result info.

   function Get_Routine_Name (Info : Result_Info) return String;
   -- Return the routine name of the result info.

   function Get_Message (Info : Result_Info) return String;
   -- Return the message of the result info.

   function Get_Long_Message (Info : Result_Info) return String;
   -- Return the long message of the result info.

   function Get_Execution_Time (Info : Result_Info) return Duration;
   -- Return the execution time of the result info.

   function Get_Output_File (Info : Result_Info) return Bounded_String;
   -- Return the name of the output file.
   -- Empty string is returned in case there is no output file.

   type Result_Collection is limited private;
   -- A collection of Result_Info objects.
   -- Contains also child collections.

   type Result_Collection_Access is access Result_Collection;

   procedure Add_Child (Collection : in out Result_Collection;
                        Child      :        Result_Collection_Access);
   -- Add a child collection to the collection.

   procedure Add_Error (Collection : in out Result_Collection;
                        Info       :        Result_Info);
   -- Add a test error to the collection.

   procedure Add_Skipped (Collection : in out Result_Collection;
                          Info       :        Result_Info);
   -- Add a skipped test to the collection.

   procedure Add_Failure (Collection : in out Result_Collection;
                          Info       :        Result_Info);
   -- Add a test failure to the collection.

   procedure Add_Pass (Collection : in out Result_Collection;
                       Info       :        Result_Info);
   -- Add a passed test to the collection

   procedure Release (Collection : in out Result_Collection);
   -- Release resourced held by the collection.
   -- Frees also all children added via Add_Child.

   procedure Set_Name (Collection : in out Result_Collection;
                       Name       :        Bounded_String);
   -- Set a test name for the collection.

   procedure Set_Parent (Collection : in out Result_Collection;
                         Parent     :        Result_Collection_Access);
   -- Set a parent collection to the collection.

   function Test_Count (Collection : Result_Collection) return Natural;
   -- Return the amount of tests in the collection.
   -- Tests in child collections are included.

   function Direct_Test_Count (Collection : Result_Collection) return Natural;
   -- Return the amount of tests in the collection.
   -- The tests in the child collections are NOT included.

   function Pass_Count (Collection : Result_Collection) return Natural;
   -- Return the amount of passed tests in the collection.
   -- Tests in child collections are included.

   function Error_Count (Collection : Result_Collection) return Natural;
   -- Return the amount of test errors in the collection.
   -- Tests in child collections are included.

   function Failure_Count (Collection : Result_Collection) return Natural;
   -- Return the amount of test errors in the collection.
   -- Tests in child collections are included.

   function Skipped_Count (Collection : Result_Collection) return Natural;
   -- Return the amount of skipped tests in the colleciton.
   -- Tests in child collections are included.

   function Get_Test_Name (Collection : Result_Collection)
     return Bounded_String;
   -- Return the name of the collection's test.

   function Get_Parent (Collection : Result_Collection)
     return Result_Collection_Access;
   -- Return the parent of the collection.

   function Get_Execution_Time (Collection : Result_Collection)
     return Duration;
   -- Return the execution time of the whole collection.

   type Result_Info_Cursor is private;
   -- A cursor type for Pass, Failure and Error results.

   function First_Pass (Collection : Result_Collection)
     return Result_Info_Cursor;
   -- Get the first pass from the collection.

   function First_Failure (Collection : Result_Collection)
     return Result_Info_Cursor;
   -- Get the first failure from the collection.

   function First_Skipped (Collection : Result_Collection)
     return Result_Info_Cursor;
   -- Get the first skipped test from the collection.

   function First_Error (Collection : Result_Collection)
     return Result_Info_Cursor;
   -- Get the first error from the collection.

   function Next (Position : Result_Info_Cursor) return Result_Info_Cursor;
   -- Get the next pass/failure/error.

   function Data (Position : Result_Info_Cursor) return Result_Info;
   -- Get the data behind the cursor.

   function Is_Valid (Position : Result_Info_Cursor) return Boolean;
   -- Is the cursor still valid?

   type Result_Collection_Cursor is private;
   -- Cursor for iterating over a set of Result_Collection access objects.

   function First_Child (Collection : in Result_Collection)
     return Result_Collection_Cursor;
   -- Get the first child of the collection.

   function Next (Position : Result_Collection_Cursor)
     return Result_Collection_Cursor;
   -- Get the next child.

   function Is_Valid (Position : Result_Collection_Cursor) return Boolean;
   -- Is the cursor still valid?

   function Data (Position : Result_Collection_Cursor)
     return Result_Collection_Access;
   -- Get the data (Result_Collection_Access) behind the cursor.

   function Child_Depth (Collection : Result_Collection) return Natural;
   -- Return the maximum depth of children. (a child of a child, etc.)
   --
   -- Examples: Child_Depth is 0 for a collection without children.
   -- Collection with a child containing another child has a depth of 2.

private
   type Result_Info is record
      Test_Name      : Bounded_String  := Null_Bounded_String;
      Output_File    : Bounded_String  := Null_Bounded_String;
      Routine_Name   : Bounded_String  := Null_Bounded_String;
      Execution_Time : Duration := 0.0;
      Message        : Bounded_String  := Null_Bounded_String;
      Long_Message   : Long_AStrings.Bounded_String
        := Long_AStrings.Null_Bounded_String;
   end record;

   Empty_Result_Info : constant Result_Info :=
     (Test_Name      => Null_Bounded_String,
      Routine_Name   => Null_Bounded_String,
      Message        => Null_Bounded_String,
      Long_Message   => Long_AStrings.Null_Bounded_String,
      Execution_Time => 0.0,
      Output_File    => Null_Bounded_String);

   package Result_Info_List is
     new Ahven.SList (Element_Type => Result_Info);

   type Result_Collection_Wrapper is record
      Ptr : Result_Collection_Access;
   end record;
   -- Work around for Janus/Ada 3.1.1d/3.1.2beta generic bug.

   package Result_List is
     new Ahven.SList (Element_Type => Result_Collection_Wrapper);

   type Result_Info_Cursor is new Result_Info_List.Cursor;

   type Result_Collection_Cursor is new Result_List.Cursor;

   type Result_Collection is limited record
      Test_Name : Bounded_String := Null_Bounded_String;
      Passes    : Result_Info_List.List;
      Failures  : Result_Info_List.List;
      Errors    : Result_Info_List.List;
      Skips     : Result_Info_List.List;
      Children  : Result_List.List;
      Parent    : Result_Collection_Access := null;
   end record;
end Ahven.Results;
