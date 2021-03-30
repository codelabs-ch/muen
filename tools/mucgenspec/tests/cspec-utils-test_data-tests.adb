--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Cspec.Utils.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

--  begin read only
--  end read only
package body Cspec.Utils.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Get_Component_Name (Gnattest_T : in out Test);
   procedure Test_Get_Component_Name_5e95a6 (Gnattest_T : in out Test) renames Test_Get_Component_Name;
--  id:2.2/5e95a6a1deec4047/Get_Component_Name/1/0/
   procedure Test_Get_Component_Name (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Spec : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Spec,
                   Kind => Muxml.None,
                   File => "data/component_vt.xml");
      Assert (Condition => Get_Component_Name (Spec => Spec) = "vt",
              Message   => "Name mismatch");
--  begin read only
   end Test_Get_Component_Name;
--  end read only


--  begin read only
   procedure Test_1_To_Memory_Str (Gnattest_T : in out Test);
   procedure Test_To_Memory_Str_ea699a (Gnattest_T : in out Test) renames Test_1_To_Memory_Str;
--  id:2.2/ea699a34dcb2416e/To_Memory_Str/1/0/
   procedure Test_1_To_Memory_Str (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
      Impl : DOM.Core.DOM_Implementation;
      Node : DOM.Core.Node;

      Ref : constant String :=
        "   Input_Address    : constant := 16#f000#;"       & ASCII.LF
        & "   Input_Size       : constant := 16#2000#;"     & ASCII.LF
        & "   Input_Executable : constant Boolean := True;" & ASCII.LF
        & "   Input_Writable   : constant Boolean := False;";
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := Mutools.XML_Utils.Create_Component_Memory_Node
        (Policy       => Data,
         Logical_Name => "input",
         Address      => "16#f000#",
         Size         => "16#2000#",
         Executable   => True,
         Writable     => False);
      Assert (Condition => To_Memory_Str (Memory => Node) = Ref,
              Message   => "String mismatch");
--  begin read only
   end Test_1_To_Memory_Str;
--  end read only


--  begin read only
   procedure Test_To_Channel_Str (Gnattest_T : in out Test);
   procedure Test_To_Channel_Str_046b8f (Gnattest_T : in out Test) renames Test_To_Channel_Str;
--  id:2.2/046b8f08d89e579c/To_Channel_Str/1/0/
   procedure Test_To_Channel_Str (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl : DOM.Core.DOM_Implementation;
      Data : Muxml.XML_Data_Type;
      Node : DOM.Core.Node;

      Ref1 : constant String :=
        "   Input_Address : constant := 16#f000#;" & ASCII.LF
        & "   Input_Size    : constant := 16#2000#;" & ASCII.LF
        & "   Input_Kind    : constant Channel_Kind := Channel_Reader;";
      Ref2 : constant String := Ref1 & ASCII.LF
        & "   Input_Vector  : constant := 54;";
      Ref3 : constant String := Ref2 & ASCII.LF
        & "   Input_Event   : constant := 234;";
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "reader");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "logical",
         Value => "input");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "virtualAddress",
         Value => "16#f000#");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "size",
         Value => "16#2000#");

      Assert (Condition => To_Channel_Str (Channel => Node) = Ref1,
              Message   => "String mismatch (1)");

      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "vector",
         Value => "54");
      Assert (Condition => To_Channel_Str (Channel => Node) = Ref2,
              Message   => "String mismatch (2)");

      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "event",
         Value => "234");
      Assert (Condition => To_Channel_Str (Channel => Node) = Ref3,
              Message   => "String mismatch (3)");
--  begin read only
   end Test_To_Channel_Str;
--  end read only


--  begin read only
   procedure Test_To_Device_Str (Gnattest_T : in out Test);
   procedure Test_To_Device_Str_cc9a94 (Gnattest_T : in out Test) renames Test_To_Device_Str;
--  id:2.2/cc9a94ad35460f94/To_Device_Str/1/0/
   procedure Test_To_Device_Str (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Fname : constant String := "to_device_str";
      Data  : Muxml.XML_Data_Type;
      Tmpl  : Mutools.Templates.Template_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.None,
                   File => "data/component_vt.xml");

      Tmpl := Mutools.Templates.Create
        (Content => To_Device_Str
           (Device => Muxml.Utils.Get_Element
                (Doc   => Data.Doc,
                 XPath => "/component/requires/devices/device"
                 & "[@logical='storage_device']")));

      Mutools.Templates.Write
        (Template => Tmpl,
         Filename => "obj/" & Fname);
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/" & Fname,
               Filename2 => "data/" & Fname),
              Message   => "String mismatch");
      Ada.Directories.Delete_File (Name => "obj/" & Fname);
--  begin read only
   end Test_To_Device_Str;
--  end read only


--  begin read only
   procedure Test_To_Event_Str (Gnattest_T : in out Test);
   procedure Test_To_Event_Str_4b1772 (Gnattest_T : in out Test) renames Test_To_Event_Str;
--  id:2.2/4b177268d9c4b6d9/To_Event_Str/1/0/
   procedure Test_To_Event_Str (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Source_No_Action
      is
         Data    : Muxml.XML_Data_Type;
         Ev_Node : DOM.Core.Node;
         Ref_Str : constant String
           := "   Handover_ID     : constant := 1;" & ASCII.LF
           & "   Handover_Action : constant Source_Event_Action_Kind "
           & ":= No_Action;";
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/component_vt.xml");
         Ev_Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/component/requires/events/source/"
            & "event[@logical='handover']");

         Assert (Condition => To_Event_Str (Event => Ev_Node) = Ref_Str,
                 Message   => "Event string mismatch (Src no action)");
      end Source_No_Action;

      ----------------------------------------------------------------------

      procedure Source_Poweroff
      is
         Data    : Muxml.XML_Data_Type;
         Ev_Node : DOM.Core.Node;
         Ref_Str : constant String
           := "   Shutdown_ID     : constant := 31;" & ASCII.LF
           & "   Shutdown_Action : constant Source_Event_Action_Kind "
           & ":= System_Poweroff;";
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/component_vt.xml");
         Ev_Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/component/requires/events/source/"
            & "event[@logical='shutdown']");

         Assert (Condition => To_Event_Str (Event => Ev_Node) = Ref_Str,
                 Message   => "Event string mismatch (Src poweroff)");
      end Source_Poweroff;

      ----------------------------------------------------------------------

      procedure Source_Reboot
      is
         Data    : Muxml.XML_Data_Type;
         Ev_Node : DOM.Core.Node;
         Ref_Str : constant String
           := "   Reboot_ID     : constant := 30;" & ASCII.LF
           & "   Reboot_Action : constant Source_Event_Action_Kind "
           & ":= System_Reboot;";
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/component_vt.xml");
         Ev_Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/component/requires/events/source/"
            & "event[@logical='reboot']");

         Assert (Condition => To_Event_Str (Event => Ev_Node) = Ref_Str,
                 Message   => "Event string mismatch (Src reboot) ");
      end Source_Reboot;

      ----------------------------------------------------------------------

      procedure Source_Unmask_Irq
      is
         Data    : Muxml.XML_Data_Type;
         Ev_Node : DOM.Core.Node;
         Ref_Str : constant String
           := "   Unmask_Irq_22_ID     : constant := 2;" & ASCII.LF
           & "   Unmask_Irq_22_Number : constant := 22;" & ASCII.LF
           & "   Unmask_Irq_22_Action : constant Source_Event_Action_Kind "
           & ":= Unmask_Irq;";
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/component_vt.xml");
         Ev_Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/component/requires/events/source/"
            & "event[@logical='unmask_irq_22']");

         Assert (Condition => To_Event_Str (Event => Ev_Node) = Ref_Str,
                 Message   => "Event string mismatch (Src unmask irq)");
      end Source_Unmask_Irq;

      ----------------------------------------------------------------------

      procedure Target_Inject_Interrupt
      is
         Data    : Muxml.XML_Data_Type;
         Ev_Node : DOM.Core.Node;
         Ref_Str : constant String
           := "   Timer_Vector : constant := 37;" & ASCII.LF
           & "   Timer_Action : constant Target_Event_Action_Kind "
           & ":= Inject_Interrupt;";
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/component_vt.xml");
         Ev_Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/component/requires/events/target/"
            & "event[@logical='timer']");

         Assert (Condition => To_Event_Str (Event => Ev_Node) = Ref_Str,
                 Message   => "Event string mismatch (Tgt inject interrupt)");
      end Target_Inject_Interrupt;

      ----------------------------------------------------------------------

      procedure Target_No_Action
      is
         Data    : Muxml.XML_Data_Type;
         Ev_Node : DOM.Core.Node;
         Ref_Str : constant String
           := "   Foo_Action : constant Target_Event_Action_Kind "
           & ":= No_Action;";
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/component_vt.xml");
         Ev_Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/component/requires/events/target/"
            & "event[@logical='foo']");

         Assert (Condition => To_Event_Str (Event => Ev_Node) = Ref_Str,
                 Message   => "Event string mismatch (Tgt no action)"
                 & ASCII.LF & "'" & Ref_Str & "' /= " & ASCII.LF & "'"
                 & To_Event_Str (Event => Ev_Node) & "'");
      end Target_No_Action;

       ----------------------------------------------------------------------

      procedure Target_Reset
      is
         Data    : Muxml.XML_Data_Type;
         Ev_Node : DOM.Core.Node;
         Ref_Str : constant String
           := "   Reset_Action : constant Target_Event_Action_Kind "
           & ":= Reset;";
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/component_vt.xml");
         Ev_Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/component/requires/events/target/"
            & "event[@logical='reset']");

         Assert (Condition => To_Event_Str (Event => Ev_Node) = Ref_Str,
                 Message   => "Event string mismatch (Tgt reset)");
      end Target_Reset;
   begin
      Source_No_Action;
      Source_Poweroff;
      Source_Reboot;
      Source_Unmask_Irq;
      Target_No_Action;
      Target_Inject_Interrupt;
      Target_Reset;
--  begin read only
   end Test_To_Event_Str;
--  end read only


--  begin read only
   procedure Test_To_Memory_Array_Str (Gnattest_T : in out Test);
   procedure Test_To_Memory_Array_Str_a91bde (Gnattest_T : in out Test) renames Test_To_Memory_Array_Str;
--  id:2.2/a91bdeda113a5f83/To_Memory_Array_Str/1/0/
   procedure Test_To_Memory_Array_Str (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl      : DOM.Core.DOM_Implementation;
      Data      : Muxml.XML_Data_Type;
      Arr, Node : DOM.Core.Node;

      Ref : constant String :=
        "   Input_Address_Base  : constant := 16#f000#;" & ASCII.LF
        & "   Input_Element_Size  : constant := 16#1000#;" & ASCII.LF
        & "   Input_Element_Count : constant := 2;" & ASCII.LF
        & "   Input_Executable    : constant Boolean := False;" & ASCII.LF
        & "   Input_Writable      : constant Boolean := True;" & ASCII.LF
        & ASCII.LF
        & "   Input_Names : constant Name_Array (1 .. Input_Element_Count)"
        & ASCII.LF
        & "     := (" & ASCII.LF
        & "         1 => To_Name (Str => ""tau0""),"  & ASCII.LF
        & "         2 => To_Name (Str => ""time"")"  & ASCII.LF
        & "        );";
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Arr := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "array");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Arr,
         Name  => "logical",
         Value => "input");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Arr,
         Name  => "virtualAddressBase",
         Value => "16#f000#");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Arr,
         Name  => "elementSize",
         Value => "16#1000#");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Arr,
         Name  => "executable",
         Value => "false");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Arr,
         Name  => "writable",
         Value => "true");

      begin
         declare
            Dummy : constant String := To_Memory_Array_Str (Arr => Arr);
         begin
            Assert (Condition => False,
                    Message   => "Exception expected");
         end;

      exception
         when E : Array_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Memory array 'Input' has no child elements",
                    Message   => "Exception message mismatch");
      end;

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "memory");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "logical",
         Value => "tau0");
      Node := DOM.Core.Nodes.Append_Child
        (N         => Arr,
         New_Child => Node);
      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "memory");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "logical",
         Value => "time");
      Node := DOM.Core.Nodes.Append_Child
        (N         => Arr,
         New_Child => Node);

      Assert (Condition => To_Memory_Array_Str (Arr => Arr) = Ref,
              Message   => "String mismatch");
--  begin read only
   end Test_To_Memory_Array_Str;
--  end read only


--  begin read only
   procedure Test_To_Channel_Array_Str (Gnattest_T : in out Test);
   procedure Test_To_Channel_Array_Str_62752c (Gnattest_T : in out Test) renames Test_To_Channel_Array_Str;
--  id:2.2/62752cef8dab5f66/To_Channel_Array_Str/1/0/
   procedure Test_To_Channel_Array_Str (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl      : DOM.Core.DOM_Implementation;
      Data      : Muxml.XML_Data_Type;
      Arr, Node : DOM.Core.Node;

      Names : constant String := ASCII.LF & ASCII.LF
        & "   Input_Names : constant Name_Array (1 .. Input_Element_Count)"
        & ASCII.LF
        & "     := (" & ASCII.LF
        & "         1 => To_Name (Str => ""tau0"")" & ASCII.LF
        & "        );";

      Ref1 : constant String :=
        "   Input_Address_Base  : constant := 16#f000#;" & ASCII.LF
        & "   Input_Element_Size  : constant := 16#1000#;" & ASCII.LF
        & "   Input_Element_Count : constant := 1;";
      Ref2 : constant String := Ref1 & ASCII.LF
        & "   Input_Vector_Base   : constant := 16;";
      Ref3 : constant String := Ref1 & ASCII.LF
        & "   Input_Event_Base    : constant := 32;";
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Arr := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "array");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Arr,
         Name  => "logical",
         Value => "input");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Arr,
         Name  => "virtualAddressBase",
         Value => "16#f000#");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Arr,
         Name  => "elementSize",
         Value => "16#1000#");
      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "reader");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "logical",
         Value => "tau0");

      begin
         declare
            Dummy : constant String := To_Channel_Array_Str (Arr => Arr);
         begin
            Assert (Condition => False,
                    Message   => "Exception expected");
         end;

      exception
         when E : Array_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Channel array 'Input' has no child elements",
                    Message   => "Exception message mismatch");
      end;

      Node := DOM.Core.Nodes.Append_Child
        (N         => Arr,
         New_Child => Node);

      Assert (Condition => To_Channel_Array_Str (Arr => Arr) = Ref1 & Names,
              Message   => "String mismatch (1)");

      DOM.Core.Elements.Set_Attribute
        (Elem  => Arr,
         Name  => "vectorBase",
         Value => "16");
      Assert (Condition => To_Channel_Array_Str (Arr => Arr) = Ref2 & Names,
              Message   => "String mismatch (2)");

      DOM.Core.Elements.Remove_Attribute
        (Elem => Arr,
         Name => "vectorBase");
      Muxml.Utils.Remove_Child
        (Node       => Arr,
         Child_Name => "reader");
      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "writer");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "logical",
         Value => "tau0");
      Node := DOM.Core.Nodes.Append_Child
        (N         => Arr,
         New_Child => Node);
      DOM.Core.Elements.Set_Attribute
        (Elem  => Arr,
         Name  => "eventBase",
         Value => "32");
      Assert (Condition => To_Channel_Array_Str (Arr => Arr) = Ref3 & Names,
              Message   => "String mismatch (3)");
--  begin read only
   end Test_To_Channel_Array_Str;
--  end read only


--  begin read only
   procedure Test_To_Config_Variable_Str (Gnattest_T : in out Test);
   procedure Test_To_Config_Variable_Str_c4bb13 (Gnattest_T : in out Test) renames Test_To_Config_Variable_Str;
--  id:2.2/c4bb13bd991a67bb/To_Config_Variable_Str/1/0/
   procedure Test_To_Config_Variable_Str (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl : DOM.Core.DOM_Implementation;
      Data : Muxml.XML_Data_Type;
      Node : DOM.Core.Node;

      Ref_Bool : constant String
        := "   Serial_Enabled : constant Boolean := True;";
      Ref_Int  : constant String
        := "   Debug_Clients : constant := 5;";
      Ref_Str  : constant String
        := "   Log_Prefix : constant String := ""Foo"";";
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "boolean");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "name",
         Value => "serial_enabled");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "value",
         Value => "true");
      Assert (Condition => To_Config_Variable_Str (Var => Node) = Ref_Bool,
              Message   => "Boolean mismatch");

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "integer");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "name",
         Value => "debug_clients");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "value",
         Value => "5");
      Assert (Condition => To_Config_Variable_Str (Var => Node) = Ref_Int,
              Message   => "Integer mismatch");

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "string");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "name",
         Value => "log_prefix");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "value",
         Value => "Foo");
      Assert (Condition => To_Config_Variable_Str (Var => Node) = Ref_Str,
              Message   => "String mismatch");
--  begin read only
   end Test_To_Config_Variable_Str;
--  end read only


--  begin read only
   procedure Test_Get_Name_Types_Str (Gnattest_T : in out Test);
   procedure Test_Get_Name_Types_Str_5134b9 (Gnattest_T : in out Test) renames Test_Get_Name_Types_Str;
--  id:2.2/5134b9ac0934fd4c/Get_Name_Types_Str/1/0/
   procedure Test_Get_Name_Types_Str (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => True,
              Message   => "Tested in Run procedure");
--  begin read only
   end Test_Get_Name_Types_Str;
--  end read only


--  begin read only
   procedure Test_2_To_Memory_Str (Gnattest_T : in out Test);
   procedure Test_To_Memory_Str_70858f (Gnattest_T : in out Test) renames Test_2_To_Memory_Str;
--  id:2.2/70858fbfdcc49d0f/To_Memory_Str/0/0/
   procedure Test_2_To_Memory_Str (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
      Impl : DOM.Core.DOM_Implementation;
      Node : DOM.Core.Node;

      Ref : constant String :=
        "   Eth0_Mmio_Address    : constant := 16#f000#;"       & ASCII.LF
        & "   Eth0_Mmio_Size       : constant := 16#2000#;"     & ASCII.LF
        & "   Eth0_Mmio_Executable : constant Boolean := True;" & ASCII.LF
        & "   Eth0_Mmio_Writable   : constant Boolean := False;";
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := Mutools.XML_Utils.Create_Component_Memory_Node
        (Policy       => Data,
         Logical_Name => "mmio",
         Address      => "16#f000#",
         Size         => "16#2000#",
         Executable   => True,
         Writable     => False);
      Assert (Condition => To_Memory_Str
              (Memory => Node, Logical_Prefix => "eth0_") = Ref,
              Message   => "String mismatch");
--  begin read only
   end Test_2_To_Memory_Str;
--  end read only


--  begin read only
   procedure Test_To_Ioport_Str (Gnattest_T : in out Test);
   procedure Test_To_Ioport_Str_e6262a (Gnattest_T : in out Test) renames Test_To_Ioport_Str;
--  id:2.2/e6262a0d9781039d/To_Ioport_Str/1/0/
   procedure Test_To_Ioport_Str (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl : DOM.Core.DOM_Implementation;
      Data : Muxml.XML_Data_Type;
      Node : DOM.Core.Node;

      Ref : constant String
        := "   Serial_Port_Start : constant := 16#03f8#;" & ASCII.LF
        & "   Serial_Port_End   : constant := 16#03ff#;";
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "ioPort");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "logical",
         Value => "port");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "start",
         Value => "16#03f8#");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "end",
         Value => "16#03ff#");

      Assert (Condition => To_Ioport_Str
              (Port => Node, Logical_Prefix => "Serial_") = Ref,
              Message   => "String mismatch");
--  begin read only
   end Test_To_Ioport_Str;
--  end read only


--  begin read only
   procedure Test_To_Irq_Str (Gnattest_T : in out Test);
   procedure Test_To_Irq_Str_f49a67 (Gnattest_T : in out Test) renames Test_To_Irq_Str;
--  id:2.2/f49a67925f46d03e/To_Irq_Str/1/0/
   procedure Test_To_Irq_Str (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl : DOM.Core.DOM_Implementation;
      Data : Muxml.XML_Data_Type;
      Node : DOM.Core.Node;

      Ref : constant String := "   Eth0_Ctrl_Irq : constant := 16#78#;";
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "irq");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "logical",
         Value => "ctrl_irq");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "vector",
         Value => "16#78#");

      Assert (Condition => To_Irq_Str
              (Irq => Node, Logical_Prefix => "Eth0_") = Ref,
              Message   => "String mismatch");
--  begin read only
   end Test_To_Irq_Str;
--  end read only


--  begin read only
   procedure Test_To_Name_Array (Gnattest_T : in out Test);
   procedure Test_To_Name_Array_3d1bd7 (Gnattest_T : in out Test) renames Test_To_Name_Array;
--  id:2.2/3d1bd72bf47433c8/To_Name_Array/1/0/
   procedure Test_To_Name_Array (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl      : DOM.Core.DOM_Implementation;
      Data      : Muxml.XML_Data_Type;
      Arr, Node : DOM.Core.Node;

        Ref : constant String
        := "   Dbg_Input_Names : constant Name_Array "
        & "(1 .. Dbg_Input_Element_Count)"
        & ASCII.LF & "     := ("
        & ASCII.LF & "         1 => To_Name (Str => ""tau0""),"
        & ASCII.LF & "         2 => To_Name (Str => ""sm""),"
        & ASCII.LF & "         3 => To_Name (Str => ""time"")"
        & ASCII.LF & "        );";
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Arr := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "array");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Arr,
         Name  => "logical",
         Value => "dbg_input");

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "reader");

      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "logical",
         Value => "tau0");
      Node := DOM.Core.Nodes.Append_Child
        (N         => Arr,
         New_Child => Node);
      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "reader");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "logical",
         Value => "sm");
      Node := DOM.Core.Nodes.Append_Child
        (N         => Arr,
         New_Child => Node);
      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "reader");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "logical",
         Value => "time");
      Node := DOM.Core.Nodes.Append_Child
        (N         => Arr,
         New_Child => Node);

      Assert (Condition => To_Name_Array (Arr => Arr) = Ref,
              Message   => "String mismatch");
--  begin read only
   end Test_To_Name_Array;
--  end read only


--  begin read only
   procedure Test_Memory_Attrs_As_String (Gnattest_T : in out Test);
   procedure Test_Memory_Attrs_As_String_9abdd9 (Gnattest_T : in out Test) renames Test_Memory_Attrs_As_String;
--  id:2.2/9abdd97303e68ce6/Memory_Attrs_As_String/1/0/
   procedure Test_Memory_Attrs_As_String (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl : DOM.Core.DOM_Implementation;
      Data : Muxml.XML_Data_Type;
      Node : DOM.Core.Node;

      Name, Addr, Size : Unbounded_String;

      Ref_Name : constant String := "mem1";
      Ref_Addr : constant String := "16#1000#";
      Ref_Size : constant String := "16#3000#";
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "memory");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "logical",
         Value => Ref_Name);

      begin
         Memory_Attrs_As_String
           (Node            => Node,
            Logical_Name    => Name,
            Virtual_Address => Addr,
            Size            => Size);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Attribute_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Memory node does not provide expected attributes",
                    Message   => "Exception mismatch");
      end;

      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "virtualAddress",
         Value => Ref_Addr);
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "size",
         Value => Ref_Size);

      Memory_Attrs_As_String
        (Node            => Node,
         Logical_Name    => Name,
         Virtual_Address => Addr,
         Size            => Size);
      Assert (Condition => To_String (Name) = Ref_Name,
              Message   => "Logical name mismatch");
      Assert (Condition => To_String (Addr) = Ref_Addr,
              Message   => "Virtual address mismatch");
      Assert (Condition => To_String (Size) = Ref_Size,
              Message   => "Size mismatch");
--  begin read only
   end Test_Memory_Attrs_As_String;
--  end read only


--  begin read only
   procedure Test_Memory_Perm_Attrs_As_String (Gnattest_T : in out Test);
   procedure Test_Memory_Perm_Attrs_As_String_7d468f (Gnattest_T : in out Test) renames Test_Memory_Perm_Attrs_As_String;
--  id:2.2/7d468f4cce634a46/Memory_Perm_Attrs_As_String/1/0/
   procedure Test_Memory_Perm_Attrs_As_String (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl : DOM.Core.DOM_Implementation;
      Data : Muxml.XML_Data_Type;
      Node : DOM.Core.Node;

      Executable, Writable : Unbounded_String;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "memory");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "executable",
         Value => "true");

      begin
         Memory_Perm_Attrs_As_String
           (Node       => Node,
            Executable => Executable,
            Writable   => Writable);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Attribute_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Memory node does not provide expected permission "
                    & "attributes",
                    Message   => "Exception mismatch");
      end;

      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "writable",
         Value => "false");

      Memory_Perm_Attrs_As_String
        (Node       => Node,
         Executable => Executable,
         Writable   => Writable);
      Assert (Condition => To_String (Executable) = "True",
              Message   => "Executable mismatch");
      Assert (Condition => To_String (Writable) = "False",
              Message   => "Writable mismatch");
--  begin read only
   end Test_Memory_Perm_Attrs_As_String;
--  end read only


--  begin read only
   procedure Test_Channel_Attrs_As_String (Gnattest_T : in out Test);
   procedure Test_Channel_Attrs_As_String_c33843 (Gnattest_T : in out Test) renames Test_Channel_Attrs_As_String;
--  id:2.2/c3384320b577cc0b/Channel_Attrs_As_String/1/0/
   procedure Test_Channel_Attrs_As_String (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl : DOM.Core.DOM_Implementation;
      Data : Muxml.XML_Data_Type;
      Node : DOM.Core.Node;

      Kind, Vector, Event : Unbounded_String;

      Ref_Kind   : constant String := "writer";
      Ref_Vector : constant String := "12";
      Ref_Event  : constant String := "245";
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "memory");

      begin
         Channel_Attrs_As_String
           (Node   => Node,
            Kind   => Kind,
            Vector => Vector,
            Event  => Event);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when Attribute_Error => null;
      end;

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "writer");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "vector",
         Value => Ref_Vector);
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "event",
         Value => Ref_Event);

      Channel_Attrs_As_String
        (Node   => Node,
         Kind   => Kind,
         Vector => Vector,
         Event  => Event);
      Assert (Condition => To_String (Kind) = Ref_Kind,
              Message   => "Channel kind mismatch");
      Assert (Condition => To_String (Vector) = Ref_Vector,
              Message   => "Vector mismatch");
      Assert (Condition => To_String (Event) = Ref_Event,
              Message   => "Event mismatch");
--  begin read only
   end Test_Channel_Attrs_As_String;
--  end read only


--  begin read only
   procedure Test_Device_Ioport_Attrs_As_String (Gnattest_T : in out Test);
   procedure Test_Device_Ioport_Attrs_As_String_8edead (Gnattest_T : in out Test) renames Test_Device_Ioport_Attrs_As_String;
--  id:2.2/8edead699face4b3/Device_Ioport_Attrs_As_String/1/0/
   procedure Test_Device_Ioport_Attrs_As_String (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl : DOM.Core.DOM_Implementation;
      Data : Muxml.XML_Data_Type;
      Node : DOM.Core.Node;

      Logical, P_Start, P_End : Unbounded_String;

      Ref_Name  : constant String := "ioport1";
      Ref_Start : constant String := "16#03f8#";
      Ref_End   : constant String := "16#03ff#";
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "ioPort");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "logical",
         Value => Ref_Name);

      begin
         Device_Ioport_Attrs_As_String
           (Port     => Node,
            Logical  => Logical,
            IO_Start => P_Start,
            IO_End   => P_End);
         Assert (Condition => False,
                 Message   => "Exception expected (1)");

      exception
         when E : Attribute_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Device I/O port node does not provide expected "
                    & "attributes",
                    Message   => "Exception mismatch (1)");
      end;

      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "start",
         Value => Ref_Start);

      begin
         Device_Ioport_Attrs_As_String
           (Port     => Node,
            Logical  => Logical,
            IO_Start => P_Start,
            IO_End   => P_End);
         Assert (Condition => False,
                 Message   => "Exception expected (2)");

      exception
         when E : Attribute_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Device I/O port node does not provide expected "
                    & "attributes",
                    Message   => "Exception mismatch (1)");
      end;

      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "end",
         Value => Ref_End);
      Device_Ioport_Attrs_As_String
           (Port     => Node,
            Logical  => Logical,
            IO_Start => P_Start,
            IO_End   => P_End);
      Assert (Condition => To_String (Logical) = Ref_Name,
              Message   => "Name mismatch");
      Assert (Condition => To_String (P_Start) = Ref_Start,
              Message   => "Start mismatch");
      Assert (Condition => To_String (P_End) = Ref_End,
              Message   => "End mismatch");
--  begin read only
   end Test_Device_Ioport_Attrs_As_String;
--  end read only


--  begin read only
   procedure Test_Device_Irq_Attrs_As_String (Gnattest_T : in out Test);
   procedure Test_Device_Irq_Attrs_As_String_74d5bb (Gnattest_T : in out Test) renames Test_Device_Irq_Attrs_As_String;
--  id:2.2/74d5bbf01e196674/Device_Irq_Attrs_As_String/1/0/
   procedure Test_Device_Irq_Attrs_As_String (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl : DOM.Core.DOM_Implementation;
      Data : Muxml.XML_Data_Type;
      Node : DOM.Core.Node;

      Logical, Vector : Unbounded_String;

      Ref_Name   : constant String := "irq1";
      Ref_Vector : constant String := "16#78#";
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "irq");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "logical",
         Value => Ref_Name);

      begin
         Device_Irq_Attrs_As_String
           (Irq     => Node,
            Logical => Logical,
            Vector  => Vector);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Attribute_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Device irq node does not provide expected attributes",
                    Message   => "Exception mismatch");
      end;

      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "vector",
         Value => Ref_Vector);
      Device_Irq_Attrs_As_String
        (Irq     => Node,
         Logical => Logical,
         Vector  => Vector);
      Assert (Condition => To_String (Logical) = Ref_Name,
              Message   => "Name mismatch");
      Assert (Condition => To_String (Vector) = Ref_Vector,
              Message   => "Vector mismatch");
--  begin read only
   end Test_Device_Irq_Attrs_As_String;
--  end read only


--  begin read only
   procedure Test_Channel_Reader_Array_Attrs_As_String (Gnattest_T : in out Test);
   procedure Test_Channel_Reader_Array_Attrs_As_String_051ec5 (Gnattest_T : in out Test) renames Test_Channel_Reader_Array_Attrs_As_String;
--  id:2.2/051ec5dbc765b137/Channel_Reader_Array_Attrs_As_String/1/0/
   procedure Test_Channel_Reader_Array_Attrs_As_String (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl : DOM.Core.DOM_Implementation;
      Data : Muxml.XML_Data_Type;
      Node : DOM.Core.Node;

      Vector_Base : Unbounded_String;

      Ref_Base : constant String := "23";
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "reader");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "eventBase",
         Value => Ref_Base);

      begin
         Channel_Reader_Array_Attrs_As_String
           (Arr         => Node,
            Vector_Base => Vector_Base);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Attribute_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Channel reader array specifies invalid 'eventBase' "
                    & "attribute",
                    Message   => "Exception mismatch");
      end;

      DOM.Core.Elements.Remove_Attribute
        (Elem => Node,
         Name => "eventBase");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "vectorBase",
         Value => Ref_Base);

      Channel_Reader_Array_Attrs_As_String
        (Arr         => Node,
         Vector_Base => Vector_Base);
      Assert (Condition => To_String (Vector_Base) = Ref_Base,
              Message   => "Vector base mismatch");
--  begin read only
   end Test_Channel_Reader_Array_Attrs_As_String;
--  end read only


--  begin read only
   procedure Test_Channel_Writer_Array_Attrs_As_String (Gnattest_T : in out Test);
   procedure Test_Channel_Writer_Array_Attrs_As_String_285e6b (Gnattest_T : in out Test) renames Test_Channel_Writer_Array_Attrs_As_String;
--  id:2.2/285e6b688392c974/Channel_Writer_Array_Attrs_As_String/1/0/
   procedure Test_Channel_Writer_Array_Attrs_As_String (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl : DOM.Core.DOM_Implementation;
      Data : Muxml.XML_Data_Type;
      Node : DOM.Core.Node;

      Event_Base : Unbounded_String;

      Ref_Base : constant String := "28";
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "writer");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "vectorBase",
         Value => Ref_Base);

      begin
         Channel_Writer_Array_Attrs_As_String
           (Arr        => Node,
            Event_Base => Event_Base);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Attribute_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Channel writer array specifies invalid 'vectorBase' "
                    & "attribute",
                    Message   => "Exception mismatch");
      end;

      DOM.Core.Elements.Remove_Attribute
        (Elem => Node,
         Name => "vectorBase");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "eventBase",
         Value => Ref_Base);

      Channel_Writer_Array_Attrs_As_String
        (Arr        => Node,
         Event_Base => Event_Base);
      Assert (Condition => To_String (Event_Base) = Ref_Base,
              Message   => "Event base mismatch");
--  begin read only
   end Test_Channel_Writer_Array_Attrs_As_String;
--  end read only


--  begin read only
   procedure Test_Get_Channel_Kind (Gnattest_T : in out Test);
   procedure Test_Get_Channel_Kind_019f06 (Gnattest_T : in out Test) renames Test_Get_Channel_Kind;
--  id:2.2/019f069797309693/Get_Channel_Kind/1/0/
   procedure Test_Get_Channel_Kind (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl : DOM.Core.DOM_Implementation;
      Data : Muxml.XML_Data_Type;
      Node : DOM.Core.Node;
      Kind : Channel_Kind;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "memory");

      begin
         Kind := Get_Channel_Kind (Node => Node);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Attribute_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Unable to determine channel kind of invalid node "
                    & "'memory'",
                    Message   => "Exception mismatch");
      end;

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "writer");
      Assert (Condition => Get_Channel_Kind (Node => Node) = Writer,
              Message   => "Channel kind mismatch (1)");
      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "reader");
      Assert (Condition => Get_Channel_Kind (Node => Node) = Reader,
              Message   => "Channel kind mismatch (2)");
--  begin read only
   end Test_Get_Channel_Kind;
--  end read only

--  begin read only
--  id:2.2/02/
--
--  This section can be used to add elaboration code for the global state.
--
begin
--  end read only
   null;
--  begin read only
--  end read only
end Cspec.Utils.Test_Data.Tests;
