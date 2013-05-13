with System.Storage_Elements;

package body SK.Subjects
--# own Descriptors is Subject_Descs, Posted_Int_Descs;
is

   --# hide SK.Subjects;

   subtype Descriptor_Array is SK.Subject_State_Array (Skp.Subject_Id_Type);

   --  Descriptors used to manage subject states.
   --# accept Warning, 396, Subject_Descs, "Not an external variable";
   Subject_Descs : Descriptor_Array;
   for Subject_Descs'Address use System'To_Address (16#001fe000#);
   --# end accept;

   --  Posted-Interrupt Descriptor, as specified in Intel SDM Vol. 3C, section
   --  29.6.

   type PID_Quadword_Range is range 1 .. 8;

   type Posted_Int_Descriptor_Type is array (PID_Quadword_Range) of SK.Word64;
   for Posted_Int_Descriptor_Type'Size use 512;
   --  for Posted_Int_Descriptor_Type'Alignment use 512;

   type Posted_Int_Descriptor_Array is array (Skp.Subject_Id_Type)
     of Posted_Int_Descriptor_Type;

   Posted_Int_Descs : Posted_Int_Descriptor_Array;

   type Page_Range is range 1 .. 512;
   type VAPIC_Type is array (Page_Range) of SK.Word64;

   VT_VAPIC : VAPIC_Type;
   for VT_VAPIC'Address use System'To_Address (16#1fb000#);

   -------------------------------------------------------------------------

   function Get_Posted_Int_Descriptor_Addr
     (Id : Skp.Subject_Id_Type)
      return SK.Word64
   --# global
   --#    Posted_Int_Descs;
   is
      --# hide Get_Posted_Int_Descriptor_Addr;
      Offset : SK.Word64;
   begin
      Offset := SK.Word64 (Id * Posted_Int_Descriptor_Type'Size);
      return SK.Word64
        (System.Storage_Elements.To_Integer
           (Value => Posted_Int_Descs'Address)) + Offset;
   end Get_Posted_Int_Descriptor_Addr;

   -------------------------------------------------------------------------

   function Get_State (Id : Skp.Subject_Id_Type) return SK.Subject_State_Type
   --# global
   --#    Subject_Descs;
   is
   begin
      return Subject_Descs (Id);
   end Get_State;

   -------------------------------------------------------------------------

   procedure Set_State
     (Id    : Skp.Subject_Id_Type;
      State : SK.Subject_State_Type)
   --# global
   --#    Subject_Descs;
   --# derives
   --#    Subject_Descs from *, Id, State;
   is
      Vector : SK.Byte;
   begin
      Vector := Subject_Descs (Id).Pending_Event;
      Subject_Descs (Id) := State;
      Subject_Descs (Id).Pending_Event := Vector;
   end Set_State;

   -------------------------------------------------------------------------

   procedure Set_Pending_Event
     (Id     : Skp.Subject_Id_Type;
      Vector : SK.Byte)
   --# global
   --#    in out Subject_Descs;
   --#    in out Posted_Int_Descs;
   --# derives
   --#    Subject_Descs, Posted_Int_Descs from *, Id, Vector;
   is
   begin
      Subject_Descs (Id).Pending_Event := Vector;
   end Set_Pending_Event;

begin
   Subject_Descs := Descriptor_Array'
     (others => SK.Subject_State_Type'
        (SK.Null_Subject_State));
   Posted_Int_Descs := Posted_Int_Descriptor_Array'
     (others => Posted_Int_Descriptor_Type'
        (others => 0));
   VT_VAPIC := VAPIC_Type'(others => SK.Word64'Last);
end SK.Subjects;
