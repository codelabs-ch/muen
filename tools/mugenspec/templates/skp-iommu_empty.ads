package Skp.IOMMU
with
   Abstract_State =>
     (State with External => (Async_Writers, Async_Readers, Effective_Writes)),
   Initializes    => State,
   Elaborate_Body
is
end Skp.IOMMU;
