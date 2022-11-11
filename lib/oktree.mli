module Make :
  functor (V3 : Oktree_intf.VEC3) -> Oktree_intf.OKTREE with type vec3 = V3.t
