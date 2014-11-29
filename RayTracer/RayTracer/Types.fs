namespace RayTracer

open Rgb
open Vectors

module Types = 
    type HitData = { t : float; normal : NVec3; color : Color }
    type Ray = { origin : Vec3; direction : Vec3 }

    type IShape = 
        abstract member Hit       : Ray * float * float -> HitData option
        abstract member ShadowHit : Ray * float * float -> bool


    
