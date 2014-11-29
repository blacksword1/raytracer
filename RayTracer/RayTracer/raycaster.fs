namespace RayTracer

open Rgb
open Vectors
open Types
open Shapes

module RayCaster = 
    
    type RayCaster() = 
        
        let castray (scene : IShape[]) (i : int) (j : int) (ray : Ray) = 
            let hits = 
                scene
                |> Array.map (fun shape -> shape.Hit(ray, 0.00001, 100000.0)) 
                |> Array.filter (fun x -> Option.isSome x)
                |> Array.map (Option.get)
            
            if Array.isEmpty hits
                then None
                else Some <| Array.minBy (fun hit -> hit.t) hits

        member this.CastRay(scene : IShape[], i : int, j : int, ray : Ray) = castray scene i j ray
    

