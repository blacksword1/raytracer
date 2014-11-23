namespace RayTracer

open Vectors

module ONB = 
    let ONB_EPSILON = 0.01

    type ONB(u : Vec3, v : Vec3, w : Vec3) =
        member this.U = u
        member this.V = v
        member this.W = w
        
        static member fromU(u : Vec3) = 
            let n = Vec3(1.0, 0.0, 0.0)
            let m = Vec3(0.0, 1.0, 0.0)
            let U = u.unit()
            match Vec3.cross(U, n) with
            | V when V.len() < ONB_EPSILON -> 
                let V = Vec3.cross(U,m)
                ONB(U, V, Vec3.cross(U,V))
            | V -> ONB(U, V, Vec3.cross(U,V))

        static member fromV(v : Vec3) = 
            let n = Vec3(1.0, 0.0, 0.0)
            let m = Vec3(0.0, 1.0, 0.0)

            let V = v.unit()
            match Vec3.cross(V, n) with
            | U when U.len2() < ONB_EPSILON ->
                let U = Vec3.cross(V, m)
                ONB(U, V, Vec3.cross(U, V))
            | U -> ONB(U, V, Vec3.cross(U,V))

        static member fromW(w : Vec3) = 
            let n = Vec3(1.0, 0.0, 0.0)
            let m = Vec3(0.0, 1.0, 0.0)

            let W = w.unit()
            match Vec3.cross(W, n) with
            | U when U.len() < ONB_EPSILON ->
                let U = Vec3.cross(W, m)
                ONB(U, Vec3.cross(W, U), W)
            | U -> ONB(U, Vec3.cross(W,U), W)

        static member fromUV(u : Vec3, v : Vec3) = 
            let U = u.unit()
            let W = Vec3.cross(u, v).unit()
            ONB(U, Vec3.cross(W, U), W)

        static member fromVU(v : Vec3, u : Vec3) = 
            let V = v.unit()
            let W = Vec3.cross(u, v).unit()
            ONB(Vec3.cross(V, W), V, W)
        
        static member fromUW(u : Vec3, w : Vec3) = 
            let U = u.unit()
            let V = Vec3.cross(w, u).unit()
            ONB(U, V, Vec3.cross(U, V))

        static member fromWU(w : Vec3, u : Vec3) = 
            let W = w.unit()
            let V = Vec3.cross(w, u).unit()
            ONB(Vec3.cross(V, W), V, W)
        
        static member fromVW(v : Vec3, w : Vec3) = 
            let V = v.unit()
            let U = Vec3.cross(v, w).unit()
            ONB(U, V, Vec3.cross(U, V))

        static member fromWV(w : Vec3, v : Vec3) =
            let W = w.unit()
            let U = Vec3.cross(v, w).unit()
            ONB(U, Vec3.cross(W, U), W)

