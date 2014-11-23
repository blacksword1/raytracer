namespace RayTracer

open Vectors

module Points = 
    
    type Point2(x : float, y : float) =
        let data = [| x ; y |]

        member this.x = data.[0]
        member this.y = data.[1]

        member this.elements = data
        
        static member (~+) (p : Point2) = Point2(p.x, p.y)
        static member (~-) (p : Point2) = Point2(-p.x, -p.y)

        static member (*) (p : Point2, k) = Point2(k * p.x, k * p.y)
        static member (*) (k, p : Point2) = Point2(k * p.x, k * p.y)

        static member (+) (p : Point2, u : Vec2) = Point2(p.x + u.x, p.y + u.y)

        static member (-) (p : Point2, u : Vec2) = Point2(p.x - u.x, p.y - u.y)
        static member (-) (p1 : Point2, p2 : Point2) = Vec2(p1.x - p2.x, p1.y - p2.y)

    type Point3(x : float, y : float, z : float) =
        let data = [| x ; y ; z |]

        member this.x = data.[0]
        member this.y = data.[1]
        member this.z = data.[2]

        member this.elements = data
        
        static member (~+) (p : Point3) = Point3(p.x, p.y, p.z)
        static member (~-) (p : Point3) = Point3(-p.x, -p.y, -p.z)

        static member (*) (p : Point3, k) = Point3(k * p.x, k * p.y, k * p.z)
        static member (*) (k, p : Point3) = Point3(k * p.x, k * p.y, k * p.z)

        static member (+) (p : Point3, u : Vec3) = Point3(p.x + u.x, p.y + u.y, p.z + u.z)

        static member (-) (p : Point3, u : Vec3) = Point3(p.x - u.x, p.y - u.y, p.z - u.z)
        static member (-) (p1 : Point3, p2 : Point3) = Vec3(p1.x - p2.x, p1.y - p2.y, p1.z - p2.z)


