namespace RayTracer

module Vectors = 
    type Vec2(x : float, y : float) = 
        let data = [| x ; y |]

        member this.x = data.[0]
        member this.y = data.[1]

        member this.elements = data

        /// Length of the vector
        member this.len() = this.x * this.x + this.y * this.y |> sqrt
        /// Square lenght of the vector
        member this.len2() = this.x * this.x + this.y * this.y

        member this.unit() = 
            let invLen = 1.0 / this.len()
            Vec2(this.x * invLen, this.y * invLen)

        member this.normal() = 
            let invLen = 1.0 / this.len()
            NVec2(this.x * invLen, this.y * invLen)

        static member (~+) (v : Vec2) = Vec2(v.x, v.y)
        static member (~-) (v : Vec2) = Vec2(-v.x, -v.y)

        static member (*) (v : Vec2, k) = Vec2(k * v.x, k * v.y)
        static member (*) (k, v : Vec2) = Vec2(k * v.x, k * v.y)

        static member (/) (v : Vec2, k) = Vec2(v.x / k, v.y / k)

        static member (+) (v1 : Vec2, v2 : Vec2) = Vec2(v1.x + v2.x, v1.y + v2.y)
        static member (-) (v1 : Vec2, v2 : Vec2) = Vec2(v1.x - v2.x, v1.y - v2.y)

        /// dot v1 v2 = v1.x * v2.x + v1.y * v2.y = || v1 || * ||v2 || * cos(angle_between_v1_and_v2)
        static member dot (v1 : Vec2, v2 : Vec2) = v1.x * v2.x + v1.y * v2.y
        /// http://allenchou.net/2013/07/cross-product-of-2d-vectors/
        /// || cross v1 v2 || = || v1 || * || v2 || * sin(angle_between_v1_and_v2)
        /// See image on page 5 in Realistic Ray Tracing (Shirley)
        static member cross (v1 : Vec2, v2 : Vec2) = v1.x * v2.y - v1.y * v2.x

    and NVec2(x : float, y : float) = 
        let data = [| x ; y |]

        member this.x = data.[0]
        member this.y = data.[1]

        member this.elements = data

        member this.vec2() = Vec2(x, y)

        static member (~+) (n : NVec2) = NVec2(n.x, n.y)
        static member (~-) (n : NVec2) = NVec2(-n.x, -n.y)

        static member (+) (n1 : NVec2, n2 : NVec2) = NVec2(n1.x + n2.x, n1.y + n2.y)
        static member (+) (n : NVec2, u : Vec2) = Vec2(n.x + u.x, n.y + u.y)
        static member (+) (u : Vec2, n : NVec2) = Vec2(n.x + u.x, n.y + u.y)

        static member dot (n : NVec2, u : Vec2) = n.x * u.x + n.y * u.y
        static member dot (u : Vec2, n : NVec2) = n.x * u.x + n.y * u.y

        static member (*) (k, n : NVec2) = NVec2(k * n.x, k * n.y)
        static member (*) (n : NVec2, k) = NVec2(k * n.x, k * n.y)

    type Vec3(x : float, y : float, z : float) = 
        let data = [| x ; y ; z |]

        member this.x = data.[0]
        member this.y = data.[1]
        member this.z = data.[2]

        member this.elements = data

        /// Length of the vector
        member this.len() = this.x * this.x + this.y * this.y + this.z * this.z |> sqrt
        /// Square lenght of the vector
        member this.len2() = this.x * this.x + this.y * this.y + this.z * this.z

        member this.unit() = 
            let invLen = 1.0 / this.len()
            Vec3(this.x * invLen, this.y * invLen, this.z * invLen)

        member this.normal() = 
            let invLen = 1.0 / this.len()
            NVec3(this.x * invLen, this.y * invLen, this.z * invLen)

        static member (~+) (v : Vec3) = Vec3(v.x, v.y, v.z)
        static member (~-) (v : Vec3) = Vec3(-v.x, -v.y, -v.z)

        static member (*) (v : Vec3, k) = Vec3(k * v.x, k * v.y, k * v.z)
        static member (*) (k, v : Vec3) = Vec3(k * v.x, k * v.y, k * v.z)

        static member (/) (v : Vec3, k) = Vec3(v.x / k, v.y / k, v.z / k)

        static member (+) (v1 : Vec3, v2 : Vec3) = Vec3(v1.x + v2.x, v1.y + v2.y, v1.z + v2.z)
        static member (-) (v1 : Vec3, v2 : Vec3) = Vec3(v1.x - v2.x, v1.y - v2.y, v1.z - v2.z)

        /// dot v1 v2 = v1.x * v2.x + v1.y * v2.y + v1.z * v2.z = || v1 || * ||v2 || * cos(angle_between_v1_and_v2)
        static member dot (v1 : Vec3, v2 : Vec3) = v1.x * v2.x + v1.y * v2.y + v1.z * v2.z
        /// || cross v1 v2 || = || v1 || * || v2 || * sin(angle_between_v1_and_v2)
        /// See image on page 5 in Realistic Ray Tracing (Shirley)
        static member cross (v1 : Vec3, v2 : Vec3) = Vec3(v1.y * v2.z - v1.z * v2.y, v1.z * v2.x - v1.x * v2.z, v1.x * v2.y - v1.y * v2.x)

    and NVec3(x : float, y : float, z : float) = 
        let data = [| x ; y ; z |]

        member this.x = data.[0]
        member this.y = data.[1]
        member this.z = data.[2]

        member this.elements = data

        member this.vec3() = Vec3(x, y, z)

        static member (~+) (n : NVec3) = NVec3(n.x, n.y, n.z)
        static member (~-) (n : NVec3) = NVec3(-n.x, -n.y, -n.z)

        static member (+) (n1 : NVec3, n2 : NVec3) = NVec3(n1.x + n2.x, n1.y + n2.y, n1.z + n2.z)
        static member (+) (n : NVec3, u : Vec3) = Vec3(n.x + u.x, n.y + u.y, n.z + u.z)
        static member (+) (u : Vec3, n : NVec3) = Vec3(n.x + u.x, n.y + u.y, n.z + u.z)

        static member dot (n : NVec3, u : Vec3) = n.x * u.x + n.y * u.y + n.z * u.z
        static member dot (u : Vec3, n : NVec3) = n.x * u.x + n.y * u.y + n.z * u.z

        static member (*) (k, n : NVec3) = NVec3(k * n.x, k * n.y, k * n.z)
        static member (*) (n : NVec3, k) = NVec3(k * n.x, k * n.y, k * n.z)
    
