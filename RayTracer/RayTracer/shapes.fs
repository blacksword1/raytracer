namespace RayTracer

open Rgb
open Vectors
open Types

module Shapes =

    type Triangle = 
        { 
            p0 : Vec3; 
            p1 : Vec3; 
            p2 : Vec3;
            color : Color
        }

        interface IShape with
            member this.Hit(ray : Ray, tmin : float, tmax : float) = 
                let p0 = this.p0
                let p1 = this.p1
                let p2 = this.p2
                
                let A = p0.x - p1.x
                let B = p0.y - p1.y
                let C = p0.z - p1.z

                let D = p0.x - p2.x
                let E = p0.y - p2.y
                let F = p0.z - p2.z

                let G = ray.direction.x
                let H = ray.direction.y
                let I = ray.direction.z

                let J = p0.x - ray.origin.x
                let K = p0.y - ray.origin.y
                let L = p0.z - ray.origin.z

                let EIHF = E * I - H * F
                let GFDI = G * F - D * I
                let DHEG = D * H - E * G

                let denom = A * EIHF + B * GFDI + C * DHEG
                let beta = (J * EIHF + K * GFDI + L * DHEG) / denom

                if beta <= 0.0 || beta >= 1.0
                    then None
                    else
                        let AKJB = A * K - J * B
                        let JCAL = J * C - A * L
                        let BLKC = B * L - K * C

                        let gamma = (I * AKJB + H * JCAL + G * BLKC) / denom
                        
                        if gamma <= 0.0 || beta + gamma >= 1.0
                            then None
                            else
                                let tval = -(F * AKJB + E * JCAL + D * BLKC) / denom

                                if tval >= tmin && tval <= tmax
                                    then
                                        Some 
                                            {
                                                t = tval
                                                normal = Vec3.cross(p1 - p0, p2 - p0).normal() 
                                                color = this.color  
                                            }
                                    else None

            member this.ShadowHit(ray : Ray, tmin : float, tmax : float) = 
                let p0 = this.p0
                let p1 = this.p1
                let p2 = this.p2
                
                let A = p0.x - p1.x
                let B = p0.y - p1.y
                let C = p0.z - p1.z

                let D = p0.x - p2.x
                let E = p0.y - p2.y
                let F = p0.z - p2.z

                let G = ray.direction.x
                let H = ray.direction.y
                let I = ray.direction.z

                let J = p0.x - ray.origin.x
                let K = p0.y - ray.origin.y
                let L = p0.z - ray.origin.z

                let EIHF = E * I - H * F
                let GFDI = G * F - D * I
                let DHEG = D * H - E * G

                let denom = A * EIHF + B * GFDI + C * DHEG
                let beta = (J * EIHF + K * GFDI + L * DHEG) / denom

                if beta <= 0.0 || beta >= 1.0
                    then false
                    else
                        let AKJB = A * K - J * B
                        let JCAL = J * C - A * L
                        let BLKC = B * L - K * C

                        let gamma = (I * AKJB + H * JCAL + G * BLKC) / denom
                        
                        if gamma <= 0.0 || beta + gamma >= 1.0
                            then false
                            else
                                let tval = -(F * AKJB + E * JCAL + D * BLKC) / denom

                                tval >= tmin && tval <= tmax

    type Sphere = 
        {
            center : Vec3
            radius : float
            color  : Color
        }

        interface IShape with
            member this.Hit(ray : Ray, tmin : float, tmax : float) =
                let temp = ray.origin - this.center
                
                let a = Vec3.dot(ray.direction, ray.direction)
                let b = 2.0 * Vec3.dot(ray.direction, temp)
                let c = Vec3.dot(temp, temp) - this.radius * this.radius

                let discriminant = b * b - 4.0 * a * c

                if discriminant > 0.0
                    then
                        let sqDiscriminant = sqrt discriminant
                        let t = (-b - sqDiscriminant) / (2.0*a)

                        let inline checkt t = 
                            if t < tmin || t > tmax
                                then None
                                else 
                                    { 
                                        t = t; 
                                        normal = (ray.origin + t * ray.direction - this.center).normal(); 
                                        color = this.color 
                                    } |> Some
                         
                        if t < tmin
                            then checkt ((-b + sqDiscriminant) / (2.0*a))
                            else checkt t
                    else None
            
            member this.ShadowHit(ray : Ray, tmin : float, tmax : float) =
                let temp = ray.origin - this.center
                
                let a = Vec3.dot(ray.direction, ray.direction)
                let b = 2.0 * Vec3.dot(ray.direction, temp)
                let c = Vec3.dot(temp, temp) - this.radius * this.radius

                let discriminant = b * b - 4.0 * a * c

                if discriminant > 0.0
                    then
                        let sqDiscriminant = sqrt discriminant
                        let t = (-b - sqDiscriminant) / (2.0*a)

                        let inline checkt t = t < tmin || t > tmax
                         
                        if t < tmin
                            then checkt ((-b + sqDiscriminant) / (2.0*a))
                            else checkt t
                    else false

