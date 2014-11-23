namespace RayTracer

module Rgb = 
    type Color(r : float, g : float, b : float) =
        member this.r = r
        member this.g = g
        member this.b = b
        
        static member (~+) (c : Color) = Color(c.r, c.g, c.b)
        static member (~-) (c : Color) = Color(-c.r, -c.g, -c.b)
        
        static member (*) (c : Color, k) = Color(k * c.r, k * c.g, k * c.b)
        static member (*) (k, c : Color) = Color(k * c.r, k * c.g, k * c.b) 

        static member (/) (c : Color, k) = Color(c.r / k, c.g / k, c.b / k)

        static member (+) (c1 : Color, c2 : Color) = Color(c1.r + c2.r, c1.g + c2.g, c1.b + c2.b)
        static member (-) (c1 : Color, c2 : Color) = Color(c1.r - c2.r, c1.g - c2.g, c1.b - c2.b)
        static member (*) (c1 : Color, c2 : Color) = Color(c1.r * c2.r, c1.g * c2.g, c1.b * c2.b)
        static member (/) (c1 : Color, c2 : Color) = Color(c1.r / c2.r, c1.g / c2.g, c1.b / c2.b)


