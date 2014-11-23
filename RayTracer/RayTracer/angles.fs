namespace RayTracer

open System

module Angles = 
    [<Measure>] type deg
    [<Measure>] type rad

    let private degToRadConstant : float<rad / deg> = (180.0 / Math.PI) * 1.0<rad / deg>
    let private radToDegConstant : float<deg / rad> = (Math.PI / 180.0) * 1.0<deg / rad>

    let degToRad (deg : float<deg>) : float<rad> = degToRadConstant * deg
    let radToDeg (rad : float<rad>) : float<deg> = radToDegConstant * rad
