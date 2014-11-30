namespace RayTracer

open System

open Vectors

module Sampling = 
    let genRandomArray (rnd : Random) n = Array.init n (fun _ -> rnd.NextDouble())

    let genRandomVec2Array (rnd : Random) n = Array.init n (fun _ -> Vec2(rnd.NextDouble(), rnd.NextDouble()))

    let inline private genSample (rnd : Random) i j n = 
        let x = ((i |> double) + (rnd.NextDouble() |> double)) / (n |> double)
        let y = ((j |> double) + (rnd.NextDouble() |> double)) / (n |> double)
        (x, y)

    /// Assumes n is a perfect square
    let jitter (rnd : Random) (n : int) = 
        let sqrt_samples = n |> float |> sqrt |> int

        let samples = Array.init n (fun _ -> Vec2(0.0, 0.0))

        for i in 0 .. sqrt_samples-1 do
            for j in 0 .. sqrt_samples-1 do
                let x, y = genSample rnd i j sqrt_samples
                samples.[i * sqrt_samples + j].elements.[0] <- x |> float
                samples.[i * sqrt_samples + j].elements.[1] <- y |> float

        samples

    let nrooks (rnd : Random) (n : int) = 
        let samples = Array.init n (fun i -> genSample rnd i i n |> fun (x, y) -> Vec2(x, y))

        //shuffle the x coords
        for i in 0 .. n - 2 do
            let target = ((double <| rnd.NextDouble()) * (double i)) |> int
            let temp = samples.[i+1].x
            samples.[i+1].elements.[0] <- samples.[target].x
            samples.[target].elements.[0] <- temp

        samples
