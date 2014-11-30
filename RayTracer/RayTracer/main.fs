namespace RayTracer

open System
open System.Threading
open System.Threading.Tasks

open Image
open Rgb
open Vectors
open Sampling
open Types
open Shapes
open RayCaster

module RTViewer = 

    let test1() = 
        let scene : IShape [] = 
            [|
                { center = Vec3(250.0, 250.0, -1000.0); radius = 150.0; color = Color(0.2, 0.2, 0.8) } 
                { p0 = Vec3(300.0, 600.0, -800.0); p1 = Vec3(0.0, 100.0, -1000.0); p2 = Vec3(450.0, 20.0, -1000.0); color = Color(0.8, 0.2, 0.2)}
            |]

        let output : Color [,] = Array2D.init 500 500 (fun _ _ -> Color(0.0, 0.0, 0.0)) 

        let direction = Vec3(0.0, 0.0, -1.0)

        let raycaster = RayCaster()

        let rnd = Random()
        let n_samples_per_pixel = 16
        let width = 500
        let height = 500
        let samples = Array.init (500 * 500) (fun _ -> jitter rnd n_samples_per_pixel) 

        let stopWatch = System.Diagnostics.Stopwatch.StartNew()

//        let tasks = Array.zeroCreate 500
//        for i in 0 .. 499 do
//            tasks.[i] <- Task.Factory.StartNew(fun () -> 
//                for j in 0 .. 499 do 
//                    let ray = { origin = Vec3(i |> float, j |> float, 0.0); direction = direction }
//                    output.[499 - j, i] <- match castray scene i j ray with | None -> Color(0.2, 0.2, 0.2) | Some hit -> hit.color)
//        Task.WaitAll(tasks)

        for i in 0 .. width-1 do
            for j in 0 .. height-1 do 
                for k in 0 .. n_samples_per_pixel-1 do 
                    let sample = samples.[i * width + j].[k]
//                    let x = ((float i) - 0.5 * (float width) + sample.x);
//                    let y = ((float j) - 0.5 * (float height) + sample.y);
                    let x = ((float i) - 0.5 + sample.x)
                    let y = ((float j) - 0.5 + sample.y)
                    let ray = { origin = Vec3(x, y, 0.0); direction = direction }
//                    let ray = { origin = Vec3(i |> float, j |> float, 0.0); direction = direction }

                    output.[499 - j, i] <- 
                        output.[499 - j, i] + 
                        (match raycaster.CastRay(scene, ray) with 
                        | None -> Color(0.2, 0.2, 0.2) 
                        | Some hit -> hit.color) / (float n_samples_per_pixel)

        stopWatch.Stop()
        printfn "%f" stopWatch.Elapsed.TotalMilliseconds

        let bitmap = BitmapImage.savePixels(output, 1.0)
        
        bitmap.Save("test.bmp")

    [<EntryPoint>]
    let main args =
        
        test1()

        0
