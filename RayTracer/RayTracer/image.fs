namespace RayTracer

open System.Drawing
open System.Drawing.Imaging

open Rgb

module Image = 
    let getPixels (image:Bitmap) =
        let width = image.Width
        let height = image.Height
        let rect = new Rectangle(0, 0, width, height)

        // Lock the image for access
        let data = image.LockBits(rect, ImageLockMode.ReadOnly, image.PixelFormat)

        // Copy the data
        let ptr = data.Scan0
        let stride = data.Stride
        let bytes = stride * data.Height
        let values : byte[] = Array.zeroCreate bytes
        System.Runtime.InteropServices.Marshal.Copy(ptr, values, 0, bytes)

        // Unlock the image
        image.UnlockBits(data)

        let pixelSize = 4 //@TODO: calculate this from the PixelFormat

        //@TODO: Gamma correction
        Array2D.init 
            width 
            height 
            (fun x y -> 
                Color(
                    values.[stride * y + x * pixelSize + 0] |> float |> ((/) 255.0),
                    values.[stride * y + x * pixelSize + 1] |> float |> ((/) 255.0),
                    values.[stride * y + x * pixelSize + 2] |> float |> ((/) 255.0))
            )

    let setPixels (pixels : Color [,]) gamma : Bitmap = 
        let width = pixels.GetLength 0
        let height = pixels.GetLength 1
        let invGamma = 1.0 / gamma

        let b = new Bitmap(width, height, PixelFormat.Format32bppRgb);

//        ColorPalette ncp = b.Palette;
//        Array.iteri (fun i -> npc[i] <- Color.FromArgb(255, ))
//            ncp.Entries[i] = Color.FromArgb(255, i, i, i);
//        b.Palette = ncp;

        let boundsRect = new Rectangle(0, 0, width, height);
        let bmpData = b.LockBits(boundsRect,
                                        ImageLockMode.WriteOnly,
                                        b.PixelFormat);

        let ptr = bmpData.Scan0;

        let bytes = bmpData.Stride*b.Height;        
        let pixels1D = 
            pixels
            |> Seq.cast<Color>
            |> Array.ofSeq
        let rgbValues : byte[] = 
            Array.init 
                bytes 
                (fun i -> 
                    match i % 4 with
                    | 0 -> (pixels1D.[i / 4].r ** invGamma) * 256.0 |> byte
                    | 1 -> (pixels1D.[i / 4].g ** invGamma) * 256.0 |> byte
                    | 2 -> (pixels1D.[i / 4].b ** invGamma) * 256.0 |> byte
                    | _ -> byte 255)

        System.Runtime.InteropServices.Marshal.Copy(rgbValues, 0, ptr, bytes);
        b.UnlockBits(bmpData);
        
        b

    type BitmapImage() =  
        static member readPixels(image : Bitmap) = getPixels image 
        static member savePixels(pixels : Color [,], ?gamma : float) = setPixels pixels (defaultArg gamma 1.0)