//// F#-based PPM image library.
//
// Krishna Vamsi Chandu
// U. of Illinois, Chicago
// CS341, Spring 2018
// Project 06
//

module PPMImageLibrary

#light


//
// DebugOutput:
//
// Outputs to console, which appears in the "Output" window pane of
// Visual Studio when you run with debugging (F5).
//
let rec private OutputImage (image:(int*int*int) list list) = 
  match image with
  | [] -> printfn "**END**"
  | hd::tl -> printfn "%A" hd
              OutputImage tl
           
let DebugOutput(width:int, height:int, depth:int, image:(int*int*int) list list) =
  printfn "**HEADER**"
  printfn "W=%A, H=%A, D=%A" width height depth
  printfn "**IMAGE**"
  OutputImage image


//
// TransformFirstThreeRows:
//
// An example transformation: replaces the first 3 rows of the given image
// with a row of Red, White and Blue pixels (go USA :-).
//
let rec BuildRowOfThisColor row color = 
  match row with
  | []     -> []
  | hd::tl -> color :: BuildRowOfThisColor tl color

let TransformFirstThreeRows(width:int, height:int, depth:int, image:(int*int*int) list list) = 
  let row1 = List.head image
  let row2 = List.head (List.tail image)
  let row3 = List.head (List.tail (List.tail image))
  let tail = List.tail (List.tail (List.tail image))
  let newRow1 = BuildRowOfThisColor row1 (255,0,0)      // red:
  let newRow2 = BuildRowOfThisColor row2 (255,255,255)  // white:
  let newRow3 = BuildRowOfThisColor row3 (0,0,255)      // blue:
  let newImage = newRow1 :: newRow2 :: newRow3 :: tail
  newImage


//
// WriteP3Image:
//
// Writes the given image out to a text file, in "P3" format.  Returns true if successful,
// false if not.
//
let Flatten (SL:string list) = 
  List.reduce (fun s1 s2 -> s1 + " " + s2) SL

let Image2ListOfStrings (image:(int*int*int) list list) = 
  List.map (fun TL -> List.map (fun (r,g,b) -> r.ToString()+" "+g.ToString()+" "+b.ToString()+" ") TL) image
  |> List.map Flatten

let rec WriteP3Image(filepath:string, width:int, height:int, depth:int, image:(int*int*int) list list) = 
  let L = [ "P3" ] @ 
          [ System.Convert.ToString(width); System.Convert.ToString(height) ] @
          [ System.Convert.ToString(depth) ] @
          (Image2ListOfStrings image)
  System.IO.File.WriteAllLines(filepath, L)
  true  // success




//
// Grayscale:
//
// Converts the image into grayscale and returns the resulting image as a list of lists. 
// Conversion to grayscale is done by averaging the RGB values for a pixel, and then 
// replacing them all by that average. So if the RGB values were 25 75 250, the average 
// would be 116, and then all three RGB values would become 116 — i.e. 116 116 116.
//

let gray (a,b,c) = 
    let avg = (a+b+c)/3
    (avg,avg,avg)

let rec Grayscale(width:int, height:int, depth:int, image:(int*int*int) list list) = 
    let L = List.map(fun a -> List.map( fun b -> gray b) a) image 
    L



//
// Threshold
//
// Thresholding increases image separation --- dark values become darker and light values
// become lighter.  Given a threshold value in the range 0 < threshold < MaxColorDepth,
// all RGB values > threshold become the max color depth (white) while all RGB values
// <= threshold become 0 (black).  The resulting image is returned.
//

let thresh (a,b,c) threshold depth = 
    if a > threshold then
        if b > threshold then
            if c > threshold then 
                (depth,depth,depth)
            else
                (depth,depth,0)
        else
            if c > threshold then 
                (depth,0,depth)
            else
                (depth,0,0)
    else
        if b > threshold then
            if c > threshold then 
                (0,depth,depth)
            else
                (0,depth,0)
        else
            if c > threshold then 
                (0,0,depth)
            else
                (0,0,0)


let rec Threshold(width:int, height:int, depth:int, image:(int*int*int) list list, threshold:int) = 
    let L = List.map(fun a -> List.map( fun b -> thresh b threshold depth) a) image 
    L



//
// FlipHorizontal:
//
// Flips an image so that what’s on the left is now on the right, and what’s on 
// the right is now on the left. That is, the pixel that is on the far left end
// of the row ends up on the far right of the row, and the pixel on the far right 
// ends up on the far left. This is repeated as you move inwards toward the center 
// of the row.
//
let rec FlipHorizontal(width:int, height:int, depth:int, image:(int*int*int) list list) = 
    let L = List.map(fun a -> List.rev a) image
    L

    



//
// Zoom:
//
// Zooms the image by the given zoom factor, which is an integer 0 < factor < 5.  
// The function uses the nearest neighbor approach where each pixel P in the original 
// image is replaced by a factor*factor block of P pixels.  For example, if the zoom 
// factor is 4, then each pixel is replaced by a 4x4 block of 16 identical pixels. 
// The nearest neighbor algorithm is the simplest zoom algorithm, but results in 
// jagged images.  The resulting image is returned.
//

let rec replicateRow factor list accum= 
    if factor = 0 then 
        accum
    else 
        replicateRow (factor-1) list (list :: accum)
 
let rec replicateItem factor list accum = 
    if factor = 0 then 
        accum
    else 
        replicateItem (factor-1) list (list :: accum)

let rec _item factor list accum = 
    match list with 
    | a :: rest -> _item factor rest (replicateItem factor a accum) 
    | [] -> List.rev accum

let rec _zoom image factor accum =
    match image with
    | a :: rest -> _zoom rest factor (replicateRow factor (_item factor a []) accum)
    | [] -> List.rev accum 

let rec Zoom(width:int, height:int, depth:int, image:(int*int*int) list list, factor:int) = 
    let L = _zoom image factor []
    L


//
// RotateRight90:
//
// Rotates the image to the right 90 degrees.
//
//let rec addToList item width accum = 
//    item :: (List.item(width) accum) 
//    accum

//let rec addToImage list width accum = 
//    if width = 0 then
//        List.rev accum 
//    else
//        addToImage list (width - 1) (addToList (List.item(width) list) width accum)
        
//let rec _rotate list height width accum = 
//    if height < 0 then 
//        accum 
//    else
//        _rotate list (height-1) width (addToImage (List.item(height) list) width accum) 

//let rec createEmpty width accum = 
//    if(width = 0) then 
//        accum 
//    else 
//        createEmpty (width - 1) ([] :: accum)

//let elementToIamge element image = 


//let rec rowToImage row image = 
//    match row with 
//    | a :: rest -> rowToImage rest (elementToImage a image)  
//    | [] -> image 

//let rec _rotate list accum = 
//    match list with 
//    | a :: rest -> _rotate rest (rowToImage list accum)  
//    | [] -> accum

let rec createRow image height width accum = 
    if height < 0 then 
        List.rev accum 
    else
        createRow image (height-1) width ((List.item(width) (List.item(height) image)):: accum)

let rec _rotate image height width accum = 
    if width < 0 then 
        accum 
    else 
        _rotate image height (width - 1) ((createRow image height width []) ::  accum) 

let rec RotateRight90(width:int, height:int, depth:int, image:(int*int*int) list list) = 
  let L = _rotate image (height-1) (width-1) []
  L
