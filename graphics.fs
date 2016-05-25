module Graphics
  
/// Get a line given 2 points using the Bresenham's algorithm.
/// From http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#F.23
let private bresenhamLine (x0, y0) (x1, y1) =
  let steep = abs(y1 - y0) > abs(x1 - x0)
  let x0, y0, x1, y1 =
    if steep then y0, x0, y1, x1 else x0, y0, x1, y1
  let x0, y0, x1, y1 =
    if x0 > x1 then x1, y1, x0, y0 else x0, y0, x1, y1
  let dx, dy = x1 - x0, abs(y1 - y0)
  let s = if y0 < y1 then 1 else -1
  
  let rec loop e x y (line: (int*int) list) =
    if x <= x1 then
      let nLine = 
          if steep then 
            List.append line [(y, x)]
          else 
            List.append line [(x, y)]
      if e < dy then
        loop (e-dy+dx) (x+1) (y+s) nLine
      else
        loop (e-dy) (x+1) y nLine
    else
      line 
  loop (dx/2) x0 y0 []
  
let vectorToLine (x0, y0) (x1, y1) =
    let line = bresenhamLine (x0, y0) (x1, y1)
    let firstPt = List.head line
    let firstPtX = fst firstPt
    let firstPtY = snd firstPt
    if abs(firstPtX - x0) <= 1 && abs(firstPtY - y0) <= 1 then
        line
    else
        List.rev line
