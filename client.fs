open System
open System.Windows.Forms
open System.IO
open System.Drawing
open System.Drawing.Drawing2D
open System.Drawing.Imaging
open System.ComponentModel

open FSharp.Collections.ParallelSeq

open Worker
open NativeImageHandler

#nowarn "40"

//type DoubleBufferedForm() = 
//    inherit Form()
//    do base.DoubleBuffered<-true

let main() =      
    /// Adjust to a higher value if gui is unable to draw. Symptoms are choppy updates, black bitmaps, program not responding/hangs.
    let guiUpdateWaitTimeInMS = 10
    
    /// magnify world to N pixels per world point
    let magnification = 7
    
    /// GUI paintbox dimensions (per world sim)
    let pbWidth = Sim.territoryWidth * magnification
    let pbHeight = Sim.territoryHeight * magnification
        
    let pbOffset = 5
            
    let windowWidth = pbWidth + pbOffset
    let windowHeight = pbHeight + pbOffset + 45 // extra height 45 = hard coded estimate of non viewport client elements. better to get from win api probably
    
    let form = new Form(Visible = true, Menu = new MainMenu(), Text = "Ant Colony", Width = windowWidth, Height = windowHeight)
//    let form = new DoubleBufferedForm(Width = windowWidth,
//                                      Height = windowHeight,
//                                      Text = "Ant Colony", 
//                                      Menu = new MainMenu())    
//    form.Visible <- true
    
    
    // Form controls

    let bitmap = new Bitmap(Sim.territoryWidth, Sim.territoryHeight, System.Drawing.Imaging.PixelFormat.Format24bppRgb)
    let pb = new PictureBox(SizeMode=PictureBoxSizeMode.Zoom)
    pb.Size <- new Size(pbWidth, pbHeight)
    pb.Location <- new Point(0, 0)
    pb.Image <- bitmap
    form.Controls.Add( pb )
    
    let byteArray = createBitmapArray Sim.territoryWidth Sim.territoryHeight

    /// setPixel (x,y) value to (A,R,G,B)
    let setPixelArgb (x, y) (A, R, G, B) =
        setBitmapArrayArgb byteArray Sim.territoryWidth (x,y) (byte A) (byte R) (byte G) (byte B)

    // Worker
    let worker = new Worker(guiUpdateWaitTimeInMS)

    worker.Updates.Add(fun updates ->
        
        for  (env, ants, envFood, envPh) in updates do

            // paint pheromones (and implicitly the black background since envPh is a sequence of all world locations and their ph values)
            let maxPhTry = snd (Seq.maxBy ( fun ((x,y), pheromone) -> pheromone) envPh)
            let maxPh = if maxPhTry = 0 then 1 else maxPhTry
                                                    
            envPh |> PSeq.iter (fun ((x,y), pheromone) ->
                let ib = pheromone * 255 / maxPh
                let b = if (ib < 60 && ib > 0) then 60 else ib
                setPixelArgb (x,y) (255, 0, b/2, b)                
                )

            // paint ants
            let RIntensity foodCarried = if foodCarried = 0 then 128 else 255
            let BIntensity foodCarried = if foodCarried = 0 then 0 else 128

            ants |> PSeq.iter (fun (x,y,f) -> 
                setPixelArgb (x,y) (255, RIntensity f, 0, BIntensity f)
                )

            // paint food
            envFood |> PSeq.iter (fun ((x,y), food, isHome) ->
                let ig = food * 255 / Sim.maxFoodPerSquare
                let g = if ig < 60 then 60 else ig                
                setPixelArgb (x,y) (255, 0, g, 0)
                )

            pb.Image <- arrayToBitmap byteArray bitmap
            //pb.Invalidate()
                        
            if env.simStep % 10 = 0 then
                form.Text <- "simulation step " + env.simStep.ToString()
        )
    
         
    // MENU

    let fileMenu = form.Menu.MenuItems.Add("&File")
    let rec runMenuItem : MenuItem = 
        new MenuItem("&Continue Run", 
                     (fun _ _ -> 
                       runMenuItem.Enabled <- false
                       stopMenuItem.Enabled <- true
                       //inputMenuItem.Enabled <- true
                       stepMenuItem.Enabled <- false
                       worker.RunAsync() ),
                     Shortcut.CtrlC)

    and stopMenuItem : MenuItem = 
        new MenuItem("&Pause", 
                     (fun _ _ -> 
                       runMenuItem.Enabled <- true
                       stopMenuItem.Enabled <- false
                       stepMenuItem.Enabled <- true
                       worker.StopAsync()),
                     Shortcut.CtrlP)
          
          
    and stepMenuItem : MenuItem = 
        new MenuItem("&Step", 
                     (fun _ _ -> worker.StepAsync()),
                     Shortcut.CtrlS)

    and resetMenuItem : MenuItem = 
        new MenuItem("&Reset", 
                     (fun _ _ -> 
                       runMenuItem.Enabled <- false
                       stopMenuItem.Enabled <- true
                       //inputMenuItem.Enabled <- true
                       stepMenuItem.Enabled <- false
                       worker.ResetAsync() ),
                     Shortcut.CtrlR)
          
//    and inputMenuItem : MenuItem = 
//        new MenuItem("User &Input", 
//                     (fun _ _ -> inputMenuItem.Checked <- not inputMenuItem.Checked),
//                     Shortcut.CtrlI)
          
    and closeMenuItem : MenuItem = 
        new MenuItem("Close", 
                     (fun _ _ -> form.Close()), 
                     Shortcut.CtrlX)

    fileMenu.MenuItems.Add(runMenuItem)   |> ignore
    fileMenu.MenuItems.Add(resetMenuItem) |> ignore
    fileMenu.MenuItems.Add(stepMenuItem)  |> ignore
    fileMenu.MenuItems.Add(stopMenuItem)  |> ignore
    //fileMenu.MenuItems.Add(inputMenuItem) |> ignore
    fileMenu.MenuItems.Add("-") |> ignore
    fileMenu.MenuItems.Add(closeMenuItem) |> ignore 
       
    //inputMenuItem.Enabled <- true
    runMenuItem.Enabled <- true
    stopMenuItem.Enabled <- true
    stepMenuItem.Enabled <- false
    



    worker.Start()
     
    form.Activate()     
    Application.Run(form)
    
[<STAThread>]
do main()