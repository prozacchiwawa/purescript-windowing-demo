module UseWindow where

import Data.Tuple (Tuple(..))
import Window as W

tryWindowThingy _ =
    let 
        ws = W.rootWindow 1000.0 1000.0
        cwResult = W.createWindow (W.rootId ws) ws (W.Bounds { top: 30.0, left: 30.0, width: 200.0, height: 500.0, title: "Hi there" })
        ws1 =
            case cwResult of
                Tuple wid1 ws1 -> ws1
        wid1 = 
            case cwResult of
                Tuple wid1 ws1 -> wid1
        ws2 = W.update (W.MouseDown (W.Pt { x: 31.0, y: 51.0 })) ws1
        cwResult2 = W.createWindow wid1 ws2 (W.Bounds { top: 100.0, left: 150.0, width: 50.0, height: 50.0, title: "Going away" })
        ws3 = case cwResult2 of
                Tuple wid2 ws2 -> ws2
        wid2 = case cwResult2 of
                Tuple wid2 ws2 -> wid2
        ws4 = W.update (W.MouseMove (W.Pt { x: 33.0, y: 51.0 })) ws3
        ws5 = W.destroyWindow wid2 ws4
    in
    W.update (W.MouseUp) ws5
            
    
