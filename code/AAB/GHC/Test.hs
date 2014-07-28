module AAB.GHC.Test where

import AAB.Game.Types (GameMap (..), Element (..))
import AAB.Game.Test
import AAB.GHC.Types (GhostWorldView (..))

import Data.Array

testWorld :: GhostWorldView
testWorld = mapToWorld testMap

mapToWorld :: GameMap -> GhostWorldView
mapToWorld (GM mp) = WS {
    lambdaManXY = (snd . head) (mapSearch LambdaManStart mp),
    myIndex = 0,
    ghostStartCoordinates = ghostCoords,
    ghostCurCoordinates   = ghostCoords,
    ghostVitDir           = listArray ghostRange (map (const (0, 2)) ghosts),
    gameMap               = array mapRange
        [(r, fromElement
             ((mp !! (fromIntegral $ snd r)) !! (fromIntegral $ fst r))) |
             r <- range mapRange]
}
    where ghosts      = mapSearch GhostStart mp
          ghostRange  = (0, fromIntegral $ (length ghosts - 1))
          ghostCoords = listArray ghostRange (map snd ghosts)
          width       = fromIntegral $ length (head mp)
          height      = fromIntegral $ length mp
          mapRange    = ((0, 0), (width - 1, height - 1))
