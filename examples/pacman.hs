
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Graphics.Web.Processing.Mid
import Graphics.Web.Processing.Mid.CustomVar
import Graphics.Web.Processing.Html
import Control.Applicative
import GHC.Generics (Generic)

-- | Tile types:
--
-- * 0 - Wall
-- * 1 - Dot
-- * 2 - Energizer
-- * 3 - Empty
-- * 4 - Pac-Man only wall
--
type Tile = Proc_Int

isWall :: Tile -> Proc_Bool
isWall t = t #== 0 #|| t #== 4

type Map = [Tile]

intFromChar :: Char -> Int
intFromChar c = read [c]

main :: IO ()
main = do
  map <- (fmap (fmap intFromChar) . lines) <$> readFile "pacman.map"
  let rows = length map
      cols = length $ head map
      map' = concat map
      dots = length $ filter (==1) map'
  writeHtml "processing.js" "pacman.pde" "Pac-Man!" "pacman.html" $ pacmanScript rows cols dots map'

------------------------------
-- CONFIGURATIONS

framesPerSecond :: Num a => a
framesPerSecond = 35

cellWidth :: Proc_Float
cellWidth = 17

cycleLength :: Num a => a
cycleLength = 6

maxFrame :: Proc_Int
maxFrame = 5

scatterTime :: Proc_Int
scatterTime = 5 * framesPerSecond

chaseTime :: Proc_Int
chaseTime = 20 * framesPerSecond

------------------------------

-- (#==) should be generalized to include this case and more.
(#===) :: (Proc_Eq a, Proc_Eq b) => (a,b) -> (a,b) -> Proc_Bool
(a,b) #=== (c,d) = a #== c #&& b #== d

(<+>) :: Num a => (a,a) -> (a,a) -> (a,a)
(a,b) <+> (c,d) = (a+c,b+d)

distance :: Proc_Point -> Proc_Point -> Proc_Float
distance (a,b) (c,d) = sqrt $ (a-c)^2 + (b-d)^2

colorize :: Color -> EventM Draw ()
colorize c = stroke c >> fill c

-- Directions
type Direction = Proc_Float

up,down,left,right :: Direction
up = 3*pi/2
down = pi/2
left = pi
right = 0

frontDirs :: Direction -> [Direction]
frontDirs d = [d, d + down, d + up]

dirVector :: Direction -> Proc_Point
dirVector d = (cos d, sin d)

type Pos = (Proc_Int,Proc_Int)

dirPos :: Direction -> Pos
dirPos d = (pround $ sin d, pround $ cos d)

data State = State {
    -- Pacman
    pacmanPos :: Pos
  , pacmanDir :: Direction
  , pacmanWillDir :: Direction
  , pacmanTarget :: Pos
    -- Ghosts
  , blinkyPos :: Pos
  , blinkyDir :: Direction
  , blinkyTarget :: Pos
  , pinkyPos  :: Pos
  , pinkyDir  :: Direction
  , pinkyTarget :: Pos
  , inkyPos   :: Pos
  , inkyDir   :: Direction
  , inkyTarget :: Pos
  , clydePos  :: Pos
  , clydeDir  :: Direction
  , clydeTarget :: Pos
    -- Move cycle
  , moveCycle :: Proc_Int
    -- Mode
  , chaseMode :: Proc_Bool
  , modeTimer :: Proc_Int
    -- Game State
  , isWon :: Proc_Bool
  , isLost :: Proc_Bool
  } deriving Generic

instance VarLength State
instance CustomValue State

initialState :: State
initialState = State {
    pacmanPos = (23,14)
  , pacmanTarget = (23,15)
  , pacmanDir = right
  , pacmanWillDir = right
  , blinkyPos = (15,12)
  , blinkyDir = up
  , blinkyTarget = blinkyPos initialState <+> dirPos (blinkyDir initialState)
  , pinkyPos  = (15,13)
  , pinkyDir  = left
  , pinkyTarget = pinkyPos initialState <+> dirPos (pinkyDir initialState)
  , inkyPos   = (15,14)
  , inkyDir   = right
  , inkyTarget = inkyPos initialState <+> dirPos (inkyDir initialState)
  , clydePos  = (15,15)
  , clydeDir  = up
  , clydeTarget = clydePos initialState <+> dirPos (clydeDir initialState)
  , moveCycle = 0
  , chaseMode = false -- start in scatter mode
  , modeTimer = scatterTime
  , isWon = false
  , isLost = false
    }

data Ghost = Blinky | Pinky | Inky | Clyde
             deriving Eq

allGhosts :: [Ghost]
allGhosts = [Blinky, Pinky, Inky, Clyde]

ghostColor :: Ghost -> Color
ghostColor Blinky = Color 255 0 0 255
ghostColor Pinky  = Color 255 184 255 255
ghostColor Inky   = Color 0 255 255 255
ghostColor Clyde  = Color 255 184 81 255

ghostPos :: Ghost -> State -> Pos
ghostPos Blinky = blinkyPos
ghostPos Pinky  = pinkyPos
ghostPos Inky   = inkyPos
ghostPos Clyde  = clydePos

ghostDir :: Ghost -> State -> Direction
ghostDir Blinky = blinkyDir
ghostDir Pinky  = pinkyDir
ghostDir Inky   = inkyDir
ghostDir Clyde  = clydeDir

ghostTarget :: Ghost -> State -> Pos
ghostTarget Blinky = blinkyTarget
ghostTarget Pinky  = pinkyTarget
ghostTarget Inky   = inkyTarget
ghostTarget Clyde  = clydeTarget

ghostHome :: Ghost -> Pos
ghostHome Blinky = (0,27)
ghostHome Pinky  = (0,0)
ghostHome Inky   = (30,27)
ghostHome Clyde  = (30,-10)

cancelPoint :: Num a => (a,a) -> (a,a)
cancelPoint (x,y) = (-x,-y)

scalePoint :: Num a => a -> (a,a) -> (a,a)
scalePoint k (x,y) = (k*x,k*y)

middlePoint :: Proc_Float -- t <- 0~1
            -> Proc_Point -- p <- R2
            -> Proc_Point -- q <- R2
            -> Proc_Point -- (1-t)p + tq
middlePoint t p q = scalePoint (1-t) p <+> scalePoint t q

drawGhost :: Color -> Proc_Point -> Direction -> EventM Draw ()
drawGhost c p d = do
  colorize c
  let corner = p <+> ((-cellWidth)/2,cellWidth/2)
  bezier corner
         (corner <+> (0,-cellWidth))
         (corner <+> (cellWidth,-cellWidth))
         (corner <+> (cellWidth,0))
  let eye1 = p <+> ((-cellWidth)/4,(-cellWidth)/6)
      eye2 = p <+> (cellWidth/4,(-cellWidth)/6)
      eyer = cellWidth/6
  -- Eye 1
  colorize $ Color 255 255 255 255
  uncurry translate eye1
  circle (0,0) eyer
  colorize $ Color 0 0 0 255
  circle (scalePoint (eyer/2) $ dirVector d) $ eyer/2
  uncurry translate $ cancelPoint eye1
  -- Eye 2
  colorize $ Color 255 255 255 255
  uncurry translate eye2
  circle (0,0) eyer
  colorize $ Color 0 0 0 255
  circle (scalePoint (eyer/2) $ dirVector d) $ eyer/2
  uncurry translate $ cancelPoint eye2

drawPacman :: Proc_Point -> Direction -> Proc_Int -> EventM Draw ()
drawPacman p d fr = do
  colorize $ Color 255 255 0 255
  let initAngle = (pi*intToFloat fr)/(4*intToFloat maxFrame)
      endAngle  = 2*pi - initAngle
  arc p cellWidth cellWidth (d+initAngle) (d+endAngle)

cornerOf :: Proc_Int -> Proc_Int -> Proc_Point
cornerOf i j = (intToFloat j * cellWidth, intToFloat i * cellWidth)

centerOf :: Proc_Int -> Proc_Int -> Proc_Point
centerOf i j = cornerOf i j <+> (cellWidth/2, cellWidth/2)

pacmanScript :: Int -- Number of rows
             -> Int -- Number of columns
             -> Int -- Number of dots
             -> [Int] -- Map
             -> ProcScript
pacmanScript rows cols dots map = execScriptM $ do
  mapv <- newArrayVar $ fmap fromInt map
  dotv <- newVar $ fromInt dots -- Remaining dots
  stv  <- newVarC initialState
  framev <- newVar 1 -- Pacman frame
  mouthv <- newVar 1 -- Mouth switcher
  boolv <- newVar false -- Boolean aux var
  let tileIndex :: Num a => a -> a -> a
      tileIndex i j = i * fromIntegral cols + j
      getTile :: Proc_Int -> Proc_Int -> EventM c Tile
      getTile i j = readArrayVar mapv $ tileIndex i j
  on Setup $ do
     setFrameRate framesPerSecond
  on Draw $ do
     -- Size
     size screenWidth screenHeight
     -- Background
     background $ Color 0 0 0 255
     -- Set center at the center of the screen
     translate (intToFloat screenWidth/2) (intToFloat screenHeight/2)
     -- Read state
     st <- readVarC stv
     -- Set bottom-left corner as the origin of coordinates
     let (cornerX,cornerY) = ((-cellWidth) *  intToFloat (fromInt rows)/2
                             ,(-cellWidth) * (intToFloat (fromInt cols)/2))
     translate cornerX cornerY
     -- Draw map
     let drawAt :: Proc_Int -- Row number
                -> Proc_Int -- Col number
                -> (Proc_Point -> EventM Draw ()) -- Drawing function
                -> EventM Draw ()
         drawAt i j f = f $ cornerOf i j
         tilesOf :: Int -> [(Proc_Int,Proc_Int)]
         tilesOf t = [ (fromInt i,fromInt j)
                     | i <- [ 0 .. rows - 1 ] , j <- [ 0 .. cols - 1 ]
                     , map !! tileIndex i j == t ]
         twalls = tilesOf 0
         tdots  = tilesOf 1
         tenergizers = tilesOf 2
         tgwalls = tilesOf 4
     mapM_ (\(i,j) -> drawAt i j drawWall) twalls -- Walls
     mapM_ (\(i,j) -> drawAt i j drawGWall) tgwalls -- Ghost walls
     mapM_ (\(i,j) -> do t <- getTile i j
                         when (t #== 1) $ drawAt i j drawDot) tdots -- Dots
     mapM_ (\(i,j) -> do t <- getTile i j
                         when (t #== 2) $ drawAt i j drawEnergizer) tenergizers -- Energizers
     -- Remove ghost walls after the first scatter time
     when (chaseMode st) $ mapM_ (\(i,j) -> writeArrayVar mapv (tileIndex i j) 0) tgwalls
     -- Cycle scalar
     let cycleScalar = intToFloat (moveCycle st) / cycleLength
     -- Draw ghosts
     mapM_ (\g -> drawGhost (ghostColor g)
                            (middlePoint cycleScalar (uncurry centerOf $ ghostPos g st)
                                                     (uncurry centerOf $ ghostTarget g st))
                            (ghostDir g st)) allGhosts
     -- Read pacman frame
     fr <- readVar framev
     mouth <- readVar mouthv
     -- Draw pacman
     let pacmanR2 = middlePoint cycleScalar (uncurry centerOf $ pacmanPos st)
                                            (uncurry centerOf $ pacmanTarget st)
     drawPacman pacmanR2 (pacmanDir st) fr
     -- Update pacman frame
     ifM (fr #== maxFrame #|| fr #== 0)
         (do writeVar mouthv $ negate mouth
             writeVar framev $ fr + negate mouth)
         (writeVar framev $ fr + mouth)
     -- Pacman movement
     let (willI,willJ) = pacmanTarget st <+> dirPos (pacmanWillDir st)
     willwall <- fmap isWall $ getTile willI willJ
     let newDir = if_ (moveCycle st #== cycleLength #&& pnot willwall) (pacmanWillDir st) (pacmanDir st)
         (targetI,targetJ) = pacmanTarget st
     targetTile <- getTile targetI targetJ
     comment "Eating"
     when (moveCycle st #== cycleLength)
          (do -- Eat dot
              when (targetTile #== 1) $ do writeArrayVar mapv (tileIndex targetI targetJ) 3
                                           readVar dotv >>= writeVar dotv . (+ negate 1)
              -- Eat energizer
              when (targetTile #== 2) $ do writeArrayVar mapv (tileIndex targetI targetJ) 3
           )
     let (ntargetI,ntargetJ) = pacmanTarget st <+> dirPos newDir
     headedToWall <- fmap isWall $ getTile ntargetI ntargetJ
     -- Ghosts movement (blinky, pinky, inky, clyde)
     [   (bpos,bdir,btar),(ppos,pdir,ptar)
       , (ipos,idir,itar),(cpos,cdir,ctar)] <- mapM (
           \g -> moveGhost g st getTile
             ) allGhosts
     -- Check if game is lost
     let nextToGhost g =
           (pacmanPos st #=== ghostTarget g st #&& pacmanDir st #/= ghostDir g st) #||
           (pacmanPos st #=== ghostPos g st)
         lose = foldr (#||) false $ fmap nextToGhost allGhosts
     -- End of the game titles
     resetMatrix
     translate (intToFloat screenWidth/2) (intToFloat screenHeight/2)
     scale 5 5
     fill $ Color 255 255 255 255
     -- Lost title
     when (isLost st) $ drawtext "You have been eaten!" (-65,-5) 500 500
     -- Won title
     when (isWon st)  $ drawtext "You win!" (-28,-5) 500 500
     -- Remaining dots
     remdots <- readVar dotv
     -- Update state
     comment "Update state"
     when (pnot $ isWon st #|| isLost st) $ writeVarC stv $ st {
         pacmanPos = ifC (moveCycle st #== cycleLength)
                         (pacmanTarget st)
                         (pacmanPos st)
       , pacmanDir = newDir
       , pacmanTarget = ifC (moveCycle st #== cycleLength #&& pnot headedToWall)
                            (ntargetI,ntargetJ)
                            (pacmanTarget st)
       , moveCycle = if_ (moveCycle st #== cycleLength) 0 (moveCycle st + 1)
       , blinkyPos = bpos
       , blinkyDir = bdir
       , blinkyTarget = btar
       , pinkyPos = ppos
       , pinkyDir = pdir
       , pinkyTarget = ptar
       , inkyPos = ipos
       , inkyDir = idir
       , inkyTarget = itar
       , clydePos = cpos
       , clydeDir = cdir
       , clydeTarget = ctar
       , chaseMode = if_ (moveCycle st #== cycleLength #&& targetTile #== 2)
                          false
                         (if_ (modeTimer st #== 0) (pnot $ chaseMode st) (chaseMode st))
       , modeTimer = if_ (moveCycle st #== cycleLength #&& targetTile #== 2)
                         (2*scatterTime)
                         (if_ (modeTimer st #== 0)
                              (if_ (chaseMode st) scatterTime chaseTime)
                              (modeTimer st - 1))
       , isLost = lose
       , isWon = remdots #== 0
         }
  on KeyPressed $ do
     st <- readVarC stv
     -- Change will dir
     matchKey boolv $ ArrowKey UP
     bUP <- readVar boolv
     matchKey boolv $ ArrowKey DOWN
     bDOWN <- readVar boolv
     matchKey boolv $ ArrowKey LEFT
     bLEFT <- readVar boolv
     matchKey boolv $ ArrowKey RIGHT
     bRIGHT <- readVar boolv
     let will = if_ bUP    up
              $ if_ bDOWN  down
              $ if_ bLEFT  left
              $ if_ bRIGHT right
              $ pacmanWillDir st
     writeVarC stv $ st { pacmanWillDir = will }

wallColor :: Color
wallColor = Color 0 0 255 255

dotColor :: Color
dotColor = Color 240 240 255 255

energizerColor :: Color
energizerColor = Color 255 255 0 255

ghostWallColor :: Color
ghostWallColor = Color 255 192 203 255

when :: Proc_Bool -> EventM c () -> EventM c ()
when b x = ifM b x $ return ()

drawWall :: Proc_Point -> EventM Draw ()
drawWall p = colorize wallColor >> rect p cellWidth cellWidth

drawGWall :: Proc_Point -> EventM Draw ()
drawGWall p = colorize ghostWallColor >> rect p cellWidth cellWidth

drawDot :: Proc_Point -> EventM Draw ()
drawDot p = colorize dotColor >> circle (p <+> (cellWidth/2,cellWidth/2)) (cellWidth/10)

drawEnergizer :: Proc_Point -> EventM Draw ()
drawEnergizer p = colorize energizerColor >> circle (p <+> (cellWidth/2,cellWidth/2)) (cellWidth/5)

-- GHOST BEHAVIOR (Blinky, Pinky, Inky and Clyde)

ghostWill :: Ghost -> State -> Pos
ghostWill Blinky st = pacmanPos st
ghostWill Pinky st = pacmanPos st <+> scalePoint 4 (dirPos $ pacmanDir st)
ghostWill Inky st =
  let pac2 = pacmanPos st <+> scalePoint 2 (dirPos $ pacmanDir st)
  in  blinkyPos st <+> scalePoint 2 (pac2 <+> cancelPoint (blinkyPos st))
ghostWill Clyde st =
  let d = distance (uncurry centerOf $ clydePos st) (uncurry centerOf $ pacmanPos st)
  in  ifC (d #> 8*cellWidth) (pacmanPos st) (ghostHome Clyde)

closestTo :: Pos -- Current position
          -> Pos -- Target position
          -> [(Direction,Tile)] -- List of directions to take, and what is in each one
          -> (Direction,Tile)
closestTo _ _ [(d,t)] = (d,t)
closestTo p0 pT ((d,t):xs) = ifC (t' #== 0) (d,t) $
                               ifC (t  #== 0) (d',t') $
                                 ifC (d1 #<= d2) (d,t) (d',t')
  where
   (d',t') = closestTo p0 pT xs
   d1 = distance (uncurry centerOf $ p0 <+> dirPos d ) $ uncurry centerOf pT
   d2 = distance (uncurry centerOf $ p0 <+> dirPos d') $ uncurry centerOf pT

chooseDir :: Ghost -> State -> [(Direction,Tile)] -> Direction
chooseDir g st xs = fst $ closestTo (ghostPos g st) willPos xs
 where
  willPos :: Pos
  willPos = ifC (chaseMode st) (ghostWill g st) (ghostHome g)

moveGhost :: Ghost -> State
          -> (Proc_Int -> Proc_Int -> EventM Draw Tile)
          -> EventM Draw (Pos,Direction,Pos)
moveGhost g st getTile = do
  let newPos = ifC (moveCycle st #== cycleLength)
                   (ghostTarget g st)
                   (ghostPos g st)
  -- Direction
  let fronts = frontDirs $ ghostDir g st
  frontTiles <- mapM (uncurry getTile) $ fmap (\d -> ghostTarget g st <+> dirPos d) fronts
  let newDir = if_ (moveCycle st #== cycleLength)
                   (chooseDir g st $ zip fronts frontTiles)
                   (ghostDir g st)
  -- Target
  let newTarget = ifC (moveCycle st #== cycleLength)
                      (ghostTarget g st <+> dirPos newDir)
                      (ghostTarget g st)
  -- Return
  return (newPos,newDir,newTarget)
