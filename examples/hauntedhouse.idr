module Main

import Js.Dom
import public Js.ASync
import Control.ST
import Js.HtmlStyle
import Data.AVL.Set
import Data.AVL.Dict

data Input
  = Tick
  | KeyDown String
  | KeyUp String

data HeroState
  = BeforeStart
  | Playing
  | GoingIntoStairs Int Int (Double,Double)
  | ComingOutOfStairs Int (Double,Double)

record Hero where
  constructor MkHero
  hero_state : HeroState
  hero_x : Double
  hero_y : Double
  hero_match : Bool

frameSpd : Double
frameSpd = 30.0

goingIntoStairsFrames : Int
goingIntoStairsFrames = 20

comingOutOfStairs : Int
comingOutOfStairs = 30

prngPrime : Int
prngPrime = 96443

prngTargetDigits : Int
prngTargetDigits = 5

prngMod : Int
prngMod = 100000

digDivisor : Int -> Int
digDivisor 0 = 1
digDivisor n =
  10 * (digDivisor (n - 1))

ilen : String -> Int
ilen s =
  cast (length s)

prng : Int -> Int
prng n =
  let
    mul = n * prngPrime
    digits = ilen (show mul)
    extraDigits = max 0 (digits - prngTargetDigits)
    padDigits = div extraDigits 2
  in
  mod (div mul (digDivisor padDigits)) prngMod

levelSeed : Int
levelSeed = 65419

xmoveDistPerFrame : Double
xmoveDistPerFrame = 4.0

ymoveDistPerFrame : Double
ymoveDistPerFrame = 4.0

roomXGrid : Double
roomXGrid = 320

roomYGrid : Double
roomYGrid = 320

camHeroDist : Double
camHeroDist = roomYGrid * 0.75

camMaxY : Double
camMaxY = roomYGrid * 1.5

heroPupilSize : Double
heroPupilSize = 3.0

heroPupilMove : Double
heroPupilMove = 3.0

heroEyeSize : Double
heroEyeSize = 7.0

heroEyeDist : Double
heroEyeDist = 7.0

flame1Size : Double
flame1Size = roomXGrid / 4.0

flame2Size : Double
flame2Size = (roomXGrid / 4.0) + 10.0

wallScaleX : Double
wallScaleX = roomXGrid / 8.0

wallScaleY : Double
wallScaleY = roomYGrid / 8.0

wallColor : Int -> String
wallColor floor =
  case floor of
    0 => "#0000CD"
    1 => "#800000"
    2 => "#008000"
    _ => "#FF4500"

heroCollWidth : Double
heroCollWidth = 21.0

heroCollHeight : Double
heroCollHeight = 14.0

record Wall where
  constructor MkWall
  x : Double
  y : Double
  width : Double
  height : Double

record SpriteBox where
  constructor MkSpriteBox
  sp_x : Double
  sp_y : Double
  sp_width : Double
  sp_height : Double

interface SpriteBoxOf t where
  spriteBox : t -> SpriteBox

implementation SpriteBoxOf Hero where
  spriteBox h =
    let
      hx = hero_x h
      hy = hero_y h
    in
    MkSpriteBox hx hy heroCollWidth heroCollHeight

record IntAndWall where
  constructor MkIntAndWall
  iw_floor : Int
  iw_wall : Wall

data StairsPosition
  = LeftStairs
  | RightStairs
  | BottomLeftStairs
  | TopLeftStairs
  | BottomRightStairs
  | TopRightStairs

record Stairs where
  constructor MkStairs
  st_floor : Int
  st_up : Bool
  st_pos : StairsPosition

record Model where
  constructor MkModel
  frameno : Int
  keymap : Set String
  hero : Hero
  floor : Int
  cam_y : Double
  matches : Int
  space_latch : Bool
  walls : List Wall
  stairs : List Stairs
  debug : String

wallCoords : Double -> Double -> Double -> Double -> Wall
wallCoords x1 y1 x2 y2 =
  let
    fx = min x1 x2
    fy = min y1 y2
    lx = max x1 x2
    ly = max y1 y2
  in
  MkWall fx fy (lx - fx) (ly - fy)

sixRoomLayoutWalls : List Wall
sixRoomLayoutWalls =
  {- Bottom row walls -}
  (horizontalWallSet 0.0) ++
  [ {- Side walls of third row -}
    wallCoords 0.0 bottomRoomWallYTop wallScaleX midRoomDoorBottom
  , wallCoords ((2.0 * roomXGrid) - wallScaleX) bottomRoomWallYTop (2.0 * roomXGrid) midRoomDoorBottom
    {- Side walls of first row -}
  , wallCoords 0.0 wallScaleY wallScaleX midRoomDoorTop
  , wallCoords ((2.0 * roomXGrid) - wallScaleX) wallScaleY (2.0 * roomXGrid) midRoomDoorTop
    {- Middle veritcal walls -}
  , wallCoords centerWallLeftSide 0.0 centerWallRightSide row1DoorTop
  , wallCoords centerWallLeftSide row1DoorBottom centerWallRightSide midRoomDoorTop
  , wallCoords centerWallLeftSide midRoomDoorBottom centerWallRightSide row3DoorTop
  , wallCoords centerWallLeftSide row3DoorBottom centerWallRightSide (3.0 * roomYGrid)
  ]
    {- Top row walls -}
    ++ (horizontalWallSet ((3.0 * roomYGrid) - wallScaleY))
    ++ (horizontalWallSet (roomYGrid - (wallScaleY / 2.0)))
    ++ (horizontalWallSet ((2.0 * roomYGrid) - (wallScaleY / 2.0)))
  where
    leftRoomDoorwayLeftSide : Double
    leftRoomDoorwayLeftSide = (roomXGrid - wallScaleX) / 2.0

    leftRoomDoorwayRightSide : Double
    leftRoomDoorwayRightSide = leftRoomDoorwayLeftSide + wallScaleX

    rightRoomDoorwayLeftSide : Double
    rightRoomDoorwayLeftSide = leftRoomDoorwayLeftSide + roomXGrid

    rightRoomDoorwayRightSide : Double
    rightRoomDoorwayRightSide = leftRoomDoorwayRightSide + roomXGrid

    bottomRoomWallYTop : Double
    bottomRoomWallYTop = ((3.0 * roomYGrid) - wallScaleY)

    midRoomDoorBottom : Double
    midRoomDoorBottom = ((roomYGrid * 3.0) + wallScaleY) / 2.0

    midRoomDoorTop : Double
    midRoomDoorTop = midRoomDoorBottom - wallScaleY

    row1DoorTop : Double
    row1DoorTop = midRoomDoorTop - roomYGrid

    row1DoorBottom : Double
    row1DoorBottom = row1DoorTop + wallScaleY

    row3DoorTop : Double
    row3DoorTop = row1DoorTop + (roomYGrid * 2.0)

    row3DoorBottom : Double
    row3DoorBottom = row1DoorBottom + (roomYGrid * 2.0)

    centerWallLeftSide : Double
    centerWallLeftSide = roomXGrid - (wallScaleX / 2.0)

    centerWallRightSide : Double
    centerWallRightSide = centerWallLeftSide + wallScaleX

    horizontalWallSet : Double -> List Wall
    horizontalWallSet ytop =
      [ wallCoords 0.0 ytop leftRoomDoorwayLeftSide (ytop + wallScaleY)
      , wallCoords leftRoomDoorwayRightSide ytop rightRoomDoorwayLeftSide (ytop + wallScaleY)
      , wallCoords rightRoomDoorwayRightSide ytop (2.0 * roomXGrid) (ytop + wallScaleY)
      ]

stairsOfBuilding : List Stairs
stairsOfBuilding =
  [ {- Left stairs up, floor 0 -}
    MkStairs 0 True LeftStairs
  , MkStairs 1 False LeftStairs
  ]
  where
    midRoomDoorBottom : Double
    midRoomDoorBottom = ((roomYGrid * 3.0) + wallScaleY) / 2.0

    midRoomDoorTop : Double
    midRoomDoorTop = midRoomDoorBottom - wallScaleY

stairsBox : Int -> Stairs -> List SpriteBox
stairsBox floor (MkStairs fl up pos) =
  let
    leftXCenter = wallScaleX / 2.0
    rightXCenter = (2.0 * roomXGrid) - (wallScaleX / 2.0)
    lEdgeXCenter = 0.5 * roomXGrid
    rEdgeXCenter = 1.5 * roomXGrid
  in
  if floor == fl then
    case pos of
      LeftStairs =>
        [MkSpriteBox leftXCenter (roomYGrid * 1.5) wallScaleX wallScaleY]
      RightStairs =>
        [MkSpriteBox rightXCenter (roomYGrid * 1.5) wallScaleX wallScaleY]
      BottomLeftStairs =>
        [MkSpriteBox lEdgeXCenter ((3.0 * roomYGrid) - (wallScaleY / 2.0)) wallScaleX wallScaleY]
      BottomRightStairs =>
        [MkSpriteBox rEdgeXCenter ((3.0 * roomYGrid) - (wallScaleY / 2.0)) wallScaleX wallScaleY]
      TopLeftStairs =>
        [MkSpriteBox lEdgeXCenter (wallScaleY / 2.0) wallScaleX wallScaleY]
      TopRightStairs =>
        [MkSpriteBox rEdgeXCenter (wallScaleY / 2.0) wallScaleX wallScaleY]
  else
    []

stairsDirection : StairsPosition -> (Double,Double)
stairsDirection LeftStairs = (xmoveDistPerFrame * (-1.0), 0)
stairsDirection RightStairs = (xmoveDistPerFrame, 0)
stairsDirection TopLeftStairs = (0, ymoveDistPerFrame * (-1.0))
stairsDirection TopRightStairs = (0, ymoveDistPerFrame * (-1.0))
stairsDirection BottomLeftStairs = (0, ymoveDistPerFrame)
stairsDirection BottomRightStairs = (0, ymoveDistPerFrame)

startHero : Hero
startHero = MkHero BeforeStart 0 0 False

spriteBoxOfStartHero : SpriteBox
spriteBoxOfStartHero = spriteBox startHero

spriteBoxOfHeroIsWhatWeThinkItShouldBe : Main.spriteBoxOfStartHero = MkSpriteBox 0.0 0.0 Main.heroCollWidth Main.heroCollHeight
spriteBoxOfHeroIsWhatWeThinkItShouldBe = Refl

emptyModel : Model
emptyModel =
  MkModel
    0
    Data.AVL.Set.empty
    startHero
    0
    0.0
    0
    False
    sixRoomLayoutWalls
    stairsOfBuilding
    ""

record Circle where
  constructor MkCircle
  center_x : Double
  center_y : Double
  radius : Double
  circ_color : String

record Box where
  constructor MkBox
  box_x : Double
  box_y : Double
  box_corner : Double
  box_width : Double
  box_height : Double
  box_color : String

p2p_dist_sq : (Double,Double) -> (Double,Double) -> Double
p2p_dist_sq (x1,y1) (x2,y2) =
  let
    dx = x2 - x1
    dy = y2 - y1
  in
  (dx * dx) + (dy * dy)

sumMoveResult : Set String -> List (String,Double) -> Double
sumMoveResult keymap =
  foldl
    (\sum,(k,v) =>
      if Data.AVL.Set.contains k keymap then
        sum + v
      else
        sum
    )
    0.0

heroCircles : Set String -> Int -> Double -> Hero -> List Circle
heroCircles km frameno cam_y hero =
  match ++ (concat eyes)
  where
    eyePoints : List Double
    eyePoints = [-1.0, 1.0]

    xpupil : Double
    xpupil = sumMoveResult km [("a", heroPupilMove * (-1.0)), ("d", heroPupilMove)]

    ypupil : Double
    ypupil = sumMoveResult km [("w", heroPupilMove * (-1.0)), ("s", heroPupilMove)]

    hx : Double
    hx = hero_x hero

    hy : Double
    hy = hero_y hero

    eye : Double -> List Circle
    eye e =
      [ MkCircle (hx + (e * heroEyeDist)) (hy - cam_y) heroEyeSize "white"
      , MkCircle (hx + (e * heroEyeDist) + xpupil) (hy - cam_y + ypupil) heroPupilSize "black"
      ]

    eyes : List (List Circle)
    eyes = map eye eyePoints

    flame_off_x_1 : Double
    flame_off_x_1 = cast ((mod (47 * frameno) 13) - 7)

    flame_off_y_1 : Double
    flame_off_y_1 = cast ((mod (93 * frameno) 11) - 6)

    flame_off_x_2 : Double
    flame_off_x_2 = cast ((mod (119 * frameno) 17) - 9)

    flame_off_y_2 : Double
    flame_off_y_2 = cast ((mod (117 * frameno) 13) - 6)

    match : List Circle
    match =
      if hero_match hero then
        [ MkCircle (hx + flame_off_x_2) ((hy - cam_y) + flame_off_y_2) flame2Size "#dbaf4a"
        , MkCircle (hx + flame_off_x_1) ((hy - cam_y) + flame_off_y_1) flame1Size "#fff491"
        ]
      else
        []

Gui : Dom m => Type
Gui {m} = DomRef {m} () (const Model) (const Input) ()

interface Drawable a where
  draw : a -> Html Input

implementation Drawable Circle where
  draw c =
    div
      [ style
          [ position (Absolute ((center_x c) - (radius c)) ((center_y c) - (radius c)))
          , width (2.0 * (radius c))
          , height (2.0 * (radius c))
          , borderRadius (radius c)
          , backgroundColor (circ_color c)
          ]
      ] []

implementation Drawable Box where
  draw b =
    div
      [ style
          [ position
              (Absolute
                ((box_x b) - ((box_width b) / 2.0))
                ((box_y b) - ((box_height b) / 2.0))
              )
          , width (box_width b)
          , height (box_height b)
          , borderRadius (box_corner b)
          , backgroundColor (box_color b)
          ]
      ] []

implementation Drawable IntAndWall where
  draw (MkIntAndWall floor (MkWall x y w h)) =
    let
      center_x = x + (w / 2.0)
      center_y = y + (h / 2.0)
    in
    draw (MkBox center_x center_y 0.0 w h (wallColor floor))

camOffset : Double -> Wall -> Wall
camOffset cy (MkWall x y w h) =
  MkWall x (y - cy) w h

drawables : Model -> List (Html Input)
drawables model =
  let
    h = hero model
    cy = cam_y model
    km = keymap model
    fn = frameno model
    fl = floor model
    hc = heroCircles km fn cy h
  in
  (map draw hc) ++
    (map draw (map (\w => MkIntAndWall fl (camOffset cy w)) (walls model)))

displays : Model -> List (Html Input)
displays model =
  [ div [] [text ("hx = " ++ (show (hero_x (hero model))))]
  , div [] [text ("hy = " ++ (show (hero_y (hero model))))]
  , div [] [text ("fn = " ++ (show (frameno model)))]
  , div [] [text ("km = " ++ (show (keymap model)))]
  , div [] [text ("db = " ++ (debug model))]
  ]

vw : () -> Model -> Html Input
vw () inp =
  let
    drawnDuringPlay =
      [ div
        [ style [ position (Absolute 0 0) ] ] (displays inp)
      ] ++ (drawables inp)

    body =
      case (hero_state (hero inp)) of
        BeforeStart =>
          [ div
            [ style [ position (Absolute 100 100) ] ]
            [ text "Press space to start" ]
          ]

        Playing => drawnDuringPlay
        GoingIntoStairs _ _ _ => drawnDuringPlay
        ComingOutOfStairs _ _ => drawnDuringPlay

  in
  div
    [ style
        [ position (Absolute 0 0)
        , width 640
        , height 480
        , color "white"
        , backgroundColor "black"
        , margin 0
        , padding 0
        , overflow Hidden
        ]
    , onkeydown (\k => KeyDown (key k))
    , onkeyup (\k => KeyUp (key k))
    , tabindex 0
    ] body

pointInSpriteBox : (Double,Double) -> SpriteBox -> Bool
pointInSpriteBox (x,y) (MkSpriteBox spx spy spw sph) =
  let
    sp_maxx = spx + (spw / 2.0)
    sp_minx = spx - (spw / 2.0)
    sp_maxy = spy + (sph / 2.0)
    sp_miny = spy - (sph / 2.0)
  in
  x >= sp_minx && x <= sp_maxx && y >= sp_miny && y <= sp_maxy

pointInSpriteBoxIsOkAtZeroZeroStartHeroBox : pointInSpriteBox (0.0,0.0) Main.spriteBoxOfStartHero = True
pointInSpriteBoxIsOkAtZeroZeroStartHeroBox = Refl

pointInSpriteBoxIsOkAtOtherPlaceStartHeroBox : pointInSpriteBox (5.0 * Main.roomXGrid, 5.0 * Main.roomYGrid) Main.spriteBoxOfStartHero = False
pointInSpriteBoxIsOkAtOtherPlaceStartHeroBox = Refl

spriteBoxOfWall : Wall -> SpriteBox
spriteBoxOfWall (MkWall x y w h) =
  MkSpriteBox (x + (w / 2.0)) (y + (h / 2.0)) w h

pointsOfSpriteBox : SpriteBox -> List (Double,Double)
pointsOfSpriteBox (MkSpriteBox spx spy spw sph) =
  let
    sp_maxx = spx + (spw / 2.0)
    sp_minx = spx - (spw / 2.0)
    sp_maxy = spy + (sph / 2.0)
    sp_miny = spy - (sph / 2.0)
  in
  [ (sp_minx, sp_miny)
  , (sp_minx, sp_maxy)
  , (sp_maxx, sp_miny)
  , (sp_maxx, sp_maxy)
  ]

spritesCollide : SpriteBox -> SpriteBox -> Bool
spritesCollide spa spb =
  let
    ptsa = pointsOfSpriteBox spa
    ptsb = pointsOfSpriteBox spb
  in
  pointsContained spa ptsb || pointsContained spb ptsa
  where
    pointsContained : SpriteBox -> List (Double,Double) -> Bool
    pointsContained sp =
      foldl
        (\coll,pt => if coll then True else pointInSpriteBox pt sp)
        False

spriteCollideWall : SpriteBox -> Wall -> Bool
spriteCollideWall sp w =
  spritesCollide sp (spriteBoxOfWall w)

heroBangsFace : Hero -> Model -> Bool
heroBangsFace proposedHero model =
  let
    heroSprite = spriteBox proposedHero
  in
  foldl
    (\hitFace,wall =>
      if hitFace then
        True
      else
        spriteCollideWall heroSprite wall
    )
    False
    (walls model)

heroWithXY : Double -> Double -> Hero -> Hero
heroWithXY x y h = set_hero_x x (set_hero_y y h)

heroInWall : Hero
heroInWall = heroWithXY (wallScaleX / 2.0) ((3.0 * roomYGrid) - (wallScaleY / 2.0)) startHero

heroBangsFaceShouldBeTrueIfHeroIsInTheMiddleOfLowerLeftWall : heroBangsFace Main.heroInWall Main.emptyModel = True
heroBangsFaceShouldBeTrueIfHeroIsInTheMiddleOfLowerLeftWall = Refl

heroInStairs : Hero -> Model -> Maybe Stairs
heroInStairs hero model =
  case gotStairs of
    [] => Nothing
    (_, st) :: _ => Just st
  where
    hx : Double
    hx = hero_x hero

    hy : Double
    hy = hero_y hero

    pairOfBoxAndStairs : Stairs -> List (SpriteBox, Stairs)
    pairOfBoxAndStairs st = map (\box => (box,st)) (stairsBox (floor model) st)

    stairsWithBoxes : List (List (SpriteBox, Stairs))
    stairsWithBoxes = map pairOfBoxAndStairs (stairs model)

    checkStairs : List (SpriteBox, Stairs)
    checkStairs = concat stairsWithBoxes

    gotStairs : List (SpriteBox, Stairs)
    gotStairs = filter (\(box,st) => pointInSpriteBox (hx,hy) box) checkStairs

runGame : Model -> Model
runGame model =
  let
    h = hero model
    hx = hero_x h
    hy = hero_y h
    cy = cam_y model
    fl = floor model
    km = keymap model
    st = stairs model
  in
  case hero_state h of
    BeforeStart =>
      if Data.AVL.Set.contains " " km then
        let
          new_hero =
            set_hero_y
              (roomYGrid * 2.9)
              (set_hero_x (roomXGrid / 2.0) (set_hero_state Playing h))
        in
        set_hero new_hero (set_space_latch True model)
      else
        emptyModel

    GoingIntoStairs frames newFloor (dx,dy) =>
      if frames == 0 then
        set_hero
          (set_hero_state
            (ComingOutOfStairs comingOutOfStairs (dx * (-1.0), dy * (-1.0)))
            h
          )
          (set_floor newFloor model)
      else
        set_hero
          (set_hero_state
            (GoingIntoStairs (frames-1) newFloor (dx,dy))
            (set_hero_x (hx+dx) (set_hero_y (hy+dy) h))
          )
          model

    ComingOutOfStairs frames (dx,dy) =>
      let
        moved_hero =
          if frames == 0 then
            set_hero_state Playing h
          else
            set_hero_state
              (ComingOutOfStairs (frames-1) (dx,dy))
              (set_hero_x (hx+dx) (set_hero_y (hy+dy) h))
      in
      set_hero moved_hero model

    Playing =>
      let
        xmove = sumMoveResult km [("a", xmoveDistPerFrame * (-1.0)), ("d", xmoveDistPerFrame)]
        ymove = sumMoveResult km [("w", ymoveDistPerFrame * (-1.0)), ("s", ymoveDistPerFrame)]
        want_moved_hero = set_hero_y (hy + ymove) (set_hero_x (hx + xmove) h)

        moved_hero =
          if heroBangsFace want_moved_hero model then
            h
          else
            want_moved_hero

        new_hy = hero_y moved_hero
        new_cam_y = max 0.0 (min camMaxY (new_hy - camHeroDist))
        literal_space_pressed = Data.AVL.Set.contains " " km
        space_pressed = literal_space_pressed && not (space_latch model)

        lightMatchIfOut =
          if hero_match h then
            False
          else
            space_pressed

        modelWithMatchLit =
          if lightMatchIfOut then
            set_matches ((matches model) + 1) model
          else
            model

        moved_hero_match =
          if lightMatchIfOut then
            set_hero_match True moved_hero
          else
            moved_hero

        entered_stairs = heroInStairs moved_hero_match model
      in
      case entered_stairs of
        Nothing =>
          set_cam_y
            new_cam_y
            (set_frameno ((frameno model) + 1)
              (set_hero
                moved_hero_match
                (set_space_latch literal_space_pressed modelWithMatchLit)))
        Just (MkStairs fl up pos) =>
          let
            new_floor = if up then fl + 1 else fl - 1
            move_dir = stairsDirection pos
            updated_hero =
              set_hero_state
                (GoingIntoStairs goingIntoStairsFrames new_floor move_dir)
                moved_hero_match
          in
          set_cam_y
            new_cam_y
            (set_frameno ((frameno model) + 1)
              (set_hero updated_hero modelWithMatchLit)
            )

handleInput : Dom m => (d: Var) -> (s: Var) -> Input -> ST m () [s:::State Model, d:::Gui {m}]
handleInput d s x =
  case x of
    KeyDown k =>
      do
        inp <- read s
        let km = keymap inp
        write s (set_keymap (Data.AVL.Set.insert k km) inp)

    KeyUp k =>
      do
        inp <- read s
        let km = keymap inp
        let turnoff = Data.AVL.Set.fromList [ k ]
        write s (set_keymap (Data.AVL.Set.difference km turnoff) inp)

    Tick =>
      do
        inp <- read s
        write s (runGame inp)
        call $ schedule d 30 Tick
        call $ domPut d inp

pageLoop : Dom m => (d: Var) -> (s:Var) -> ST m () [s:::State Model, d:::Gui {m}]
pageLoop d s =
  do
    x <- call $ getInput d

    handleInput d s x

    pageLoop d s

pageStart : Dom m => (d: Var) -> (s:Var) -> ST m () [s:::State Model, d:::Gui {m}]
pageStart d s =
  do
    call $ schedule d 30 Tick
    pageLoop d s

page : Dom m => ST m () []
page =
  do
    let model = emptyModel
    dom <- initBody [] vw () model
    txt <- new model
    pageStart dom txt
    delete txt
    clearDom dom

main : JS_IO ()
main = setASync_ $ run page
