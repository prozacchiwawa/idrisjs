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

record Hero where
  constructor MkHero
  hero_state : HeroState
  hero_x : Double
  hero_y : Double
  hero_match : Bool

frameSpd : Double
frameSpd = 30.0

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

wallColor : String
wallColor = "#008000"

record Wall where
  constructor MkWall
  x : Double
  y : Double
  width : Double
  height : Double

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
  let
    leftRoomDoorwayLeftSide = (roomXGrid - wallScaleX) / 2.0
    leftRoomDoorwayRightSide = leftRoomDoorwayLeftSide + wallScaleX
    rightRoomDoorwayLeftSide = leftRoomDoorwayLeftSide + roomXGrid
    rightRoomDoorwayRightSide = leftRoomDoorwayRightSide + roomXGrid
    bottomRoomWallYTop = ((3.0 * roomYGrid) - wallScaleY)
  in
  [ {- Bottom row walls -}
    wallCoords 0.0 (3.0 * roomYGrid) leftRoomDoorwayLeftSide bottomRoomWallYTop
  , wallCoords leftRoomDoorwayRightSide (3.0 * roomYGrid) rightRoomDoorwayLeftSide bottomRoomWallYTop
  , wallCoords rightRoomDoorwayRightSide (3.0 * roomYGrid) (2.0 * roomXGrid) bottomRoomWallYTop
  ]

emptyModel : Model
emptyModel =
  MkModel
    0
    Data.AVL.Set.empty
    (MkHero BeforeStart 0 0 False)
    0
    0.0
    0
    False
    sixRoomLayoutWalls
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

implementation Drawable Wall where
  draw (MkWall x y w h) =
    let
      center_x = x + (w / 2.0)
      center_y = y + (h / 2.0)
    in
    draw (MkBox center_x center_y 0.0 w h wallColor)

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
    hc = heroCircles km fn cy h
  in
  (map draw hc) ++ (map (draw . (camOffset cy)) (walls model))

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
    ]
    ([ div [ style [ position (Absolute 0 0) ] ] (displays inp) ] ++ (drawables inp))

runGame : Model -> Model
runGame model =
  let
    h = hero model
    hx = hero_x h
    hy = hero_y h
    cy = cam_y model
    km = keymap model
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

    Playing =>
      let
        xmove = sumMoveResult km [("a", xmoveDistPerFrame * (-1.0)), ("d", xmoveDistPerFrame)]
        ymove = sumMoveResult km [("w", ymoveDistPerFrame * (-1.0)), ("s", ymoveDistPerFrame)]
        moved_hero = set_hero_y (hy + ymove) (set_hero_x (hx + xmove) h)
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
      in
      set_cam_y
        new_cam_y
        (set_frameno ((frameno model) + 1)
          (set_hero
            moved_hero_match
            (set_space_latch literal_space_pressed modelWithMatchLit)))

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
