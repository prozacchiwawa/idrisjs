
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
  = Racing
  | Dead Double
  | Win Double

record Hero where
  constructor MkHero
  hero_state : HeroState
  hero_x : Double
  hero_y : Double
  hero_vx : Double
  hero_vy : Double

record Bullet where
  constructor MkBullet
  bullet_x : Double
  bullet_y : Double
  bullet_vx : Double
  bullet_vy : Double
  bullet_time : Double

frameSpd : Double
frameSpd = 30.0

minSpeed : Double
minSpeed = 10.0 / frameSpd

maxSpeed : Double
maxSpeed = 75.0 / frameSpd

avgSpeed : Double
avgSpeed = 27.0 / frameSpd

changeSpeed : Double
changeSpeed = 1.5 / frameSpd

jumpSpeed : Double
jumpSpeed = 50.0 / frameSpd

gravity : Double
gravity = 0.6 / frameSpd

restY : Double
restY = 230.0

distance : Double
distance = 10.0 * 1000.0

rock_min : Double
rock_min = 10.0

rock_max : Double
rock_max = 25.0

after : Double
after = 160.0

deadTime : Double
deadTime = 3.0

winTime : Double
winTime = 3.0

record Rock where
  constructor MkRock
  rock_x : Double
  rock_size : Double

implementation Eq Rock where
  r1 == r2 = (rock_x r1) == (rock_x r2)

implementation Ord Rock where
  compare r1 r2 =
    let
      x1 = rock_x r1
      x2 = rock_x r2
    in
    compare x1 x2

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

randomRocks : Int -> Int -> List Rock
randomRocks n p =
  let
    n1 = prng p
    rockSizeRatio = (cast n1) / 100000.0
    n2 = prng n1
    rockPosition = 1280.0 + ((cast n2) / 100000.0) * (distance - 1280.0)
  in
  if n == 0 then
    []
  else
    (MkRock rockPosition (rock_min + ((rock_max - rock_min) * rockSizeRatio))) :: (randomRocks (n-1) n2)

record Model where
  constructor MkModel
  n : Int
  cam_spd : Double
  cam_x : Double
  keymap : Set String
  hero : Hero
  bullets : List Bullet
  rocks : List Rock

emptyModel : Model
emptyModel =
  MkModel
    0
    0
    avgSpeed
    Data.AVL.Set.empty
    (MkHero Racing 0 restY avgSpeed 0)
    []
    (sort (randomRocks 30 levelSeed))
    
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

data CircleToRoundedBoxHit
  = NoHit
  | UpperLeft
  | LowerLeft
  | UpperRight
  | LowerRight
  | LeftFace
  | TopFace
  | RightFace
  | BottomFace
  | Within

implementation Eq CircleToRoundedBoxHit where
  NoHit == NoHit = True
  UpperLeft == UpperLeft = True
  LowerLeft == LowerLeft = True
  UpperRight == UpperRight = True
  LowerRight == LowerRight = True
  LeftFace == LeftFace = True
  TopFace == TopFace = True
  RightFace == RightFace = True
  BottomFace == BottomFace = True
  Within == Within = True
  _ == _ = False

hitFromCornerTest : Double -> List (Double,CircleToRoundedBoxHit) -> CircleToRoundedBoxHit
hitFromCornerTest hit_dist_sq ((d,tag) :: _) =
  if d < hit_dist_sq then tag else NoHit
hitFromCornerTest _ _ = NoHit

{- Intersection happens when:
 - The circle is near enough to a corner that a circle at the corner with the box's
 - corner radius and the circle touch.
 - The circle is within the x bounds of the box and within height/2 + radius of the y
 - coord.
 - The circle is within the y bounds of the box and within width/2 + radius of the x
 - coord.
 - Return the hit type.
 -}
circle_to_rounded_box_intersection : Circle -> Box -> CircleToRoundedBoxHit
circle_to_rounded_box_intersection c b =
  let 
    cx = center_x c
    cy = center_y c
    radius = radius c
    bx = box_x b
    by = box_y b
    bw = box_width b
    bh = box_height b
    brad = box_corner b
    ylb = by - bh / 2.0
    yub = by + bh / 2.0
    xlb = bx - bw / 2.0
    xub = bx + bw / 2.0
    lr = (xub - brad,yub - brad)
    ur = (xub - brad,ylb + brad)
    ll = (xlb + brad,yub - brad)
    ul = (xlb + brad,ylb + brad)
    hit_dist_sq = (radius * radius) + (brad * brad)
  in
  if cx > xlb && cx < xub && cy > ylb && cy < yub then
    Within
  else if cx > xlb && cy < xub then
    (if cy > ylb - radius && cy < by then
      TopFace
    else if cy < yub + radius && cy > by then
      BottomFace
    else
      NoHit
    )
  else if cy > ylb && cy < yub then
    (if cx > xlb - radius && cx < bx then
      LeftFace
    else if cx < xub + radius && cx > bx then
      RightFace
    else
      NoHit
    )
  else
    {- We only have corners left.  We'll list the corners and compute the p2p dist sq
     - of each to (cx,cy) and sort them.  If the first one is less than hit_dist_sq
     - then hit, otherwise no hit.
     -}
    let
      testCorners =
        sortBy
          (\(d1,_),(d2,_) => compare d1 d2)
          (map
            (\(bc,t) => 
              (p2p_dist_sq bc (cx,cy),t)
            ) 
            [ (lr,LowerRight)
            , (ur,UpperRight)
            , (ll,LowerLeft)
            , (ul,UpperLeft)
            ]
          )
    in
    hitFromCornerTest hit_dist_sq testCorners

circle_to_rounded_box_intersection_within_test : circle_to_rounded_box_intersection (MkCircle 0 0 0 "") (MkBox 0 0 1 5 5 "") = Within
circle_to_rounded_box_intersection_within_test = Refl

circle_to_rounded_box_intersection_left_face_test : circle_to_rounded_box_intersection (MkCircle (-1.2) 0.0 0.3 "") (MkBox 0 0 0 2 2 "") = LeftFace
circle_to_rounded_box_intersection_left_face_test = Refl

circle_to_rounded_box_intersection_left_face_test2 : circle_to_rounded_box_intersection (MkCircle (-1.4) 0.0 0.3 "") (MkBox 0 0 0 2 2 "") = NoHit
circle_to_rounded_box_intersection_left_face_test2 = Refl

firstHit : List (CircleToRoundedBoxHit,Rock) -> Maybe (CircleToRoundedBoxHit,Rock)
firstHit [] = Nothing
firstHit ((NoHit,_) :: tl) = firstHit tl
firstHit (a :: tl) = Just a

{- The first rock we get something other than NoHit on -}
hero_touching_rock : Model -> Maybe (CircleToRoundedBoxHit,Rock)
hero_touching_rock model =
  let 
    hx = hero_x (hero model)
    hy = hero_y (hero model)
    hero_circ = MkCircle hx hy 5.0 "#3407ca"
    ground_rocks = rocks model
  in
  firstHit 
    (map 
      (\r => 
        let
          rx = rock_x r
          rs = rock_size r
          ry = 240.0 - (rs / 2.0)
          rbox = MkBox (320.0 + rx) ry 5.0 rs rs "#888"
        in
        (circle_to_rounded_box_intersection hero_circ rbox,r)
      )
      ground_rocks
    )

Gui : Dom m => Type
Gui {m} = DomRef {m} () (const Model) (const Input) ()

interface Drawable a where
  draw : a -> Html Input
  
implementation Drawable Circle where
  draw c =
    div
      [ style 
          [ position (Fixed (center_x c) (center_y c))
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
              (Fixed 
                ((box_x b) - ((box_width b) / 2.0)) 
                ((box_y b) - ((box_height b) / 2.0))
              )
          , width (box_width b)
          , height (box_height b)
          , borderRadius (box_corner b)
          , backgroundColor (box_color b)
          ]
      ] []
      
get_hero_color : Hero -> String
get_hero_color h =
  case hero_state h of
    Racing => "#3407ca"
    Win t =>
      let c = 1.0 + ((sin t) / 2.0) in
      "rgb(" ++ (show (255.0 * c)) ++ "," ++ (show (255.0 * c)) ++ "," ++ (show (255.0 * c)) ++ ")"
    Dead t => 
      let c = 1.0 + ((sin t) / 2.0) in
      "rgb(" ++ (show (255.0 * c)) ++ ",0,0)"
      
drawables : Model -> List (Html Input)
drawables model =
  let
    hx = hero_x (hero model)
    hy = hero_y (hero model)
    cx = cam_x model
    heroColor = get_hero_color (hero model)
  in
  [ draw (MkCircle (320.0 - cx + hx) hy 5.0 heroColor)
  , draw (MkBox 320.0 (240.0 + 240.0 / 2.0) 0.0 640.0 240.0 "#ccc")
  , draw (MkBox (320.0 - cx) 250.0 3.0 10.0 20.0 "red")
  , draw (MkBox (320.0 - cx + distance) 250.0 3.0 10.0 20.0 "red")
  ] ++ (
    map 
      (\t => 
        let
          md = t * distance
        in
        draw (MkBox (320.0 - cx + md) 250.0 3.0 10.0 20.0 "white")
      )
      [0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9]
  ) ++ (
    map
      (\r =>
        let
          rx = rock_x r
          rs = rock_size r
          ry = 240.0 - (rs / 2.0)
        in
        draw (MkBox (320.0 + rx - cx) ry 5.0 rs rs "#888")
      )
      (rocks model)
  )
  
displays : Model -> List (Html Input)
displays model =
  [ div [] [text ("hx = " ++ (show (hero_x (hero model))))]
  , div [] [text ("hy = " ++ (show (hero_y (hero model))))]
  , div [] [text ("hvx = " ++ (show (hero_vx (hero model))))]
  , div [] [text ("hvy = " ++ (show (hero_vy (hero model))))]
  , div [] [text ("cx = " ++ (show (cam_x model)))]
  , div [] [text ("cs = " ++ (show (cam_spd model)))]
  ]

vw : () -> Model -> Html Input
vw () inp = 
  div 
    [ style 
        [ position (Fixed 0 0)
        , width 640
        , height 480
        , color "white"
        , backgroundColor "black"
        , margin 0
        , padding 0
        ] 
    , onkeydown (\k => KeyDown (key k))
    , onkeyup (\k => KeyUp (key k))
    , tabindex 0
    ]
    ((displays inp) ++ (drawables inp))

{- We hit the brake, causing speed to decrease to a minimum value -}
heroDecelerate : Model -> Model
heroDecelerate model =
  let
    h = hero model
  in
  set_hero (set_hero_vx (max minSpeed ((hero_vx h) - changeSpeed)) h) model

heroAccelerate : Model -> Model
heroAccelerate model =
  let
    h = hero model
  in
  set_hero (set_hero_vx (min maxSpeed ((hero_vx h) + changeSpeed)) h) model
      
heroModerate : Model -> Model
heroModerate model =
  let
    h = hero model
  in
  set_hero (set_hero_vx ((avgSpeed + (hero_vx h)) / 2.0) h) model
      
heroFall : Model -> Model
heroFall model =
  let 
    h = hero model
    fvy = (hero_vy h) + gravity
    fy = (hero_y h) + fvy
    fvx = (hero_vx h)
    fx = min ((hero_x h) + fvx) (distance + after)
    h' =
      if fy >= restY then
        set_hero_y restY (set_hero_vy 0 h)
      else
        set_hero_y fy (set_hero_vy fvy h)
  in
  set_hero (set_hero_x fx h') model

heroJump : Model -> Model
heroJump model =
  let
    h = hero model
  in
  set_hero (set_hero_vy (jumpSpeed * -1.0) h) model

camMove : Model -> Model
camMove model =
  let
    cs = ((cam_spd model) + (hero_vx (hero model))) / 2.0
    cx = (cam_x model) + cs
  in
  set_cam_spd cs (set_cam_x (min cx (distance + after)) model)
                                                      
handleKeys : Model -> Model
handleKeys model =
  let
    hx = hero_x (hero model)
    hy = hero_y (hero model)
    km = keymap model
    model' = camMove (heroFall model)
  in
  if Data.AVL.Set.contains "a" km then
    if hy == restY then
      heroDecelerate model'
    else
      model'
  else if Data.AVL.Set.contains "s" km then
    {- shoot/charge jump -}
    model'
  else if Data.AVL.Set.contains "d" km then
    if hy == restY then
      heroAccelerate model'
    else
      model'
  else if Data.AVL.Set.contains "w" km then
    if hy == restY then
      heroJump model'
    else
      model'
  else
    if hy == restY then
      heroModerate model'
    else
      model'

winGame : Model -> Model
winGame model =
  let
    h = hero model
  in
  set_hero (set_hero_state (Win winTime) h) model

normalRun : Model -> Model
normalRun model =
  let 
    hx = hero_x (hero model)
    updated = handleKeys (set_n ((n model) + 1) model)
  in
  if hx > distance then
    winGame updated
  else
    updated
    
bounceUp : Rock -> Model -> Model
bounceUp rock model =
  let
    h = hero model
  in
  set_hero (set_hero_vy ((hero_vy h) * -1.0) h) model

deadState : Model -> Model
deadState model =
  let
    h = hero model
  in
  set_hero (set_hero_state (Dead deadTime) h) model

runGame : Model -> Model
runGame model =
  let
    h = hero model
    hx = hero_x h
  in
  case hero_state h of
    Racing =>
      let htr = hero_touching_rock model in
      case htr of
        Nothing => normalRun model
        Just (NoHit,_) => normalRun model
        Just (TopFace,r) => normalRun (bounceUp r model)
        Just (_,r) => deadState model
    Dead t =>
      if t < 0.0 then
        emptyModel {- Start game over -}
      else
        set_hero (set_hero_state (Dead (t - (1.0 / frameSpd))) h) model
    Win t =>
      if t < 0.0 then
        emptyModel
      else
        set_hero (set_hero_state (Win (t - (1.0 / frameSpd))) h) (normalRun model)

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
