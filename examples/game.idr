
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
  hero_shot_time : Int

record Bullet where
  constructor MkBullet
  bullet_x : Double
  bullet_y : Double
  bullet_vx : Double
  bullet_vy : Double
  bullet_time : Int

frameSpd : Double
frameSpd = 30.0

minSpeed : Double
minSpeed = 30.0 / frameSpd

maxSpeed : Double
maxSpeed = 200.0 / frameSpd

avgSpeed : Double
avgSpeed = 60.0 / frameSpd

changeSpeed : Double
changeSpeed = 1.5 / frameSpd

jumpSpeed : Double
jumpSpeed = 100.0 / frameSpd

gravity : Double
gravity = 6.0 / frameSpd

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

maxShots : Int
maxShots = 3

shotCool : Int
shotCool = 7

shotOutFrontDist : Double
shotOutFrontDist = 10.0

shotMuzzleSpeed : Double
shotMuzzleSpeed = 500.0 / frameSpd

shotLife : Int
shotLife = 40

record Rock where
  constructor MkRock
  rock_x : Double
  rock_y : Double
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
    let rs = (rock_min + ((rock_max - rock_min) * rockSizeRatio)) in
    (MkRock rockPosition (240.0 - (rs / 2.0)) rs) :: (randomRocks (n-1) n2)

record Model where
  constructor MkModel
  frameno : Int
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
    (MkHero Racing 0 restY avgSpeed 0 0)
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

implementation Show CircleToRoundedBoxHit where
  show NoHit = "NH"
  show UpperLeft = "UL"
  show LowerLeft = "LL"
  show UpperRight = "UR"
  show LowerRight = "LR"
  show LeftFace = "LF"
  show TopFace = "TF"
  show RightFace = "RF"
  show BottomFace = "BF"
  show Within = "Wi"

hitFromCornerTest : Double -> List (Double,CircleToRoundedBoxHit) -> CircleToRoundedBoxHit
hitFromCornerTest hit_dist_sq ((d,tag) :: _) =
  if d < hit_dist_sq then tag else NoHit
hitFromCornerTest _ _ = NoHit

testCorners : ((Double,Double),(Double,Double),(Double,Double),(Double,Double)) -> (Double,Double) -> List (Double,CircleToRoundedBoxHit)
testCorners (ll,lr,ul,ur) (cx,cy) =
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

corners : Box -> ((Double,Double),(Double,Double),(Double,Double),(Double,Double))
corners b =
  let
    bx = box_x b
    by = box_y b
    bw = box_width b
    bh = box_height b
    brad = box_corner b
    ylb = by - (bh / 2.0) + brad
    yub = by + (bh / 2.0) - brad
    xlb = bx - (bw / 2.0) + brad
    xub = bx + (bw / 2.0) - brad
    lr = (xub,yub)
    ur = (xub,ylb)
    ll = (xlb,yub)
    ul = (xlb,ylb)
  in
  (ll,lr,ul,ur)

hit_dist_sq : Circle -> Box -> Double
hit_dist_sq c b =
  let
    crad = radius c
    brad = box_corner b
    tots = crad + brad
  in
  tots * tots
      
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
    bx = box_x b
    by = box_y b
    cx = center_x c
    cy = center_y c
    crad = radius c
    brad = box_corner b
    (ll,lr,ul,ur) = corners b
    (ylb,xlb) = ul
    (yub,xub) = lr
  in
  if cx > xlb && cx < xub && cy > ylb && cy < yub then
    Within
  else if cy > ylb && cy < yub && cx > bx && cx < xub + crad + brad then
    RightFace
  else if cy > ylb && cy < yub && cx < bx && cx > xlb - crad - brad then
    LeftFace
  else if cx > xlb && cx < xub && cy > by && cy < yub + crad + brad then
    TopFace
  else if cx > xlb && cx < xub && cy < by && cy > ylb - crad - brad then
    BottomFace
  else
    {- We only have corners left.  We'll list the corners and compute the p2p dist sq
     - of each to (cx,cy) and sort them.  If the first one is less than hit_dist_sq
     - then hit, otherwise no hit.
     -}
    hitFromCornerTest (hit_dist_sq c b) (testCorners (ll,lr,ul,ur) (cx,cy))

pointsToCheck : List Double
pointsToCheck = [-1.4,-1.1,-0.8,0.0,0.8,1.1,1.4]
checkGrid : List (List (Double,Double))
checkGrid = map (\p => map (\r => (r,p)) pointsToCheck) pointsToCheck

wantGrid : List (List String)
wantGrid = 
  [ ["NH","NH","NH","NH","NH","NH","NH"]
  , ["NH","UL","BF","BF","BF","UR","NH"]
  , ["NH","LF","Wi","Wi","Wi","RF","NH"]
  , ["NH","LF","Wi","Wi","Wi","RF","NH"]
  , ["NH","LF","Wi","Wi","Wi","RF","NH"]
  , ["NH","LL","TF","TF","TF","LR","NH"]
  , ["NH","NH","NH","NH","NH","NH","NH"]
  ]

testBox : Box
testBox = (MkBox 0 0 0 2 2 "")
testCirc : Double -> Double -> Circle
testCirc x y = (MkCircle x y 0.3 "")
      
touchRow : List (Double,Double) -> List String
touchRow =
  map 
    (\(x,y) => 
      show (circle_to_rounded_box_intersection (testCirc x y) testBox)
    )
    
haveGrid : List (List String)
haveGrid = map touchRow checkGrid

haveGridWantGridTest : Main.haveGrid = Main.wantGrid
haveGridWantGridTest = Refl
  
circle_to_rounded_box_intersection_within_test : circle_to_rounded_box_intersection (MkCircle 0 0 0 "") (MkBox 0 0 1 5 5 "") = Within
circle_to_rounded_box_intersection_within_test = Refl

circle_to_rounded_box_intersection_left_face_test : circle_to_rounded_box_intersection (testCirc (-1.2) 0.0) Main.testBox = LeftFace
circle_to_rounded_box_intersection_left_face_test = Refl

circle_to_rounded_box_intersection_left_face_test2 : circle_to_rounded_box_intersection (testCirc (-1.4) 0.0) Main.testBox = NoHit
circle_to_rounded_box_intersection_left_face_test2 = Refl

circle_to_rounded_box_intersection_right_face_test : circle_to_rounded_box_intersection (testCirc 1.2 0.0) Main.testBox = RightFace
circle_to_rounded_box_intersection_right_face_test = Refl

circle_to_rounded_box_intersection_right_face_test2 : circle_to_rounded_box_intersection (testCirc 1.4 0.0) Main.testBox = NoHit
circle_to_rounded_box_intersection_right_face_test2 = Refl

firstHit : List (CircleToRoundedBoxHit,Rock) -> Maybe (CircleToRoundedBoxHit,Rock)
firstHit [] = Nothing
firstHit ((NoHit,_) :: tl) = firstHit tl
firstHit (a :: tl) = Just a

bulletCircle : Bullet -> Circle
bulletCircle bullet =
  MkCircle (bullet_x bullet) (bullet_y bullet) 2.0 "white"

heroCircle : Hero -> Circle
heroCircle hero =
  MkCircle (hero_x hero) (hero_y hero) 5.0 "#3407ca"

{- The first rock we get something other than NoHit on -}
circle_touching_rock : Circle -> Model -> Maybe (CircleToRoundedBoxHit,Rock)
circle_touching_rock circ model =
  let 
    ground_rocks = rocks model
  in
  firstHit
    (map
      (\r =>
        let
          rx = rock_x r
          ry = rock_y r
          rs = rock_size r
          rbox = MkBox rx ry 5.0 rs rs "#888"
        in
        (circle_to_rounded_box_intersection circ rbox,r)
      )
      ground_rocks
    )

{- Hero touching rock -}
hero_touching_rock : Model -> Maybe (CircleToRoundedBoxHit,Rock)
hero_touching_rock model = circle_touching_rock (heroCircle (hero model)) model

{- Bullet touching rock -}
bullet_touching_rock : Bullet -> Model -> Maybe (CircleToRoundedBoxHit,Rock)
bullet_touching_rock b = circle_touching_rock (bulletCircle b)

shotsLandedOnRock : Model -> (List (Rock,Bullet), List Bullet)
shotsLandedOnRock model =
  let
    results = map recordTouchingBullet (bullets model)
  in
  foldl addToResult ([],[]) results
  where
    addToResult : (List (Rock,Bullet), List Bullet) -> (List (Rock,Bullet), List Bullet) -> (List (Rock,Bullet), List Bullet)
    addToResult (hits,flying) (newhits,newflying) =
      (hits ++ newhits, flying ++ newflying)
      
    recordTouchingBullet : Bullet -> (List (Rock,Bullet), List Bullet)
    recordTouchingBullet b =
      case bullet_touching_rock b model of
        Nothing => ([],[b])
        Just (_,r) => ([(r,b)],[])
                
Gui : Dom m => Type
Gui {m} = DomRef {m} () (const Model) (const Input) ()

interface Drawable a where
  draw : a -> Html Input
  
implementation Drawable Circle where
  draw c =
    div
      [ style 
          [ position (Absolute (center_x c) (center_y c))
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
          ry = rock_y r
          rs = rock_size r
        in
        draw (MkBox (320.0 + rx - cx) ry 5.0 rs rs "#888")
      )
      (rocks model)
  ) ++ (
    map
      (\b =>
        let
          bx = bullet_x b
          by = bullet_y b
        in
        draw (MkCircle (320 + bx - cx) by 2.0 "white")
      )
      (bullets model)
  )
  
displays : Model -> List (Html Input)
displays model =
  [ div [] [text ("hx = " ++ (show (hero_x (hero model))))]
  , div [] [text ("hy = " ++ (show (hero_y (hero model))))]
  , div [] [text ("hvx = " ++ (show (hero_vx (hero model))))]
  , div [] [text ("hvy = " ++ (show (hero_vy (hero model))))]
  , div [] [text ("st = " ++ (show (hero_shot_time (hero model))))]
  , div [] [text ("cx = " ++ (show (cam_x model)))]
  , div [] [text ("cs = " ++ (show (cam_spd model)))]
  , div [] [text ("fn = " ++ (show (frameno model)))]
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
    hvx = hero_vx (hero model)
  in
  if hvx < avgSpeed then
    set_hero (set_hero_vx (min (hvx + changeSpeed) avgSpeed) h) model
  else if hvx > avgSpeed then
    set_hero (set_hero_vx (max (hvx - changeSpeed) avgSpeed) h) model
  else
    model

canShoot : Model -> Bool
canShoot model =
  let
    h = hero model
    frame = frameno model
    shott = hero_shot_time h
  in
  (frame - shott) > shotCool
  
shoot : Model -> Model
shoot model =
  let 
    h = hero model
    prev_shots =
      case (bullets model) of
        [] => []
        [a] => [a]
        a :: b :: _ => [a,b]
    hx = hero_x h
    hy = hero_y h
    hvx = hero_vx h
    hvy = hero_vy h
  in
  set_bullets ((MkBullet (hx + shotOutFrontDist) hy (hvx + shotMuzzleSpeed) hvy shotLife) :: prev_shots) (set_hero (set_hero_shot_time (frameno model) h) model)
  
updateShot : Bullet -> Bullet
updateShot b =
  MkBullet 
    ((bullet_x b) + (bullet_vx b))
    ((bullet_y b) + (bullet_vy b))
    (bullet_vx b)
    (bullet_vy b)
    ((bullet_time b) - 1)
  
subtractRocks : List Rock -> Model -> Model
subtractRocks rs model =
  let 
    locset = Data.AVL.Set.fromList (map rock_x rs)
  in
  set_rocks (filter (\r => not (Data.AVL.Set.contains (rock_x r) locset)) (rocks model)) model
  
runBullets : Model -> Model
runBullets model =
  let
    shots = bullets model
    shotsUpdated = filter (\b => bullet_time b > 0) (map updateShot shots)
    model' = set_bullets shotsUpdated model
    (shotHits,flying) = shotsLandedOnRock model'
  in
  subtractRocks (map (\(r,b) => r) shotHits) (set_bullets flying model')
      
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
    model' = runBullets (camMove (heroFall model))
  in
  if Data.AVL.Set.contains "a" km then
    if hy == restY then
      heroDecelerate model'
    else
      model'
  else if Data.AVL.Set.contains "s" km then
    if canShoot model then
      shoot model'
    else
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
    updated = handleKeys (set_frameno ((frameno model) + 1) model)
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
