module App where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window


-- MODEL

type alias Model =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , dir : Direction
  , isAttacking : Bool
  , health : Int
  }


type Direction = Left | Right


type alias Keys = { x:Int, y:Int }


phil : Model
phil =
  { x = -600
  , y = 0
  , vx = 0
  , vy = 0
  , dir = Right
  , isAttacking = False
  , health = 3
  }

monster : Model
monster =
  { x = 400
  , y = 0
  , vx = 0
  , vy = 0
  , dir = Left
  , isAttacking = False
  , health = 1
  }

damsel : Model
damsel =
  { x = 285
  , y = 85
  , vx = 0
  , vy = 0
  , dir = Left
  , isAttacking = False
  , health = 3
  }


-- UPDATE

update : ( Float, Keys, Bool ) -> Model -> Model
update (dt, keys, spacebar) phil =
  phil
    |> gravity dt
    |> jump keys
    |> walk keys
    |> hit spacebar
    |> physics dt


jump : Keys -> Model -> Model
jump keys phil =
  if keys.y > 0 && phil.vy == 0 then
      { phil | vy = 8.0 }

  else
      phil


hit : Bool -> Model -> Model
hit spacebar phil =
  if spacebar then
  {phil | isAttacking = True}
  else
  {phil | isAttacking = False}

gravity : Float -> Model -> Model
gravity dt phil =
  { phil |
      vy = if phil.y > 0 then phil.vy - dt/4 else 0
  }


physics : Float -> Model -> Model
physics dt phil =
  { phil |
      x = phil.x + dt * phil.vx,
      y = max 0 (phil.y + dt * phil.vy)
  }


walk : Keys -> Model -> Model
walk keys phil =
  { phil |
      vx = 1.5 * toFloat keys.x,
      dir =
        if keys.x < 0 then
            Left

        else if keys.x > 0 then
            Right

        else
            phil.dir
  }

-- VIEW

view : (Int, Int) -> Model -> Element
view (w',h') phil =
  let
    (w,h) = (toFloat w', toFloat h')

    verb =
      if phil.y > 0 then
          "jump"

      else if phil.vx /= 0 then
          "walk"

      else if phil.isAttacking then
          "attk"

      else
          "stand"

    dir =
      case phil.dir of
        Left -> "left"
        Right -> "right"

    verb2 =
      if damsel.vx /= 0 then
          "walk"

      else
          "stand"

    dir2 =
      case damsel.dir of
        Left -> "left"
        Right -> "right"

    verb3 =
      if monster.vx /= 0 then
          "walk"

      else if phil.isAttacking && (phil.x > 370 && phil.x < 420) then
          "attk"

      else
          "stand"

    dir3 =
      case monster.dir of
        Left -> "left"
        Right -> "right"

    src =
      if verb == "stand" || verb == "jump" then
        "/imgs/hero/"++ verb ++ "/" ++ dir ++ ".png"
      else
        "/imgs/hero/"++ verb ++ "/" ++ dir ++ ".gif"

    src2 =
      if verb2 == "stand" then
        "/imgs/damsel/"++ verb ++ "/" ++ dir2 ++ ".png"
      else
        "/imgs/damsel/"++ verb ++ "/" ++ dir2 ++ ".gif"

    src3 =
      if verb3 == "stand" then
        "/imgs/enemy/"++ verb ++ "/" ++ dir3 ++ ".png"
      else
        "/imgs/enemy/"++ verb ++ "/" ++ dir3 ++ ".gif"

    philImage = image 34 44 src
    damselImage = image 34 44 src2
    monsterImage = image 84 94 src3

    -- XXX There must be a better way than this ...
    groundY =
      if (phil.x > 30) && (phil.x < 1000) then
        200 - h/2

      else if (phil.x < -450) then
        200 - h/2

      else
        175 - h/2

    -- XXX There must be a better way than this ...
    groundY2 =
      if (damsel.x > 30) && (damsel.x < 1000) then
        225 - h/2

      else if (damsel.x < -450) then
        200 - h/2

      else
        175 - h/2


    position = (phil.x, phil.y + groundY)
    position2 = (damsel.x, damsel.y + groundY2)
    position3 = (monster.x, monster.y + groundY2)

  in
    collage w' h'
      [ toForm (image w' h' "/imgs/beastlands2.png")
      , philImage
          |> toForm
          |> move position
      , damselImage
          |> toForm
          |> move position2
      , monsterImage
          |> toForm
          |> move position3
      ]


-- SIGNALS

main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp update phil input)

input : Signal ( Float, Keys, Bool )
input =
  let
    delta = Signal.map (\t -> t/20) (fps 30)
  in
    Signal.sampleOn delta (Signal.map3 (,,) delta Keyboard.arrows Keyboard.space)


