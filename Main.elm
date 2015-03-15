module Main where

import QuadTree
import Graphics.Element
import Graphics.Collage
import List
import Color
import Array
import Signal
import Signal (Signal)
import Time


------------ Signals -------------------

type Update = TimeDelta Float

inputs : Signal Update
inputs = Signal.map TimeDelta (Time.fps 3)


------------ Model -------------------

type Class = Peon | Warrior

type alias Unit =
  { class : Class
  }

type alias MapEntity = QuadTree.Bounded Unit
type alias Map       = QuadTree.QuadTree MapEntity

emptyMap : Map
emptyMap = QuadTree.emptyQuadTree
  { horizontal  = { high = 100, low = 0 }
  , vertical    = { high = 100, low = 0 }
  }
  15

unit : QuadTree.Bounded Unit
unit =
  { class = Peon
  , boundingBox =
    { horizontal = { high = 5, low = 4 }
    , vertical   = { high = 5, low = 4 }
    }
  }

unit2 : QuadTree.Bounded Unit
unit2 =
  { class = Warrior
  , boundingBox = QuadTree.boundingBox 95 98 95 98
  }




updatedMap = QuadTree.insert unit emptyMap
updatededMap = QuadTree.insert unit2 updatedMap
result = QuadTree.findItems ({class = Peon, boundingBox = QuadTree.boundingBox 4 5 4 5}) updatededMap


------------ Rendering -------------------

viewUnit : MapEntity -> Graphics.Collage.Form
viewUnit u =
    case u.class of
      Peon -> Graphics.Collage.circle 10
             |> Graphics.Collage.filled (Color.rgba 20 20 200 0.2)
             |> Graphics.Collage.move (u.boundingBox.horizontal.low, u.boundingBox.vertical.low)
      Warrior -> Graphics.Collage.circle 15
                 |> Graphics.Collage.filled Color.red
                 |> Graphics.Collage.move (u.boundingBox.horizontal.low, u.boundingBox.vertical.low)

view : GameState -> Graphics.Element.Element
view tree = tree.map
            |> QuadTree.getAllItems
            |> Array.toList
            |> List.map viewUnit
            |> Graphics.Collage.collage 500 500

--main : Graphics.Element.Element
--main = view updatededMap


type alias GameState =
  { units   : List Unit
  , map     : Map
  }

defaultGame : GameState
defaultGame =
    { units = []
    , map   = emptyMap
    }


stepGame : Update -> GameState -> GameState
stepGame update state = {state | map <- (QuadTree.insert unit state.map)}


gameState : Signal GameState
gameState =
    Signal.foldp stepGame defaultGame inputs

main : Signal Graphics.Element.Element
main =
    Signal.map view gameState









