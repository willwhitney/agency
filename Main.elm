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
import Debug

worldWidth  = 500
worldHeight = 500

------------ Signals -------------------

type Update = TimeDelta Float

inputs : Signal Update
inputs = Signal.map TimeDelta (Time.fps 3)


------------ Model -------------------

type UnitClass = Peon | Warrior

type alias Unit = QuadTree.Bounded {class : UnitClass}

type alias MapEntity = Unit
type alias Map       = QuadTree.QuadTree MapEntity

emptyMap : Map
emptyMap = QuadTree.emptyQuadTree
  { horizontal  = { high = worldWidth,  low = 0 }
  , vertical    = { high = worldHeight, low = 0 }
  }
  100000

unit : Unit
unit =
  { class = Peon
  , boundingBox =
    { horizontal = { high = 5, low = 4 }
    , vertical   = { high = 5, low = 4 }
    }
  }

unit2 : Unit
unit2 =
  { class = Warrior
  , boundingBox = QuadTree.boundingBox 95 125 95 125
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
             |> Graphics.Collage.move (u.boundingBox.horizontal.high, u.boundingBox.vertical.high)
      Warrior -> Graphics.Collage.circle 15
                 |> Graphics.Collage.filled (Color.rgba 200 20 20 0.2)
                 |> Graphics.Collage.move (u.boundingBox.horizontal.high, u.boundingBox.vertical.high)

view : GameState -> Graphics.Element.Element
view tree = tree.map
            |> QuadTree.getAllItems
            |> Array.toList
            |> List.map viewUnit
            |> Graphics.Collage.collage worldWidth worldHeight

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

stepUnit : Unit -> Unit
stepUnit unit =
    let newXHigh = min (worldWidth / 2) (unit.boundingBox.horizontal.high + 3)
        newXLow  = newXHigh - (QuadTree.width unit.boundingBox)
        newYHigh = min (worldHeight / 2) (unit.boundingBox.vertical.high + 3)
        newYLow  = newYHigh - (QuadTree.width unit.boundingBox)
    in
        Debug.watch "new unit" {unit | boundingBox <- QuadTree.boundingBox newXLow newXHigh newYLow newYHigh}

stepGame : Update -> GameState -> GameState
stepGame update state =
    let newUnit         = unit
        updatedUnits    = Debug.watch "new unit list" (newUnit :: (List.map stepUnit state.units))
        --unitUpdatePairs = List.map2 (,) state.units updatedUnits
        newMap          = List.foldl (QuadTree.update stepUnit) state.map updatedUnits
    in
        { state |
          units <- updatedUnits
        , map   <- QuadTree.insert newUnit newMap
        }


gameState : Signal GameState
gameState =
    Signal.foldp stepGame defaultGame inputs



main : Signal Graphics.Element.Element
main = Signal.map view gameState
