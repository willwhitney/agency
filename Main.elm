module Main where

import QuadTree
import Graphics.Element
import Graphics.Collage
import List
import Color
import Array

--type alias Coord = (Int, Int)

type Class = Peon | Warrior

type alias Unit a =
  { a |
    class : Class
  }

type alias Map a = QuadTree.QuadTree a
type alias MapEntity a = Unit a

worldMap : Map (MapEntity (QuadTree.Bounded {}))
worldMap = QuadTree.emptyQuadTree
  { horizontal  = { high = 100, low = 0 }
  , vertical    = { high = 100, low = 0 }
  }
  10

unit : Unit (QuadTree.Bounded {})
unit =
  { class = Peon
  , boundingBox =
    { horizontal = { high = 5, low = 4 }
    , vertical   = { high = 5, low = 4 }
    }
  }

unit2 : Unit (QuadTree.Bounded {})
unit2 =
  { class = Peon
  , boundingBox = QuadTree.boundingBox 95 98 95 98
  }




updatedMap = QuadTree.insert unit worldMap
updatededMap = QuadTree.insert unit2 updatedMap
--result = QuadTree.findItems ({class = Peon, boundingBox = QuadTree.boundingBox 4 5 4 5}) updatedMap

--containerView : Graphics.Element

viewUnit : Unit (QuadTree.Bounded {}) -> Graphics.Collage.Form
viewUnit u = Graphics.Collage.move (u.boundingBox.horizontal.low, u.boundingBox.vertical.low) (Graphics.Collage.filled Color.red (Graphics.Collage.circle 10))

view : QuadTree.QuadTree (MapEntity (QuadTree.Bounded {})) -> Graphics.Element.Element
view tree = Graphics.Collage.collage 500 500 (List.map viewUnit (Array.toList (QuadTree.getAllItems tree)))

main : Graphics.Element.Element
main = view updatededMap


--type alias GameState =
--  { units   : List Unit
--  , worldMap     :
--  }

--defaultGame : GameState
--defaultGame =
--    {}


