module UI where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center

ui :: Widget ()
ui = str "Hello, world!"

w :: Widget ()
w = withBorderStyle Brick.Widgets.Border.Style.ascii $
        Brick.Widgets.Border.border $ center $ str "Hello, world!"

main :: IO ()
main = simpleMain w
