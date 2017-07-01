-- {-# LANGUAGE RecordWildCards #-}

module Univalency
    ( initial
    , update
    , subscriptions
    , view
    , windowDims
    ) where


import           Linear.V2 (V2(V2))

import           Helm
import qualified Helm.Cmd as Cmd
import           Helm.Color
import           Helm.Engine.SDL (SDLEngine)
import           Helm.Graphics2D
import qualified Helm.Sub as Sub
import qualified Helm.Time as Time


data Action = DoNothing

data Model = Model
     -- { angleOffset :: Double
     -- }


windowDims :: V2 Int
windowDims = V2 1024 768

windowCenter :: V2 Double
windowCenter = (fromIntegral.(`div` 2)) <$> windowDims

squareColor :: Color
squareColor = rgb 0.6 0 0.05

squareForm :: Form e
squareForm = outlined (solid squareColor) (square 120)


initial :: (Model, Cmd SDLEngine Action)
initial = (Model, Cmd.none)

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update _ _ = (Model, Cmd.none)

subscriptions :: Sub SDLEngine Action
subscriptions = Sub.batch [{-Time.fps 60 Animate -}]

view :: Model -> Graphics SDLEngine
view _ = Graphics2D
    $ center windowCenter
    $ collage [squareForm]
