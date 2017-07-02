module Main where

import           Univalency

import           Helm
import qualified Helm.Engine.SDL as SDL


main :: IO ()
main = do
    engine <- SDL.startupWith SDL.SDLEngineConfig
        { SDL.windowDimensions   = windowDims
        , SDL.windowIsFullscreen = False
        , SDL.windowIsResizable  = False
        , SDL.windowTitle        = "univalency"
        }

    run engine GameConfig
        { initialFn       = initial
        , updateFn        = update
        , subscriptionsFn = subscriptions
        , viewFn          = view
        }
