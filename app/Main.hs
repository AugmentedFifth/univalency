module Main where

import           Univalency

import           Helm
import qualified Helm.Engine.SDL as SDL


main :: IO ()
main = do
    engine <- SDL.startupWith $ SDL.defaultConfig
        { SDL.windowIsResizable = False
        , SDL.windowDimensions  = windowDims
        }

    run engine GameConfig
        { initialFn       = initial
        , updateFn        = update
        , subscriptionsFn = subscriptions
        , viewFn          = view
        }
