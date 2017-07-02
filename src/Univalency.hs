{-# LANGUAGE RecordWildCards #-}

module Univalency
    ( initial
    , update
    , subscriptions
    , view
    , windowDims
    ) where


import           Data.List

import           Linear.Metric
import           Linear.V2 (V2(V2))
import           Linear.Vector hiding (unit)

import           Helm
import qualified Helm.Cmd as Cmd
import           Helm.Color
import           Helm.Engine.SDL (SDLEngine)
import           Helm.Graphics2D
import qualified Helm.Graphics2D.Text as Text
import qualified Helm.Keyboard as Keyboard
import qualified Helm.Sub as Sub
import qualified Helm.Time as Time


data Action
    = DoNothing
    | Animate Double
    | StartMove Keyboard.Key
    | StopMove Keyboard.Key

data PlayerStatus
    = Moving [Keyboard.Key]
    | Waiting

data Model = Model
    { playerPos    :: V2 Double
    , playerVel    :: V2 Double
    , playerAcc    :: V2 Double
    , playerStatus :: PlayerStatus
    , debug        :: String
    }


windowDims :: V2 Int
windowDims = V2 1024 768

windowCenter :: V2 Double
windowCenter = (fromIntegral.(`div` 2)) <$> windowDims

movementKeys :: [Keyboard.Key]
movementKeys =
    [ Keyboard.AKey
    , Keyboard.DKey
    , Keyboard.SKey
    , Keyboard.WKey
    ]

direction :: Keyboard.Key -> V2 Double
direction Keyboard.AKey = V2 (-1) 0
direction Keyboard.DKey = V2 1 0
direction Keyboard.SKey = V2 0 1
direction Keyboard.WKey = V2 0 (-1)
direction _             = error $ "Using a key that isn't "
                                ++ show movementKeys
                                ++ " to move"

squareColor :: Color
squareColor = rgb 0.6 0 0.05

squareForm :: Form e
squareForm = outlined (solid squareColor) (square 120)

playerImpetus :: Double
playerImpetus = 0.03

controlAcc :: Double
controlAcc = 0.005

frictionalAcc :: Double
frictionalAcc = 2e-5

playerMaxVel :: Double
playerMaxVel = 0.5

epsilon :: Double
epsilon = 5e-7

defaultText :: Text.Text
defaultText = Text.Text
    { textString   = ""
    , textColor    = rgb 1 1 1
    , textTypeface = "monospace"
    , textHeight   = 14
    , textWeight   = Text.NormalWeight
    , textStyle    = Text.NormalStyle
    }

toText :: String -> Text
toText s = defaultText { textString = s }

showV2 :: Show a => Int -> V2 a -> String
showV2 padTo (V2 x y) = '(' : xPad ++ xStr ++ ", " ++ yPad ++ yStr ++ ")"
    where
        xStr = show x
        xPad = replicate (padTo - length xStr) ' '
        yStr = show y
        yPad = replicate (padTo - length yStr) ' '

setPos :: V2 Double -> Form e -> Form e
setPos pos form@Form{..} = form { formPos = pos }


initial :: (Model, Cmd SDLEngine Action)
initial =
    ( Model
        { playerPos    = zero
        , playerVel    = zero
        , playerAcc    = zero
        , playerStatus = Waiting
        , debug        = ""
        }
    , Cmd.none
    )

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model@Model{..} (Animate dt) =
    ( model
        { playerPos    = newPos
        , playerVel    = newVel
        , playerAcc    = newAcc
        , debug = "pos: "
               ++ showV2 4 (round <$> newPos :: V2 Int)
               ++ "  ||  vel: "
               ++ showV2 5 ((round.(10000*)) <$> newVel :: V2 Int)
               ++ "  ||  acc: "
               ++ showV2 3 ((round.(10000*)) <$> newAcc :: V2 Int)
        }
    , Cmd.none
    )
    where
        updateSpeed vel acc
            | signum newSpeed /= signum vel
            , vel /= 0     = 0
            | newSpeed > 0 = min newSpeed playerMaxVel
            | otherwise    = max newSpeed (-playerMaxVel)
            where
                newSpeed = acc * dt + vel

        newPos = playerVel ^* dt + playerPos
        newVel' = liftI2 updateSpeed playerVel playerAcc
        newVel =
            if norm newVel' <= epsilon then
                zero
            else
                newVel'
        newAcc' = if norm newVel > 0 then
                      (frictionalAcc * dt *^).negate.signorm $ newVel
                  else
                      zero
        newAcc =
            case playerStatus of
                Moving keys -> controlAcc *^ sum (direction <$> keys) + newAcc'
                _           -> newAcc'

update model@Model{..} (StartMove key) =
    let updating =
            case playerStatus of
                Moving keys -> key `notElem` keys
                _           -> True
    in
        if updating then
            ( model
                { playerVel    = playerVel + playerImpetus *^ direction key
                , playerAcc    = playerAcc + controlAcc *^ direction key
                , playerStatus = newStatus
                }
            , Cmd.none
            )
        else
            (model, Cmd.none)
    where
        newStatus = case playerStatus of
            Moving keys -> Moving $ key : keys
            _           -> Moving [key]

update model@Model{..} (StopMove key) =
    ( model
        { playerStatus = newStatus
        }
    , Cmd.none
    )
    where
        newStatus = case playerStatus of
            Moving keys -> if length keys < 2 then
                               Waiting
                           else
                               Moving $ delete key keys
            _           -> Waiting

update model DoNothing = (model, Cmd.none)

subscriptions :: Sub SDLEngine Action
subscriptions = Sub.batch
    [ Time.fps 30 Animate
    , Keyboard.downs $
        \key -> if key `elem` movementKeys then StartMove key else DoNothing
    , Keyboard.ups $
        \key -> if key `elem` movementKeys then StopMove key else DoNothing
    ]

view :: Model -> Graphics SDLEngine
view Model{..} = Graphics2D
    $ collage
        [ toForm $ center (V2 xcenter 20)
                 $ collage [text $ toText debug]
        , toForm $ center windowCenter
                 $ collage [setPos playerPos squareForm]
        ]
    where
        V2 xcenter _ = windowCenter
