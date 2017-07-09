{-# LANGUAGE RecordWildCards #-}

module Univalency
    ( initial
    , update
    , subscriptions
    , view
    , windowDims
    ) where


import           Data.List            hiding (group)

import           Linear.Metric
import           Linear.V2            (V2 (V2))
import           Linear.Vector        hiding (unit)

import           Helm
import qualified Helm.Cmd             as Cmd
import           Helm.Color
import           Helm.Engine.SDL      (SDLEngine)
import           Helm.Graphics2D
import qualified Helm.Graphics2D.Text as Text
import qualified Helm.Keyboard        as Keyboard
import qualified Helm.Mouse           as Mouse
import qualified Helm.Sub             as Sub
import qualified Helm.Time            as Time


data Action
    = DoNothing
    | Animate Double
    | StartMove Keyboard.Key
    | StopMove Keyboard.Key
    | MouseMove (V2 Int)
    | MouseClick Mouse.MouseButton (V2 Int)
    | MouseRelease Mouse.MouseButton (V2 Int)

data PlayerStatus
    = Moving [Keyboard.Key]
    | Waiting

data ClickEffect = Active Int (V2 Int)

data Model = Model
    { playerPos    :: V2 Double
    , playerVel    :: V2 Double
    , playerAcc    :: V2 Double
    , playerAngle  :: Double
    , playerStatus :: PlayerStatus
    , shadowPos    :: V2 Double
    , shadowVel    :: V2 Double
    , shadowAcc    :: V2 Double
    , shadowAngle  :: Double
    , shadowAngVel :: Double
    , shadowAngAcc :: Double
    , mousePos     :: V2 Double
    , mouseDown    :: Bool
    , clickEffects :: [ClickEffect]
    , tractorLoop  :: Int
    , debug        :: String
    }


windowDims :: V2 Int
windowDims = V2 1024 768

windowCenter :: V2 Double
windowCenter = (fromIntegral.(`div` 2)) <$> windowDims

centerForms :: V2 Double -> [Form e] -> Form e
{-# INLINE centerForms #-}
centerForms pos forms = toForm $ center pos
                               $ collage forms

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
squareColor = rgb 0.5 0 0.05

squareForm :: Form e
squareForm = filled squareColor (square 120)

tractorLoopPeriod :: Int
tractorLoopPeriod = 90  -- Frames

tractorForm :: Int -> Form e
tractorForm i =
    let semiperiod = tractorLoopPeriod `div` 2
        stopPos =
            fromIntegral (if i <= semiperiod then i else 2 * semiperiod - i) /
                fromIntegral tractorLoopPeriod
        origRect = gradient
            ( linear
                (-45, 60)
                (90,  60)
                [ (0,       rgba 0.125  0.375 0.625 0.1875)
                , (stopPos, rgba 0.0625 0.5   0.375 0)
                , (1,       rgba 0.0625 0.5   0.375 0)
                ]
            )
            (rect $ V2 90 120)
    in  origRect { formPos = V2 105 0 }

shadowForm :: Form e
shadowForm = filled shadowColor (square 128)
    where
        Color r g b _ = squareColor
        shadowColor = rgba (r - 0.25) (g + 0.125) (b + 0.0625) 0.375

playerImpetus :: Double
playerImpetus = 3e-2

controlAcc :: Double
controlAcc = 7.5e-3

frictionalAcc :: Double
frictionalAcc = 1e-4

playerMaxSqSpeed :: Double
playerMaxSqSpeed = 0.375

minTurnVel :: Double
minTurnVel = 0.015625

turnVel :: Double
turnVel = 0.01

springConstant :: Double
springConstant = 2e-4

epsilon :: Double
epsilon = 1e-8

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

normalizeAngle :: Double -> Double
{-# INLINE normalizeAngle #-}
normalizeAngle t = t - if abs t > pi then signum t * 2 * pi else 0


initial :: (Model, Cmd SDLEngine Action)
initial =
    ( Model
        { playerPos    = zero
        , playerVel    = zero
        , playerAcc    = zero
        , playerAngle  = 0
        , playerStatus = Waiting
        , shadowPos    = zero
        , shadowVel    = zero
        , shadowAcc    = zero
        , shadowAngle  = 0
        , shadowAngVel = 0
        , shadowAngAcc = 0
        , mousePos     = zero
        , mouseDown    = False
        , clickEffects = []
        , tractorLoop  = 0
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
        , playerAngle  = newAngle
        , shadowPos    = newShadowPos
        , shadowVel    = newShadowVel
        , shadowAcc    = newShadowAcc
        , shadowAngle  = newShadowAngle
        , shadowAngVel = newShadowAngVel
        , shadowAngAcc = newShadowAngAcc
        , clickEffects = clickEffects >>= decrementClickEffects
        , tractorLoop  = (tractorLoop + 1) `mod` tractorLoopPeriod
        , debug        = "pos: "
                      ++ showV2 4 (round <$> newPos :: V2 Int)
                      ++ "  ||  vel: "
                      ++ showV2 5 ((round.(10000 *)) <$> newVel :: V2 Int)
                      ++ "  ||  acc: "
                      ++ showV2 3 ((round.(10000 *)) <$> newAcc :: V2 Int)
        }
    , Cmd.none
    )
    where
        updateSpeed speed acc =
            if signum newSpeed /= signum speed && speed /= 0 then
                0
            else
                newSpeed
            where
                newSpeed = acc * dt + speed

        updateVel maxSqSpeed vel acc
            | newSqSpeed <= epsilon   = zero
            | newSqSpeed > maxSqSpeed = sqrt maxSqSpeed *^ signorm newVel'
            | otherwise               = newVel'
            where
                newVel' = liftI2 updateSpeed vel acc
                newSqSpeed = quadrance newVel'

        newPos = playerVel ^* dt + playerPos

        newVel = updateVel playerMaxSqSpeed playerVel playerAcc

        newAcc = ((frictionalAcc * dt *^).negate.normalize) newVel +
            case playerStatus of
                Moving keys -> controlAcc *^
                                   (normalize.sum) (direction <$> keys)
                _           -> zero

        newAngle = normalizeAngle $ playerAngle +
            if not mouseDown then 0 else deltaAngle
            where
                towardCursor@(V2 _ ty) = mousePos - (newPos + windowCenter)
                V2 x _ = normalize towardCursor
                destAngle = acos x * signum ty
                angleDiff = normalizeAngle $ destAngle - playerAngle
                deltaAngle = signum angleDiff
                           * min
                               (minTurnVel + abs angleDiff * turnVel * dt)
                               (abs angleDiff / 2)

        newShadowAcc = ((frictionalAcc' * dt *^).negate.normalize) shadowVel +
            (springConstant *^ displacement)
            where
                displacement = playerPos - shadowPos
                frictionalAcc' = norm displacement * frictionalAcc / 24

        newShadowVel = updateVel 1e12 shadowVel newShadowAcc

        newShadowPos = newShadowVel ^* dt + shadowPos

        newShadowAngAcc = ((frictionalAcc' * dt *).negate.signum) shadowAngVel +
            (2 * springConstant * displacement)
            where
                displacement = normalizeAngle $ playerAngle - shadowAngle
                frictionalAcc' = abs displacement * frictionalAcc / 12

        newShadowAngVel = updateSpeed shadowAngVel newShadowAngAcc

        newShadowAngle = normalizeAngle $ newShadowAngVel * dt + shadowAngle

        decrementClickEffects (Active i pos)
            | i > 0     = [Active (i - 1) pos]
            | otherwise = []

update model@Model{..} (StartMove key) =
    let updating =
            case playerStatus of
                Moving keys -> key `notElem` keys
                _           -> True
    in  if updating then
            ( model
                { playerVel    = playerVel + playerImpetus *^ direction key
                , playerAcc    = newAcc
                , playerStatus = newStatus
                }
            , Cmd.none
            )
        else
            (model, Cmd.none)
    where
        newAcc = playerAcc + controlAcc *^ direction key

        newStatus = case playerStatus of
            Moving keys -> Moving $ key : keys
            _           -> Moving [key]

update model@Model{..} (StopMove key) =
    (model { playerStatus = newStatus }, Cmd.none)
    where
        newStatus = case playerStatus of
            Moving keys -> let newKeys = delete key keys
                           in  if null newKeys then Waiting else Moving newKeys
            _           -> Waiting

update model@Model{..} (MouseMove mousePos') =
    (model { mousePos = fromIntegral <$> mousePos' }, Cmd.none)

update model@Model{..} (MouseClick button clickPos) =
    if button == Mouse.LeftButton then
        ( model
            { mouseDown    = True
            , clickEffects = newClickEffects
            }
        , Cmd.none
        )
    else
        (model, Cmd.none)
    where
        samePos (Active _ pos) = pos /= clickPos
        filteredClickEffects = filter samePos clickEffects
        newClickEffects = Active 24 clickPos : filteredClickEffects

update model@Model{..} (MouseRelease button _) =
    if button == Mouse.LeftButton then
        (model { mouseDown = False }, Cmd.none)
    else
        (model, Cmd.none)

update model DoNothing = (model, Cmd.none)

subscriptions :: Sub SDLEngine Action
subscriptions = Sub.batch
    [ Time.fps 60 Animate
    , Keyboard.downs $
          \key -> if key `elem` movementKeys then StartMove key else DoNothing
    , Keyboard.ups $
          \key -> if key `elem` movementKeys then StopMove key else DoNothing
    , Mouse.moves MouseMove
    , Mouse.downs MouseClick
    , Mouse.ups MouseRelease
    ]

view :: Model -> Graphics SDLEngine
view Model{..} = Graphics2D
    $ collage
        [ centerForms (V2 xCenter 20) [text $ toText debug]
        , centerForms windowCenter
              [ rotate shadowAngle.setPos shadowPos
                  $ shadowForm
              ]
        , centerForms windowCenter
              [ rotate playerAngle.setPos playerPos
                  $ group (squareForm : maybeTractorForm)
              ]
        , group $ clickEffectForm <$> clickEffects
        ]
    where
        V2 xCenter _ = windowCenter

        clickEffectForm (Active i pos) =
            let clickEffectAlpha = 5 / 4096 * fromIntegral (i ^ (2 :: Int))
                clickEffectColor = rgba 0.75 0.75 1 clickEffectAlpha
            in  setPos (fromIntegral <$> pos)
              $ filled clickEffectColor
              $ circle (24 - fromIntegral (i ^ (2 :: Int) `div` 24))

        maybeTractorForm = [tractorForm tractorLoop | mouseDown]
