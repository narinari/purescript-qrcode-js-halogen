module Main where

import QRCode.Types
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import QRCode.Halogen.Component as QRCode
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Data.Void (Void, absurd)
import Halogen.Aff (HalogenEffects, runHalogenAff, awaitBody)
import Halogen.VDom.Driver (runUI)
import Prelude (class Eq, class Ord, type (~>), Unit, unit, const, (=<<), ($), (==), ($>), (*>))

type MainEffects = HalogenEffects (qrcode :: QRCODE)

data Query a
  = UpdateText String String a

type State =
  { textA :: String
  , textB :: String
  }

initialState :: State
initialState =
  { textA: "http://example.com"
  , textB: "http://purescript.org"
  }

data QRCodeSlot = QRCodeSlot String

derive instance qrcodeSlotOrd :: Ord QRCodeSlot
derive instance qrcodeSlotEq :: Eq QRCodeSlot

type MainHTML g = H.ParentHTML Query QRCode.Query QRCodeSlot g
type MainAff = Aff MainEffects
type MainDSL g = H.ParentDSL State Query QRCode.Query QRCodeSlot Void g


ui :: H.Component HH.HTML Query Unit Void MainAff
ui = H.parentComponent { initialState: const initialState, render, eval, receiver: const Nothing }
  where
  qrcodeConfig = defaultConfig { height = 80, width = 80 }
  render :: State -> MainHTML MainAff
  render state =
    HH.div_
      [ HH.div_
        [ HH.slot (QRCodeSlot "A") (QRCode.component Nothing) Nothing absurd
        , HH.text "QR Code"
        , HH.input
            [ HP.type_ HP.InputText
            , HP.placeholder "URL"
            , HP.value state.textA
            , HE.onValueChange $ HE.input $ UpdateText "A"
            ]
        ]
      , HH.div_
        [ HH.slot (QRCodeSlot "B") (QRCode.component $ Just qrcodeConfig) (Just state.textB) absurd
        , HH.text "QR Code"
        , HH.input
            [ HP.type_ HP.InputText
            , HP.placeholder "URL"
            , HP.value state.textB
            , HE.onValueChange $ HE.input $ UpdateText "B"
            ]
        ]
      ]

  eval :: Query ~> MainDSL MainAff
  eval (UpdateText slotID newText next) =
    H.modify (\s -> if slotID == "A" then s { textA = newText } else s { textB = newText })
    *> H.query (QRCodeSlot slotID) (H.action $ QRCode.SetText $ Just newText)
    $> next

main :: Eff MainEffects Unit
main = runHalogenAff $ runUI ui unit =<< awaitBody
