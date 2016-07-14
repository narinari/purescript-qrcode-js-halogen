module Main where

import Prelude (class Eq, class Ord, type (~>), Unit, pure, bind, (=<<), ($), (==))
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff)

import Data.Functor.Coproduct (Coproduct())
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Util (runHalogenAff, awaitBody)

import QRCode.Types
import QRCode.Halogen.Component as QRCode

type MainEffects = H.HalogenEffects (qrcode :: QRCODE)

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

type StateP g = H.ParentState State QRCode.State Query QRCode.Query g QRCodeSlot
type QueryP = Coproduct Query (H.ChildF QRCodeSlot QRCode.Query)
type MainHTML g = H.ParentHTML QRCode.State Query QRCode.Query g QRCodeSlot
type MainAff = Aff MainEffects
type MainDSL = H.ParentDSL State QRCode.State Query QRCode.Query MainAff QRCodeSlot


ui :: H.Component (StateP MainAff) QueryP MainAff
ui = H.parentComponent { render, eval, peek: Nothing }
  where
  qrcodeConfig = defaultConfig { height = 80, width = 80 }
  render :: State -> MainHTML MainAff
  render state =
    HH.div_
      [ HH.div_
        [ HH.slot (QRCodeSlot "A")
            \_ -> { component: QRCode.component
                  , initialState: QRCode.initialState
                      { config = qrcodeConfig { text = state.textA } }
                  }
        , HH.text "QR Code"
        , HH.input
            [ HP.inputType HP.InputText
            , HP.placeholder "URL"
            , HP.value state.textA
            , HE.onValueChange $ HE.input $ UpdateText "A"
            ]
        ]
      , HH.div_
        [ HH.slot (QRCodeSlot "B")
            \_ -> { component: QRCode.component
                  , initialState: QRCode.initialState
                      { config = qrcodeConfig { text = state.textB } }
                  }
        , HH.text "QR Code"
        , HH.input
            [ HP.inputType HP.InputText
            , HP.placeholder "URL"
            , HP.value state.textB
            , HE.onValueChange $ HE.input $ UpdateText "B"
            ]
        ]
      ]
  
  eval :: Query ~> MainDSL
  eval (UpdateText slotID newText next) = do
    H.modify (\s -> if slotID == "A" then s { textA = newText } else s { textB = newText })
    H.query (QRCodeSlot slotID) $ H.action $ QRCode.SetText newText
    pure next
 
main :: Eff MainEffects Unit
main = runHalogenAff $ H.runUI ui (H.parentState initialState) =<< awaitBody
