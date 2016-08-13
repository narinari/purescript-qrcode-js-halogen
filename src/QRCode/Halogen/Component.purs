module QRCode.Halogen.Component where

import Prelude (type (~>), pure, void, unit, bind, const, (<$>), (<<<), ($), ($>))

import QRCode (Config, QRCode, QRCODE, makeCode, mkQRCodeNode, defaultConfig)

import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..), maybe)

import DOM.HTML.Types (HTMLElement)

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

type QREffects eff = (qrcode :: QRCODE | eff)

type State =
  { element :: Maybe HTMLElement
  , qrcode :: Maybe QRCode
  , config :: Config
  }

data Query a
  = SetElement (Maybe HTMLElement) a
  | Init a
  | SetText String a

initialState :: State
initialState = 
  { element: Nothing
  , qrcode: Nothing
  , config: defaultConfig
  }

component
  :: forall eff
  .  H.Component State Query (Aff (QREffects eff))
component = H.lifecycleComponent
  { render
  , eval
  , initializer: Just (H.action Init)
  , finalizer: Nothing
  }
  where
  render :: State -> H.ComponentHTML Query
  render = const $ HH.div [ HP.ref (\elm -> H.action (SetElement elm)) ] []

  eval :: Query ~> H.ComponentDSL State Query (Aff (QREffects eff))
  eval (SetElement elm next) = H.modify (_ { element = elm}) $> next
  eval (Init next) = do
    elm <- H.gets _.element
    config <- H.gets _.config
    case elm of
      Nothing -> pure unit
      Just el' -> do
        qrcode <- H.fromEff $ mkQRCodeNode el' config
        H.modify (_ { qrcode = Just qrcode })
        pure unit
    pure next
  eval (SetText content next) = do
    H.modify (\state -> state { config = state.config { text = content }})
    qrcode <- H.gets _.qrcode
    maybe (pure unit) (void) $ H.fromEff <<< makeCode content <$> qrcode
    pure next
