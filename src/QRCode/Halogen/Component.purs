module QRCode.Halogen.Component where

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Control.Monad.Aff.Class (class MonadAff)
import DOM.HTML.Types (HTMLElement)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Traversable (traverse_)
import Data.Void (Void)
import Prelude (type (~>), const, pure, bind, unit, (<*>), (<$>), ($))
import QRCode (Config, QRCode, QRCODE, makeCode, mkQRCodeNode, defaultConfig)

type QREffects eff = (qrcode :: QRCODE | eff)

type State =
  { element :: Maybe HTMLElement
  , qrcode :: Maybe QRCode
  , config :: Config
  }

data Query a
  = Init a
  | SetText Input a

type Input = Maybe String

initialState :: Config -> Input -> State
initialState config text =
  { element: Nothing
  , qrcode: Nothing
  , config: maybe config (\t -> config { text = t }) text
  }

component
  :: forall m eff
  . ( MonadAff (QREffects eff) m
    )
  => Maybe Config -> H.Component HH.HTML Query Input Void m
component cfg = H.lifecycleComponent
  { initialState: initialState (fromMaybe defaultConfig cfg)
  , render
  , eval
  , receiver: const Nothing
  , initializer: Just (H.action Init)
  , finalizer: Nothing
  }

  where
  render :: State -> H.ComponentHTML Query
  render = const $ HH.div [ HP.ref (H.RefLabel "qrcode") ] []

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval (Init next) = do
    elm <- H.getHTMLElementRef (H.RefLabel "qrcode")
    H.modify _ { element = elm }
    config <- H.gets _.config
    case elm of
      Nothing -> pure unit
      Just el' -> do
        qrcode <- H.liftEff $ mkQRCodeNode el' config
        H.modify (_ { qrcode = Just qrcode })
        pure unit
    pure next
  eval (SetText content next) = do
    H.modify (\state -> state { config = state.config { text = fromMaybe "" content }})
    qrcode <- H.gets _.qrcode
    traverse_ H.liftEff $ makeCode <$> content <*> qrcode
    pure next
