module Container where

import Prelude

import ComponentA as CA
import ComponentB as CB
import ComponentC as CC
import Control.Alt ((<|>))
import Data.Map (values)
import Data.Maybe (Maybe(..))
import Data.Traversable (foldr)
import Halogen as H
import Halogen.Component.Proxy (ProxyQ, proxyTrans, queryQ)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State =
  { a :: Maybe Boolean
  , b :: Maybe Int
  , c :: Maybe String
  }

data Query a = ReadStates a

data ChildQuery a = GetState (State → a)

type ChildSlot = Int

childATransform ∷ ChildQuery ~> CA.Query
childATransform (GetState f) = CA.GetState \a → f { a: Just a, b: Nothing, c: Nothing }

childBTransform ∷ ChildQuery ~> CB.Query
childBTransform (GetState f) = CB.GetCount \b → f { a: Nothing, b: Just b, c: Nothing }

childCTransform ∷ ChildQuery ~> CC.Query
childCTransform (GetState f) = CC.GetValue \c → f { a: Nothing, b: Nothing, c: Just c }

proxyComponentA ∷ ∀ m. H.Component HH.HTML (ProxyQ ChildQuery Unit Void) Unit Void m
proxyComponentA = proxyTrans childATransform CA.component

proxyComponentB ∷ ∀ m. H.Component HH.HTML (ProxyQ ChildQuery Unit Void) Unit Void m
proxyComponentB = proxyTrans childBTransform CB.component

proxyComponentC ∷ ∀ m. H.Component HH.HTML (ProxyQ ChildQuery Unit Void) Unit Void m
proxyComponentC = proxyTrans childCTransform CC.component


component :: forall m. Applicative m => H.Component HH.HTML Query Unit Void m
component =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { a: Nothing, b: Nothing, c: Nothing }

  render :: State -> H.ParentHTML Query (ProxyQ ChildQuery Unit Void) ChildSlot m
  render state = HH.div_
    [ HH.div
        [ HP.class_ (H.ClassName "box")]
        [ HH.h1_ [ HH.text "Component A" ]
        , HH.slot 1 proxyComponentA unit absurd
        ]
    , HH.div
        [ HP.class_ (H.ClassName "box")]
        [ HH.h1_ [ HH.text "Component B" ]
        , HH.slot 2 proxyComponentB unit absurd
        ]
    , HH.div
        [ HP.class_ (H.ClassName "box")]
        [ HH.h1_ [ HH.text "Component C" ]
        , HH.slot 3 proxyComponentC unit absurd
        ]
    , HH.p_
        [ HH.text "Last observed states:"]
    , HH.ul_
        [ HH.li_ [ HH.text ("Component A: " <> show state.a) ]
        , HH.li_ [ HH.text ("Component B: " <> show state.b) ]
        , HH.li_ [ HH.text ("Component C: " <> show state.c) ]
        ]
    , HH.button
        [ HE.onClick (HE.input_ ReadStates) ]
        [ HH.text "Check states now" ]
    ]

  eval :: Query ~> H.ParentDSL State Query (ProxyQ ChildQuery Unit Void) ChildSlot Void m
  eval (ReadStates next) = do
    m ← H.queryAll (queryQ <<< H.request $ GetState)
    let res = foldr foldFn {a: Nothing, b: Nothing, c: Nothing} (values m)
    H.put res
    pure next

  foldFn {a: a1, b: b1, c: c1} {a: a2, b: b2, c: c2} = { a: a1 <|> a2, b: b1 <|> b2, c: c1 <|> c2 }
