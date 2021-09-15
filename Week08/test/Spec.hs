module Main
    ( main
    ) where

import qualified Spec.Model
import qualified Spec.Trace
import qualified Spec.HWModel
import qualified Spec.HWTrace
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "token sale"
    [ Spec.Trace.tests
    , Spec.Model.tests
    , Spec.HWTrace.tests
    , Spec.HWModel.tests
    ]
