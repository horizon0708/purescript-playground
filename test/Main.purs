module Test.Main where

import Data.Array
import Data.Position
import Data.Tuple
import Prelude

import Effect (Effect)
import Test.Unit (suite, test, timeout)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
    suite "Gradient filtering" do
        test "passing test"
            $ Assert.equal 1 1
        -- test "1 1 should be empty" 
        --     $ Assert.equal [] 
        --     $ filterByGradient UpDiagonal [ Tuple 2 3 ]
        -- test "1 1 should not be empty"
        --     $ Assert.equal [ Tuple 2 2, Tuple 3 3, Tuple (-1) (-1) ]
        --     $ filterByGradient UpDiagonal [ Tuple 2 2, Tuple 3 3, Tuple (-1) (-1)]
        -- test "-1 1"
        --     $ Assert.equal []
        --     $ filterByGradient DownDiagonal [ Tuple 2 (-2) ] 