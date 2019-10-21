{-# OPTIONS_GHC -F -pgmF htfpp #-}

import           Lib
import           Test.Framework

main = htfMain htf_thisModulesTests

test_not_matchesId = do
  assertEqual False $ matchesId 1 jenny

test_matchesId = do
  assertEqual True $ matchesId 2 jenny
