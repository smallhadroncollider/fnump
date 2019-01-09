import           Test.Tasty
import           Test.Tasty.HUnit

import           Nump             (filter')

main :: IO ()
main = defaultMain tests

-- tests
tests :: TestTree
tests =
  testCase
    "filter"
    (assertEqual
       "Returns just bigger than numbered paths"
       ["03-blah.md", "04-blah.md", "05-blah.md", "06-blah.md"]
       (filter'
          03
          [ "01-blah.md"
          , "02-blah.md"
          , "03-blah.md"
          , "04-blah.md"
          , "05-blah.md"
          , "06-blah.md"
          , "."
          , ".."
          ]))
