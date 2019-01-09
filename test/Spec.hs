import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.Maybe       (catMaybes)

import           Nump             (rename)

main :: IO ()
main = defaultMain tests

-- tests
tests :: TestTree
tests =
  testGroup
    "Nump"
    [ testGroup
        "rename"
        [ testCase
            "03 -> 04 (n = 03)"
            (assertEqual
               "Bumps each number by 1"
               (Just ("03-blah.md", "04-blah.md"))
               (rename 03 "03-blah.md"))
        , testCase
            "03 -> 04 (n = 05)"
            (assertEqual "Nothing" Nothing (rename 05 "03-blah.md"))
        , testCase
            "04 -> 05 (n = 03)"
            (assertEqual
               "Bumps each number by 1"
               (Just ("04-blah.md", "05-blah.md"))
               (rename 03 "04-blah.md"))
        , testCase
            "09 -> 10 (n = 03)"
            (assertEqual
               "Bumps each number by 1"
               (Just ("09-blah.md", "10-blah.md"))
               (rename 03 "09-blah.md"))
        , testCase
            "22 -> 23 (n = 03)"
            (assertEqual
               "Bumps each number by 1"
               (Just ("22-blah.md", "23-blah.md"))
               (rename 03 "22-blah.md"))
        , testCase
            "Non-Numeric Filename"
            (assertEqual "Nothing" Nothing (rename 05 "blah.md"))
        ]
    ]
