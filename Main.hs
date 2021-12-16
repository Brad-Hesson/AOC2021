import qualified Day1.Main as D1
import qualified Day2.Main as D2
import qualified Day3.Main as D3
import qualified Day4.Main as D4
import qualified Day5.Main as D5
import qualified Day6.Main as D6
import qualified Day7.Main as D7
import qualified Day8.Main as D8
import qualified Day9.Main as D9

days =
  [ (D1.main, 1),
    (D2.main, 2),
    (D3.main, 3),
    (D4.main, 4),
    (D5.main, 5),
    (D6.main, 6),
    (D7.main, 7),
    (D8.main, 8),
    (D9.main, 9)
  ]

main :: IO ()
main =
  mapM_
    ( \(f, n) -> do
        putStr "[ Day "
        putStr . show $ n
        putStrLn " ]"
        f
        putStrLn ""
    )
    days