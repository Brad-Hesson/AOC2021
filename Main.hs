import qualified Day1.Main as D1
import qualified Day10.Main as D10
import qualified Day11.Main as D11
import qualified Day12.Main as D12
import qualified Day13.Main as D13
import qualified Day14.Main as D14
import qualified Day15.Main as D15
import qualified Day16.Main as D16
import qualified Day17.Main as D17
import qualified Day18.Main as D18
import qualified Day19.Main as D19
import qualified Day2.Main as D2
import qualified Day20.Main as D20
import qualified Day21.Main as D21
import qualified Day22.Main as D22
import qualified Day3.Main as D3
import qualified Day4.Main as D4
import qualified Day5.Main as D5
import qualified Day6.Main as D6
import qualified Day7.Main as D7
import qualified Day8.Main as D8
import qualified Day9.Main as D9

days :: [(IO (), Integer)]
days =
  [ (D1.main, 1),
    (D2.main, 2),
    (D3.main, 3),
    (D4.main, 4),
    (D5.main, 5),
    (D6.main, 6),
    (D7.main, 7),
    (D8.main, 8),
    (D9.main, 9),
    (D10.main, 10),
    (D11.main, 11),
    (D12.main, 12),
    (D13.main, 13),
    (D14.main, 14),
    (D15.main, 15),
    (D16.main, 16),
    (D17.main, 17),
    (D18.main, 18),
    (D19.main, 19),
    (D20.main, 20),
    (D21.main, 21),
    (D22.main, 22)
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