import Prelude

data Boy = Matthew | Peter | Jack | Arnold | Carl deriving (Eq, Show)

boys :: [Boy]
boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses :: Boy -> Boy -> Bool
accuses Matthew thief = thief /= Carl && thief /= Matthew
accuses Peter thief = thief == Matthew || thief == Jack
accuses Jack thief = not (accuses Matthew thief) && not (accuses Peter thief)
accuses Arnold thief = accuses Matthew thief /= accuses Peter thief
accuses Carl thief = not (accuses Arnold thief)

accusers :: Boy -> [Boy]
accusers thief = [b | b <- boys, accuses b thief]

guilty :: [Boy]
guilty = [thief | thief <- boys, length (accusers thief) == 3]

honest :: [Boy]
honest = concatMap accusers guilty

main :: IO ()
main = do
    putStrLn $ "The guilty boy is: " ++ show guilty
    putStrLn $ "The honest boys are: " ++ show honest