module Exercise6 where

import Prelude

data Boy = Matthew | Peter | Jack | Arnold | Carl deriving (Eq, Show)

boys :: [Boy]
boys = [Matthew, Peter, Jack, Arnold, Carl]

-- accuses: encodes whether a boy's statement is True GIVEN a hypothesized thief.
-- Matthew: "Carl didn't do it, and neither did I." -> thief is not Carl and not Matthew.
-- Peter: "It was Matthew or it was Jack." -> thief is Matthew or Jack.
-- Jack: "Matthew and Peter are both lying." -> Matthew's statement is false AND Peter's statement is false.
-- Arnold: "Matthew or Peter is speaking the truth, but not both." -> Exclusive OR of Matthew and Peter.
-- Carl: "What Arnold says is not true." -> Arnold's statement is false.
accuses :: Boy -> Boy -> Bool
accuses Matthew thief = thief /= Carl && thief /= Matthew
accuses Peter   thief = thief == Matthew || thief == Jack
accuses Jack    thief = not (accuses Matthew thief) && not (accuses Peter thief)
accuses Arnold  thief = accuses Matthew thief /= accuses Peter thief
accuses Carl    thief = not (accuses Arnold thief)

-- accusers: Returns the list of boys whose statements evaluate to True, 
-- assuming the provided 'thief' is the actual guilty party.
accusers :: Boy -> [Boy]
accusers thief = [b | b <- boys, accuses b thief]

-- guilty: Computes the actual thief. 
-- The class teacher states exactly 3 boys tell the truth. Therefore, the guilty boy 
-- is the one whose hypothetical guilt results in exactly 3 'accusers' (truth-tellers).
guilty :: [Boy]
guilty = [thief | thief <- boys, length (accusers thief) == 3]

-- honest: Computes the boys who made honest declarations.
-- It evaluates the accusers function against the resolved guilty party.
honest :: [Boy]
honest = concatMap accusers guilty

main :: IO ()
main = do
    putStrLn $ "The guilty boy is: " ++ show guilty
    putStrLn $ "The honest boys are: " ++ show honest