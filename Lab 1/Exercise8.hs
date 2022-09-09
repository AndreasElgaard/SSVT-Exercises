module Exercise8 where
import Data.List
import Test.QuickCheck    

data Boy = Matthew | Peter | Jack | Arnold | Carl
    deriving (Eq,Show)
    
boys :: [Boy]
boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses ::  Boy -> Boy -> Bool
Peter `accuses` Matthew = True
Peter `accuses` Jack = True
Jack `accuses` Matthew = True
Jack `accuses` Peter = True
Arnold `accuses` Matthew = True
Arnold `accuses` Peter = True
Carl `accuses` Arnold = True
_ `accuses` _ = False

accusers ::  Boy -> [Boy]
accusers Matthew = [Peter, Jack, Arnold]
accusers Jack = [Peter]
accusers Peter = [Jack]
accusers Arnold = [Carl]
accusers Carl = []

matthewStatement = not (Matthew `accuses` Carl || Matthew `accuses` Matthew) --DeMorgan's law
peterStatement = Peter `accuses` Matthew || Peter `accuses` Jack
jackStatement = not matthewStatement && not peterStatement
arnoldStatement = matthewStatement /= peterStatement
carlStatement = not arnoldStatement

-- Assuming that the statements provided by Jack and Arnold
-- have contradictions within, we can deduct that their statements
-- are false and hence Jack and Arnold prove to be not honest.

statements :: [(Boy, Bool)]
statements = [(Matthew, matthewStatement), (Peter, peterStatement), (Jack, jackStatement), (Arnold, arnoldStatement), (Carl, carlStatement)]

-- guilty, honest ::  [Boy]


-- honest ::  [Boy]
-- honest = matthewStatement && peterStatement 