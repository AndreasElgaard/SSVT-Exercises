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
jackStatement = Jack `accuses` Matthew && Jack `accuses` Peter
arnoldStatement = Arnold `accuses` Matthew /= Arnold `accuses` Peter
carlStatement = Carl `accuses` Arnold



-- honest ::  [Boy]
-- honest = matthewStatement && peterStatement 