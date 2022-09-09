module Exercise8 where
import Data.List 

data Boy = Matthew | Peter | Jack | Arnold | Carl
    deriving (Eq,Show)
    
boys :: [Boy]
boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses ::  Boy -> Boy -> Bool
Peter `accuses` Matthew = True
Peter `accuses` Jack = True
Matthew `accuses` Matthew = False
Matthew `accuses` Carl = False
_ `accuses` _ = False

accusers ::  Boy -> [Boy]
accusers Matthew = [Peter, Jack, Arnold]
accusers Peter = [Jack]
accusers Jack = [Peter]
accusers Arnold = [Carl]
accusers Carl = []

statement :: Boy -> Bool
statement Matthew = not (Matthew `accuses` Carl || Matthew `accuses` Matthew) --DeMorgan's law
statement Peter = Peter `accuses` Matthew || Peter `accuses` Jack
statement Jack = not (statement Matthew) && not (statement Peter)
statement Arnold = statement Matthew /= statement Peter
statement Carl = not (statement Arnold)

statementWBoy :: Boy -> (Boy, Bool)
statementWBoy b = (b, statement b)

-- Assuming that the statements provided by Jack and Arnold
-- have contradictions within, we can deduct that their statements
-- are false and hence Jack and Arnold prove to be not honest.

isHonest :: [(a, Bool)] -> [a] -> [a]
isHonest [] outputArray = outputArray 
isHonest ((boy, True):xs) outputArray = isHonest xs (boy : outputArray)
isHonest ((_, False):xs)  outputArray = isHonest xs outputArray 

honest ::  [Boy]
honest = isHonest tuplesArr []
    where tuplesArr = map statementWBoy boys 

guilty :: [Boy]
guilty = boys 
-- We know that the honest boys -> [Carl,Peter,Matthew]
-- Therefore the liars are -> [Arnold, Jack]
-- By checking the statements we see that Jack accuses Matthew's and Peter's statement, but they are telling the truth
--      Matthews statement says that it was neither Matthew nor Carl
--      Peters statement says it was either Matthew or Jack, by the premise that Matthew's statement is honest, Jack is the thief
