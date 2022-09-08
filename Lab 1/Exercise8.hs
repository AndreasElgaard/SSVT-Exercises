module Exercise8 where
import Data.List
import Test.QuickCheck    


data Boy = Matthew | Peter | Jack | Arnold | Carl
    deriving (Eq,Show)
    
boys :: [Boy]
boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses ::  Boy -> Boy -> Bool
accuses Peter Matthew = True
accuses Peter Jack = True
accuses Jack Peter = True
accuses Jack Matthew = True
accuses Arnold Matthew = True
accuses Arnold Peter = True
accuses Carl Arnold = True
accuses _ _ = False

accusers ::  Boy -> [Boy]
accusers Matthew = [Peter, Jack, Arnold]
accusers Jack = [Peter]
accusers Peter = [Jack]
accusers Arnold = [Carl]
accusers Carl = []