import Lecture3

--contradiction 
contradiction :: Form -> Bool
contradiction = not . satisfiable 

-- if any (\ v -> evl v f) (allVals f) then False else True
-- not (any (\ v -> evl v f) (allVals f))

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

    -- | logical entailment 
entails :: Form -> Form -> Bool
entails p q 
    | p Equiv False = True
    | p Equiv True && q == True = True
    | otherwise False

    -- | logical equivalence
equiv :: Form -> Form -> Bool
equiv p q 
    | p Equiv True && q == True = True
    | p Equiv False && q == False = True
    | otherwise False
