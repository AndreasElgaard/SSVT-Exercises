import Lecture3

--contradiction 
contradiction :: Form -> Bool
contradiction f = not (any (\ v -> evl v f) (allVals f))

-- if any (\ v -> evl v f) (allVals f) then False else True

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

    -- | logical entailment 
entails :: Form -> Form -> Bool
entails p q 
    | p == False = True
    | p == True && q == True = True
    | otherwise False

    -- | logical equivalence
equiv :: Form -> Form -> Bool
equiv p q 
    | p == True && q == True = True
    | p == False && q == False = True
    | otherwise False
