
--Invalid IOLTS by definition:

--  Def 1. The list of states and list of labels should be countable (since quickcheck is infinite, a take should be necessary)
--  Def 1. List of states non empty
--  Def 1. [LabeledTransition] states are part of the list of states (If you create from [LabelTrans] a LTS, it should be the same as the lts where we obtaiend the [LabelTrans])
--  Def 1. Last state in LTS is initial state
--  Def 1. Tau constraints. I am not sure how to deal with them

--  Def 2. A with a set size. Any subsets of A, if we compose them, they are a concatenation
--  

--  Def 3. For any two states in [State], in [LabelledTransitions]:
--      -  there should be both these states, and their trace in [LabelTrans]
--  Def 3. For several states in [State], in [LabelledTransitions]:
--      - Exist all the states and they have traces between them
--      - If we have only the first state and go through all the traces, we should reach the final state
--      - If we start on first state, but miss any trace, we should not reach final state q'


--  Def 4. 