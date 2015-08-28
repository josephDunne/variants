{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Lib

data Const e = Const Int
data Sum e = Plus e e
data Product e = Times e e
data Minus e = Minus e e

evalConst   (Const x) r   = x
evalSum     (Plus x y) r  = r x + r y

mkConst e = In (Inl (Const e))
mkPlus e f = In (Inr (Plus e f))

eval' = cases (evalConst ? evalSum)

evalProduct (Times x y) r = r x * r y

mkProduct e f = In (Inr (Times e f))

eval'' = cases (evalProduct ? (evalConst ? evalSum))



-- construct an effect as a fix point of e

-- (?) :: (g e -> r) -> (OutOf (Minus f g) e -> r) -> f e -> r   --- take an evaluator of expressions e of  type g, take a second evaluator fo type f Minus g, returns an evaluator
                                                                                            -- i.e a function from the fix point of the epression to the return type.

-- (t1 (Fix t1) -> (Fix t1 -> t) -> t) ->  Fix t1 -> t             cases
--                                         Fix t1 -> t             cases (m ? n)
--                                         Fix e -> IO ()
--runEffect :: 'Effect' m r -> m r

main = do
    let x = eval' (mkPlus (mkConst 1) (mkConst 2))
        y = eval'' (mkProduct (mkConst 3) (mkPlus (mkConst 1) (mkConst 2)))
    print x
    print y
    return ()
