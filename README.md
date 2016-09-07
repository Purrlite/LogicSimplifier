Logic simplifier
================

Just a logical expression simplifier I wrote out of curiosity.

I was mainly curious if I could make a function that would make another function recursive and then apply it on Fix f to change its structure. I also wanted such function to allow me to write my to-be-recursive functions without concerning themselves about the patterns it didn't care about.

So instead of something like:

    removeDoubleNegations (Not (Not x)) = fmap removeDoubleNegations x
    removeDoubleNegations (Not (Var x)) = Not $ Var x
    removeDoubleNegations (Not (AND l)) = Not $ AND $ fmap removeDoubleNegations l
    removeDoubleNegations (Not (OR  l)) = Not $ OR $ fmap removeDoubleNegations l
    ...

I wanted to write something simple like:

    removeDoubleNegations (Not (Not x)) = x
    removeDoubleNegations x             = x

and funnily enough there is already something similar. That is cata(morphism) that you can find in the Data.Fix library. I took it, slightly altered it and voila. I got the function I wanted, that I call buReshape (bottom up Reshape).
