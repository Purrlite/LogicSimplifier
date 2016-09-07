{-# LANGUAGE  GADTSyntax #-}
{-# LANGUAGE  DeriveFunctor #-}
{-# LANGUAGE  PatternSynonyms #-}

module LogicSimplifier (
-- * Type definition and Patterns
    Logic(..),
    pattern T,
    pattern F,
    pattern Var,
    pattern Not,
    pattern AND,
    pattern OR,
    pattern Op,
    
-- * And/Or
    (-&-),
    (-|-),
    
-- * Printing
    toPrettyPrint,
    prettyPrint,
    
-- * Reshapers
    buReshape,
    tdReshape,
    tdReshape',
    
-- * Simplifying functions
    or_ify,
    and_ify,
    merge,
    moveNegationsInside,
    removeDoubleNegations,
    minimizeNegations,
    simplify,
) where


import Data.Monoid ((<>))
import Data.Char
import Data.Fix
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as PPrint


data Logic' rec where
    True'  :: Logic' rec
    
    False' :: Logic' rec

    Var'   :: String
           -> Logic' rec

    Not'   :: rec
           -> Logic' rec

    And'   :: NonEmpty rec
           -> Logic' rec

    Or'    :: NonEmpty rec
           -> Logic' rec

    Op'    :: String
           -> (Bool -> Bool -> Bool)
           -> rec
           -> rec
           -> Logic' rec
    deriving (Functor)


instance Eq rec => Eq (Logic' rec) where
    True'        == True'        = True
    False'       == False'       = True
    (Var' name1) == (Var' name2) = name1 == name2
    (Not' rec1)  == (Not' rec2)  = rec1 == rec2
    (And' l1)    == (And' l2)    = l1 == l2
    (Or' l1)     == (Or' l2)     = l1 == l2
    (Op' name1 f1 r1 r'1) == (Op' name2 f2 r2 r'2)
        =  name1 == name2 
        && are2BoolFunsSame f1 f2
        && r1 == r'1
        && r2 == r'2
    _ == _ = False


instance Show rec => Show (Logic' rec) where
    show True'              = "True'"
    show False'             = "False'"
    show (Var' name1)       = "Var' " ++ name1
    show (Not' rec)         = "Neg' " ++ show rec
    show (And' l)           = "And' " ++ show l
    show (Or' l)            = "Or' " ++ show l
    show (Op' name f r1 r2) = "Op' " ++ name
                            ++ show (fmap (uncurry f)  possible2BoolArgs)
                            ++ " " ++ show r1
                            ++ " " ++ show r2


-- | Checks if two (Bool -> Bool -> Bool) functions are the same.
are2BoolFunsSame :: (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> Bool
are2BoolFunsSame f g = and $ map (distributeFun (==) f' g') possible2BoolArgs
  where
    f' = uncurry f
    g' = uncurry g
    distributeFun comb f g x = f x `comb` g x


possible2BoolArgs :: [(Bool, Bool)]
possible2BoolArgs = [(False, False), (False, True), (True, False), (True, True)]


type Logic = Fix Logic'


pattern T :: Logic
pattern T = Fix True'

pattern F :: Logic
pattern F = Fix False'

pattern Var :: String -> Logic
pattern Var name = Fix (Var' name)

pattern Not :: Logic -> Logic
pattern Not x = Fix (Not' x)

pattern AND :: NonEmpty Logic -> Logic
pattern AND l = Fix (And' l)

pattern OR :: NonEmpty Logic -> Logic
pattern OR l = Fix (Or' l)

pattern Op :: String -> (Bool -> Bool -> Bool) -> Logic -> Logic -> Logic
pattern Op name fun x y = Fix (Op' name fun x y)


-- | _And_ between 2 logical expressions
(-&-) :: Logic -> Logic -> Logic
x -&- y = Fix (And' (x :| y : []))

-- | _Or_ between 2 logical expressions
(-|-) :: Logic -> Logic -> Logic
x -|- y = Fix (Or' (x :| y : []))


-- | Takes a function that can change the layout of Fix f, makes it recursive 
-- and applies it from the bottom up on Fix f
buReshape :: (Functor f) => (Fix f -> Fix f) -> Fix f -> Fix f
buReshape fn = fn . Fix . fmap (buReshape fn) . unFix


-- | Takes a function that can change the layout of Fix f, makes it recursive 
-- and applies it from the top down on Fix f
-- 
-- The Maybe in the return value of the function is there so that the function 
-- itself can decide when to descend down the structure of Fix f and when to 
-- try applying itself to the last result.
tdReshape :: (Functor f) => (Fix f -> Maybe (Fix f)) -> Fix f -> Fix f
tdReshape fn x = case fn x of
                   Nothing -> Fix . fmap (tdReshape fn) . unFix $ x
                   Just y  -> tdReshape fn y


-- | Same as tdReshape but instead uses Either instead of Maybe.
--
-- Left x signifies that the function should apply itself to the last result
-- - the x that it returned.
--
-- Right x signifies that the function should descend down the structure of x
-- before applying itself again.
tdReshape' :: (Functor f) => (Fix f -> Either (Fix f) (Fix f)) -> Fix f -> Fix f
tdReshape' fn x = case fn x of
                    Left y  -> Fix . fmap (tdReshape' fn) . unFix $ y
                    Right y -> tdReshape' fn y


toPrettyPrint' :: Logic' Doc -> Doc
toPrettyPrint' True' 
    = PPrint.char 'T'
toPrettyPrint' False'
    = PPrint.char 'F'
toPrettyPrint' (Not' x)
    = PPrint.char '¬' <> x
toPrettyPrint' (Var' name)
    = PPrint.text $ map toLower name
toPrettyPrint' (And' l)
    = PPrint.parens ( PPrint.hcat $ NonEmpty.toList 
                    $ NonEmpty.intersperse (PPrint.text " ∧ ") l)
toPrettyPrint' (Or' l)
    = PPrint.parens ( PPrint.hcat $ NonEmpty.toList
                    $ NonEmpty.intersperse (PPrint.text " ∨ ") l)
toPrettyPrint' (Op' name _ x y)
    = PPrint.parens (x <> PPrint.space <> PPrint.text (map toLower name) 
                     <> PPrint.space <> y)

-- | Creates a nice Doc from Logic
toPrettyPrint :: Logic -> Doc
toPrettyPrint = cata toPrettyPrint'


-- | Nicely prints out the value of Logic
prettyPrint :: Logic -> IO ()
prettyPrint = putStrLn . PPrint.render . toPrettyPrint


removeDoubleNot :: Logic -> Logic
removeDoubleNot (Not (Not x)) = x
removeDoubleNot x             = x

-- | Removes all double negations in a logical expression
removeDoubleNegations :: Logic -> Logic
removeDoubleNegations = buReshape removeDoubleNot


or_ify' :: Logic -> Logic
or_ify' (AND l) = OR $ fmap Not l
or_ify' x       = x

-- | Changes all logical ANDs to logical ORs
or_ify :: Logic -> Logic
or_ify = buReshape or_ify'


and_ify' :: Logic -> Logic
and_ify' (OR l) = AND $ fmap Not l
and_ify' x      = x

-- | Changes all logical ORs to logical ANDs
and_ify :: Logic -> Logic
and_ify = buReshape and_ify'


merge' :: Logic -> Logic
merge' (AND list) = AND $ NonEmpty.fromList $ notAnds ++ concat andLists
    where (ands, notAnds) = NonEmpty.partition isAND list
          
          isAND (AND _) = True
          isAND _       = False
          
          andLists = map (NonEmpty.toList . getAndList) ands
          getAndList (AND l) = l
          
merge' (OR  list) = OR $ NonEmpty.fromList $ notOrs ++ concat orLists
    where (ors,  notOrs)  = NonEmpty.partition isOR list
          
          isOR (OR _) = True
          isOR _      = False
          
          orLists = map (NonEmpty.toList . getOrList) ors
          getOrList (OR l) = l

merge' x = x

-- | Merges logical ANDs and ORs in the structure of Logic to make it take 
-- less space
merge :: Logic -> Logic
merge = buReshape merge' . removeDoubleNegations


moveNegationsInside' :: Logic -> Maybe Logic
moveNegationsInside' (Not (AND l))
    = Just $ OR $ fmap Not l
moveNegationsInside' (Not (OR  l))
    = Just $ AND $ fmap Not l
moveNegationsInside' (Not (Op name f x y))
    = Just $ Op name (\a b -> not $ f a b) x y
moveNegationsInside' _
    = Nothing

-- | Puts negations as much inside the structure of the logical expression 
-- as it can
moveNegationsInside :: Logic -> Logic
moveNegationsInside = tdReshape moveNegationsInside'


-- | Minimizes the amount of negations in a given logical expression
minimizeNegations :: Logic -> Logic
minimizeNegations
  = removeDoubleNegations . moveNegationsInside . removeDoubleNegations


simplify' :: Logic -> Logic
simplify' (Not T) = F
simplify' (Not F) = T
simplify' (AND l)
    | F `elem` l = F
    | otherwise  = case NonEmpty.nub l of
                     T :| [] -> T
                     F :| [] -> F
                     nubbedL -> AND $ NonEmpty.fromList
                                    $ NonEmpty.filter (/= T) nubbedL
simplify' (OR l)
    | T `elem` l = T
    | otherwise  = case NonEmpty.nub l of
                     T :| [] -> T
                     F :| [] -> F
                     nubbedL -> OR $ NonEmpty.fromList
                                   $ NonEmpty.filter (/= F) nubbedL
simplify' x = x

-- | Simplifies logical expression using few simple rules.
-- 
-- Best used after other simplification functions.
simplify :: Logic -> Logic
simplify = buReshape simplify'
