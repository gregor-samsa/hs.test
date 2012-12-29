{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}


{-- TODO
  o do a traversable example
  o do it with a gadt
  o conquer world
--}

{-- GADT TEST: 
    from "Fun with Phantom Types" - http://www.cs.ox.ac.uk/ralf.hinze/publications/With.pdf
    code translated to gadt syntax 
      @ https://github.com/cutsea110/fun-of-phantom-type/blob/master/PhantomType.hs 
--}
type Name = String
type Age = Int
data Person = Person Name Age deriving Show

rString :: Type String
rString = RList RChar

data Type t where
  RInt :: Type Int
  RChar :: Type Char
  RList :: Show a => Type a -> Type [a]
  RPair :: (Show a, Show b) => Type a -> Type b -> Type (a, b)
  RPerson :: Type Person
  RDyn :: Type Dynamic
  RFunc :: Type a -> Type b -> Type (a -> b)

data Dynamic = forall t. Show t => Dyn (Type t) t

type Traversal = forall t. Type t -> t -> t
type Query s = forall t. Type t -> t -> s

tick :: Name -> Traversal
tick s (RPerson) (Person n a) | s == n = Person n (a + 1)
tick s rt t = t

copy :: Traversal
copy rt = id

(<*>) :: Traversal -> Traversal -> Traversal
(f <*> g) rt = f rt . g rt

imap :: Traversal -> Traversal
imap f (RInt) i = i
imap f (RChar) c = c
imap f (RList ra) [] = []
imap f (RList ra) (a:as) = f ra a:f (RList ra) as
imap f (RPair ra rb) (a, b) = (f ra a, f rb b)
imap f (RPerson) (Person n a) = Person (f rString n) (f RInt a)

everywhere, everywhere' :: Traversal -> Traversal
everywhere f = f <*> imap (everywhere f)
everywhere' f = imap (everywhere' f) <*> f

age :: Query Age
age (RPerson) (Person n a) = a
age _ _ = 0

sizeof :: Query Int
sizeof (RInt) _ = 2
sizeof (RChar) _ = 2
sizeof (RList ra) [] = 0
sizeof (RList ra) (_:_) = 3
sizeof (RPair ra rb) _ = 3
sizeof (RPerson) _ = 3

isum :: Query Int -> Query Int
isum f (RInt) i = 0
isum f (RChar) c = 0
isum f (RList ra) [] = 0
isum f (RList ra) (a:as) = f ra a + f (RList ra) as
isum f (RPair ra rb) (a, b) = f ra a + f rb b
isum f (RPerson ) (Person n a) = f rString n + f RInt a

total :: Query Int -> Query Int
total f rt t = f rt t + isum (total f) rt t

ps = [Person "Norma" 50, Person "Richard" 59]
ps'= everywhere (tick "Richard") (RList RPerson) ps
ta = total age (RList RPerson) ps'

{-- End TEST --}

-- Must get this to work
data Date
data Prop

instance Show Prop where
    show a = "Prop"

data Expr a where
    I          :: Int  -> Expr Int
    B          :: Bool -> Expr Bool
    EChar      :: Char -> Expr Char
    PpRotation :: Prop -> Expr Int -> Expr Int
    PpDate     :: Prop -> String -> Expr Prop
    EList      :: [a] -> Expr [a]
    Pair       :: Expr a -> Expr b -> Expr (a, b)

-- a standard list (homogeneous type)
data SList a where
    SNil   :: SList a
    SCons, (:@) :: a -> SList a -> SList a 

-- a heterogeneous list
data HList as where
  Nil  :: HList ()
  Cons, (:*:) :: a -> HList as -> HList (a, as)

infixr 5 :@ 
infixr 5 :*: 

-- test the hlist
sl = 1 :@ 1 :@ SNil
hl = "HI" :*: 1 :*: Nil

-- basics
reString :: Expr String
reString = EList "HELLO"

rPropList' :: Expr [Expr Int]
rPropList' = EList [I 3, I 4] -- <- homogeneous

-- what do I do with this guy?!?
-- rPropList :: Expr [Expr Prop]
-- rPropList = EList [PpDate (undefined::Prop) "1.1.2012", PpRotation (undefined::Prop) (I 3)]

eval :: Expr a -> a
eval (I i) 	   = i
eval (B t)     = t
eval (EChar c) = c
eval (PpRotation p r) = eval r
eval (Pair a b) = (eval a, eval b)
{--
eval (PpRotation p r) = p
eval (PpDate p d) = d
--}


-- similar to above... [AnyExpr (I 3), AnyExpr (B True)]...
data AnyExpr where 
    AnyExpr :: Expr a -> AnyExpr


-- existential type - i can show it
data TG3 where
  MkTG3 :: Show a => a -> TG3

instance Show TG3 where
  show (MkTG3 s) = show s     

xs :: [TG3]
xs = [MkTG3 5, MkTG3 'Z', MkTG3 3.14, MkTG3 False]

{--
--}

main = print "Hello World!"
