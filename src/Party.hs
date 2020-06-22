module Party where
import Employee
import Data.Tree
import Data.List

instance Semigroup GuestList where
    (<>) (GL g1 f1) (GL g2 f2) = GL (g1 ++ g2) (f1 + f2)

instance Monoid GuestList where
    mempty = GL [] 0

glCons :: Employee -> GuestList -> GuestList
glCons e (GL g f)= GL (e:g) (f + empFun e)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold ::  (a -> [b] -> b) -> Tree a -> b
treeFold f (Node a fs) = f a $ map (treeFold f) fs

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e gs = (glCons e withBosses, moreFun withBosses withoutBosses) where
    withBosses =  foldMap snd gs
    withoutBosses = foldMap fst gs

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel


main :: IO ()
main = readFile "company.txt" >>= (putStr . print . maxFun . read ) where
    print (GL es f) = (unlines . summary . sort . map empName) es
        where summary = (++) ["Total fun: "  ++ show f]
