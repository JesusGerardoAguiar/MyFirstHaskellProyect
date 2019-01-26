module Entregable where
--Jesus Aguiar 216242


--ejercicio1
type Conj a = [a]

prim :: Conj Int
prim = [2,3,5,7,11]


pert :: Eq a => a -> Conj a -> Bool
pert = \x l-> case l of{[]->False;
                        z:zs-> case z==x of{True->True; False -> pert x zs}}

esConj :: Eq a => [a] -> Bool
esConj = \l -> case l of{[]->True;
                        x:xs -> case pert x xs of{True -> False; False -> esConj xs}}

incl :: Eq a => Conj a -> Conj a -> Bool
incl = \l h -> case l of{[]-> True;
                          z:zs -> pert z h && incl zs h }

inter :: Eq a => Conj a -> Conj a -> Conj a
inter = \l h -> case l of{[]->[]; z:zs -> case pert z h of{True -> z:inter zs h; False -> inter zs h } }
