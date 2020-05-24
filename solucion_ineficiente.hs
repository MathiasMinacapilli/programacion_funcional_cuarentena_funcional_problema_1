import Data.List
import Data.Maybe
data Color = Roja | Verde | Blanca | Amarilla | Azul
deriving (Show,Eq,Enum)
data Nacion = Brit | Suizo | Danes | Noruego | Aleman
deriving (Show,Eq,Enum)
data Bebe = Te | Cafe | Leche | Cerveza | Agua
deriving (Show,Eq,Enum)
data Fuma = Pall | Dunhill | Blends | BlueMaster | Prince
deriving (Show,Eq,Enum)
data Mascota = Perro | Pajaro | Gato | Caballo | Pez
deriving (Show,Eq,Enum)
data Barrio = Barrio [Color ] [Nacion ] [Bebe ] [Fuma ] [Mascota ]
deriving Show
numero e = (+1) . fromJust . elemIndex e
color e (Barrio cs ) = numero e cs
nacion e (Barrio ns ) = numero e ns
bebe e (Barrio bs ) = numero e bs
fuma e (Barrio fs ) = numero e fs
mascota e (Barrio ms) = numero e ms
casas = [Barrio cs ns bs fs ms
| cs ← permutations . enumFrom $ Roja
, ns ← permutations . enumFrom $ Brit
, bs ← permutations . enumFrom $ Te
, fs ← permutations . enumFrom $ Pall
, ms ← permutations . enumFrom $ Perro ]
sols = [b | b ← casas
, nacion Brit b == color Roja b
, nacion Suizo b == mascota Perro b
, nacion Danes b == bebe Te b
, color Verde b == color Blanca b − 1
, color Verde b == bebe Cafe b
, fuma Pall b == mascota Pajaro b
, color Amarilla b == fuma Dunhill b
, bebe Leche b == 3
, nacion Noruego b == 1
, next (fuma Blends b) (mascota Gato b)
, next (fuma Dunhill b) (mascota Caballo b)
, fuma BlueMaster b == bebe Cerveza b
, nacion Aleman b == fuma Prince b
, next (nacion Noruego b) (color Azul b)
, next (fuma Blends b) (bebe Agua b)]
next x y = abs (x − y) == 1
main = print $ (naciones solucion) !! (mascota Pez solucion − 1)
where solucion = head sols
naciones (Barrio ns ) = ns
