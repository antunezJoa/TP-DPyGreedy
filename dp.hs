import Data.Matrix

largoAux p f [] d = 0
largoAux p f (s:ss) d =  if (d >= p && d <= f)
                    then s + (largoAux p f ss (d+1))
                    else (largoAux p f ss (d+1))



pals = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]

len = length pals + 1
ren = 5
initial = zero ren len

largo p f = largoAux p f pals 1


setUnRenglon 1 mat = setElem (largo 1 1) (1,2) mat
setUnRenglon n mat = setUnRenglon (n-1) (setElem (largo 1 (n-1)) (1, n) mat)
first = setUnRenglon len initial

kcalc 1 m n mat = min (1000) (max (getElem (m-1) (n) mat) (largo n n))
kcalc k m n mat = min (kcalc (k-1) m n mat) (max (getElem (m-1) (n-k+1) mat) (largo (n-k+1) n)) 

rencalc 2 n mat = setElem (kcalc n 2 (n) (mat)) (2,(n+1)) (mat)  
rencalc m n mat = setElem (kcalc n m (n) (rencalc (m-1) n mat)) (m,(n+1)) ((rencalc (m-1) n mat))

wcalc 1 m mat = rencalc m 1 mat
wcalc n m mat = rencalc m (n-1) (wcalc (n-1) m mat)


-- wcalc len ren first
