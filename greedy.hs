minima [] m r l = l
minima (n:ns) 1 r l = max (sum (n:ns) + r) (l)
minima (n:ns) m r l = min (minima ns m (r + n) la)  (minima (n:ns) (m-1) 0 ls)
    where   la = max (r+n) (l)
            ls = max (r) (l)


mergeSort [] = []
mergeSort [a] = [a]
mergeSort a =
  merge (mergeSort firstFew) (mergeSort lastFew)
    where firstFew = take ((length a) `div` 2) a
          lastFew = drop ((length a) `div` 2) a

merge a [] = a
merge [] b = b
merge ((a,b,c):as) ((x,y,z):bs)
  | c > z     = (a,b,c):(merge as ((x,y,z):bs))
  | otherwise = (x,y,z):(merge ((a,b,c):as) bs)

match [] v = []
match ((a,b,w):es) v = if ((elem a v) || (elem b v)) 
                        then match es v
                        else (a,b,w):(match es ([a,b] ++ v))

matching e = match (mergeSort e) []

e = [(1,2,3), (4,3, 1), (1,6,6)]
v = []
