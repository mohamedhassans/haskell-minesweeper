type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState deriving (Show,Eq)

 
solve :: Cell->[Cell]->[String]
solve cell list = realsolve (cell:(sort cell list))

--solvem h t =realsolve (h:t)
realsolve [x] =[]
realsolve (x:y:t)= (getrout x y)++(realsolve (y:(sort y t)))


getrout (x1,y1) (x2,y2)	| x1==x2&&y1==y2=["collect"]
                        | x2<x1 ="up":getrout (x1-1,y1) (x2,y2)
	                    | x2>x1	="down":getrout (x1+1,y1) (x2,y2)		
						| y2<y1	="left":getrout (x1,y1-1) (x2,y2)
						| y2>y1	="right":getrout (x1,y1+1) (x2,y2)

--sort	
sort  c []=[]
sort  c (h:t) =helper1sort c t [h]

helper1sort c [] x=x
helper1sort c (x:t) l = helper1sort c t (intsert1 c x l)

intsert1:: Cell -> Cell->[Cell]->[Cell]
intsert1 c x []=[x]
intsert1 robot tr (h:t)| ((manhaten robot tr)<(manhaten robot h)) = (tr:((h:t)))
                       |  otherwise = (h:(intsert1 robot tr t))
--endsort	
 
--manhaten formula
--abss x | x<0=0-x
--       | otherwise =x
manhaten (x1,y1) (x2,y2)=(abs(x1-x2))+(abs(y1-y2))
