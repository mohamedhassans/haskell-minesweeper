type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState deriving (Show,Eq)
up:: MyState -> MyState
up (S (x, y) list string myState) = if x-1 <0 then Null 
								  else S (x-1,y) list "up" (S (x, y) list string myState)
						
down:: MyState -> MyState						
down (S (x, y) list string myState) = if x+1 >3 then Null 
								  else S (x+1,y) list "down" (S (x, y) list string myState)

left:: MyState -> MyState								  
left (S (x, y) list string myState) = if y-1 <0 then Null 
								  else S (x,y-1) list "left" (S (x, y) list string myState)
right:: MyState -> MyState								  
right (S (x, y) list string myState) = if y+1 >3 then Null 
								  else S (x,y+1) list "right" (S (x, y) list string myState)
								  
								  
								 
collect:: MyState -> MyState								 
collect (S (x, y) list string myState)	= if (elem (x,y) list ) then (S (x,y) (filter(/=(x,y)) list) "collect" (S (x, y) list string myState))	
										else Null
								
nextMyStates::MyState->[MyState]								
nextMyStates s =filter (/=Null) [up s,down s,left s,right s,collect s]

isGoal::MyState->Bool
isGoal (S c lc string mystate) = length lc ==0

search::[MyState]->MyState
search (x:t) |isGoal x = x
             |otherwise = search (t++nextMyStates x)

constructSolution:: MyState ->[String]
constructSolution (S c lc "" mystate)= []
constructSolution (S c lc string mystate) =  (string : constructSolution mystate)

solve :: Cell->[Cell]->[String]
solve cell []=[]
solve cell list =   reverse (constructSolution  (search (nextMyStates (S cell list "" Null))))
	