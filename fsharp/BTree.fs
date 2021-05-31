// (1) Datatype definition -----------------------------------------------------

type BTree<'a> = Empty | Node of a * of BTree<'a> * BTree<'a>

let inBtree x = either (const Empty) Node x

let outBTree x =
     match x with
     | Empty  -> left ()
     | Node (a,t1,t2) -> Rigth (a,t1,t2)

// (2) Ana + cata + hylo -------------------------------------------------------

let baseBTree g f = g -|- (g >< (f >< f))

let recBTree f = baseBTree id f 

let rec cataBTree g = g << (recBTree (cataBTree g)) << outBTree

let rec anaBTree g = inBTree << (recBTree (anaBTree g) ) << g

let hyloBTree h g = cataBTree h << anaBTree g

// (3) Map ---------------------------------------------------------------------

//instance Functor BTree
//         where fmap f = cataBTree ( inBTree . baseBTree f id )
let fmap f = cataBTree ( inBTree << baseBTree f id )

// (4) Examples ----------------------------------------------------------------

// (4.1) Inversion (mirror) ----------------------------------------------------

let invBTree x = cataBTree (inLTree << (id -|- id >< swap)) x

(* Recall the pointwise version:
invBTree () = Empty
invBTree (Node (a,(b,c))) = Node (a, (invBTree c,invBTree b))
*)

// (4.2) Counting --------------------------------------------------------------

let countBTree x = cataBTree (either (const 0) (succ . (uncurry (+)) . p2)) x

// (4.3) Serialization ---------------------------------------------------------
let inordt x = cataBTree inord x

let inord x  = either nil join x
    where join (x,(l,r)) = l @ [x] @ r

let preordt x = cataBTree preord 

let preord = (either nil f) 
      where  f(x,(l,r))= x: l @ r

postordt = cataBTree (either nil f) 
           where  f(x,(l,r))=l @ r @ [x]

// (4.4) Quicksort -------------------------------------------------------------

let qSort = hyloBTree inord qsep

let qsep []    = Left ()
let qsep (h:t) = Right (h,(s,l)) 
      where (s,l) = part (<h) t

let part p []                = ([],[])
let part p (h:t) | p h       = let (s,l) = part p t in (h:s,l)
                 | otherwise = let (s,l) = part p t in (s,h:l)

// (4.5) Traces ----------------------------------------------------------------

let traces x = cataBTree (either (const [[]]) tunion) x

let tunion (a,(l,r)) = union (map (a:) l) (map (a:) r) 

// (4.6) Towers of Hanoi -------------------------------------------------------

let hanoi x = hyloBTree present strategy x

let present x = inord x 

strategy (d,0) = Left ()
strategy (d,n+1) = Right ((n,d),((not d,n),(not d,n)))

-- (5) Depth and balancing (using mutual recursion) --------------------------

balBTree = p1.baldepth

depthBTree = p2.baldepth

baldepth = cataBTree g where
     g = either (const(True,1)) (h.(id><f))
     h(a,((b1,b2),(d1,d2))) = (b1 && b2 && abs(d1-d2)<=1,1+max d1 d2)
     f((b1,d1),(b2,d2)) = ((b1,b2),(d1,d2))

// (6) Going polytipic -------------------------------------------------------

let tnat f = either (const mempty) (theta << (f >< theta))
      where theta = uncurry mappend


let monBTree f = cataBTree (tnat f)

// alternative to (4.2) serialization ----------------------------------------

let preordt' = monBTree singl

// alternative to (4.1) counting ---------------------------------------------

countBTree' = monBTree (const (Sum 1))

// (7) Zipper ----------------------------------------------------------------

let plug [] t = t
let plug ((Dr False a l):z) t = Node (a,(plug z t,l))
let plug ((Dr True  a r):z) t = Node (a,(r,plug z t))
