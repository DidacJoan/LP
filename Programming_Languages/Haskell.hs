module SpatialSet where

type Point a = (a, a)

data SpatialSet a = Node (Point a) (SpatialSet a) (SpatialSet a) (SpatialSet a) (SpatialSet a) 
             | Abuit

plantar :: (Point a) -> (SpatialSet a)
plantar x = (Node x Abuit Abuit Abuit Abuit)

-- ============================================================================================

-- Declara la funció iguals com una instancia de la classe Eq (Equal)

instance (Eq a) => Eq (SpatialSet a) where x==y = iguals x y
iguals :: (Eq a) => (SpatialSet a) -> (SpatialSet a) -> Bool
iguals Abuit Abuit = True
iguals a1 a2 = all (contains a2) (get_all a1) &&  all (contains a1) (get_all a2)

-- ============================================================================================

-- Controla que la indentacio del show sigui correctament gestionada segons els nivells del SpatialSet

indent x = if(x==1) then "\t"
	   else "\t" ++ (indent (x-1))

-- Mostra els diferents elements del arbre indentats per nivells i indicant a quin quadrant pertany cada un

instance Show (SpatialSet a) where
showx Abuit l = "\n"
showx (Node x a1 a2 a3 a4) l = show x ++ "\n" ++ sa1 ++ sa2 ++ sa3 ++ sa4
	  where
	      sa1 = if(a1/=Abuit) then tab ++ "<NE> " ++ showx a1 nl
		else ""
	      sa2 = if(a2/=Abuit) then tab ++ "<SE> " ++ showx a2 nl
		else ""
	      sa3 = if(a3/=Abuit) then tab ++ "<NO> " ++ showx a3 nl
		else ""
	      sa4 = if(a4/=Abuit) then tab ++ "<SO> " ++ showx a4 nl
		else ""
	      tab = indent l
	      nl = l+1

-- ============================================================================================

-- Donat un SpatialSet i un punt, inserta el punt donat al SpatialSet seguint els criteris
-- següents: 
-- Suposem els 4 quadrants respecte un punt
--   ╔═══╦═══╗	Les fronteres se situarien 	╔═══╦═══╗
--   ║   ║   ║	de la forma Seguent:		║ ^ ║>  ║
--   ╠═══╬═══╣					╠═══╬═══╣
--   ║   ║   ║					║ < ║ v ║
--   ╚═══╩═══╝					╚═══╩═══╝

insert :: (Ord a) => (SpatialSet a) -> (Point a) -> (SpatialSet a)
insert Abuit n = (Node n Abuit Abuit Abuit Abuit)
insert (Node x a1 a2 a3 a4) n =
          if (n1>=x1 && n2>x2 )  then (Node x (insert a1 n) a2 a3 a4)
          else if (n1>x1 && n2<=x2 ) then (Node x a1 (insert a2 n) a3 a4)
          else if (n1<x1 && n2>=x2 )  then (Node x a1 a2 (insert a3 n) a4)
          else if (n1<=x1 && n2<x2 )  then (Node x a1 a2 a3 (insert a4 n))
          else (Node x a1 a2 a3 a4)
          where
              n1=fst(n)
              n2=snd(n)
              x1=fst(x)
              x2=snd(x)


-- Inserta una llista de punts al arbre mitjançant foldl

build :: (Ord a) => (SpatialSet a) -> [(Point a)] -> (SpatialSet a)
build a x = foldl insert a x

-- ============================================================================================

-- Donat un SpatialSet,retorna una llista amb tots els seus nodes en preordre

get_all :: (SpatialSet a) -> [(Point a)]
get_all Abuit = []
get_all (Node x a1 a2 a3 a4) = x : (get_all a1)++(get_all a2)++(get_all a3)++(get_all a4)

-- ============================================================================================

-- Donats un SpatialSet i un Punt del mateix tipus elimina el punt (si existeix) del conjunt

remove :: (Ord a) => (SpatialSet a) -> (Point a) -> (SpatialSet a)
remove Abuit n = Abuit
remove (Node x a1 a2 a3 a4) n = if(x==n) then (insertFill Abuit (Node x a1 a2 a3 a4))
				else (Node x (remove a1 n) (remove a2 n) (remove a3 n) (remove a4 n))

-- Donats dos SpatialSets inserta els punts del segon SpatialSet al primer 

insertFill :: (Ord a) => (SpatialSet a) -> (SpatialSet a) -> (SpatialSet a)
insertFill a Abuit = a
insertFill a (Node _ a1 a2 a3 a4) = build a ((get_all a1)++(get_all a2)++(get_all a3)++(get_all a4))

-- ============================================================================================

-- Donats un SpatialSet i un punt del mateix tipus retorna si el punt es troba en el conjunt

contains :: (Eq a) => (SpatialSet a) -> (Point a) -> Bool
contains Abuit _ = False
contains a b = any (b==) (get_all a)

-- ============================================================================================

-- Donats un SpatialSet i un punt del mateix tipus retorna el punt pertanyent al conjunt que te la distancia 
-- mes petita respecte el punt donat. El punt donat no te per que formar part del SpatialSet

nearest :: (Ord a) => (Floating a) => (SpatialSet a) -> (Point a) -> (Point a)
nearest a n = (chooseCloser (nearDist a n) n)

-- Sonat un SpatialSet i un punt del mateix tipus, retorna la llista de punts de l'estructura on es possible
-- trobar el punt que presenta la distancia minima respecte el punt donat

nearDist :: (Ord a) => (SpatialSet a) -> (Point a) -> [(Point a)]
nearDist Abuit n = []
nearDist (Node x a1 a2 a3 a4) n = if(n1>=x1 && n2>x2) then (x):(nearDist a1 n)++(nearDist a2 n)++(nearDist a3 n)
					else if(n1<=x1 && n2<x2) then x:(nearDist a2 n)++(nearDist a3 n)++(nearDist a4 n)
					else if(n1<x1 && n2>=x2) then x:(nearDist a1 n)++(nearDist a3 n)++(nearDist a4 n)
					else if(n1>x1 && n2<=x2) then x:(nearDist a1 n)++(nearDist a2 n)++(nearDist a4 n)
					else x:[]
					where 
						x1=fst(x)
              					x2=snd(x)
              					n1=fst(n)
              					n2=snd(n)

-- Donada una llista de punts i un punt del mateix tipus, retorna el punt de la llista que
-- esta a la distancia mes petita respecte el punt donat

chooseCloser :: (Ord a) => (Num a) => (Floating a) => [(Point a)] -> (Point a) -> (Point a)
chooseCloser l n = snd(minimum [(distance n p',p')|p'<-l])

-- Donats dos punts de tipus a, retorna la distancia entre ells

distance :: (Floating a) => (Point a) -> (Point a) -> a
distance (a,b) (x,y) = sqrt((a-x)*(a-x)+(b-y)*(b-y))

-- ============================================================================================

-- Donada una funcio de tipus ((a,a) -> (b,b)) i un SpatialSet de tipus a, retorna un SpatialSet
-- de punts de tipus b resultant d'aplicar la funció a tots els punts mantenint l'estructura.

qmap :: (Ord b) => ((a,a) -> (b,b)) -> (SpatialSet a) -> (SpatialSet b)
qmap f a = build Abuit (map f (get_all a))

-- ============================================================================================

-- Donats dos valors del tipus a i un SpatialSet, aplica una translacio a tots els punts de l'estructura

translation :: (Num a) => (Ord a) => (a) -> (a) -> (SpatialSet a) -> (SpatialSet a)
translation _ _ Abuit = Abuit
translation x y a = qmap (suma x y) a

suma ::  (Num a) => a -> a -> (Point a) -> (Point a)
suma a b (x,y) = (x+a,y+b)

-- ============================================================================================

-- Donat un valor del tipus a i un SpatialSet retorna un SpatialSet tal que tots els seus punts
-- han estat escalats pel valor donat

scale :: (Num a) => (Ord a) => (a) -> (SpatialSet a) -> (SpatialSet a)
scale _ Abuit = Abuit
scale x a = qmap (esc x) a 

esc :: (Num a) => (a) -> (Point a) -> (Point a)
esc a (x,y) = (a*x,a*y) 

-- ============================================================================================

-- Donada una funció de tipus a -> (b,b) -> a, un valor acumulat de tipus a i un SpatialSet
-- de punts de tipus b, aplica la funció a tots els punts del SpatialSet acumulant el resultat

qfold :: (Num a) => ((a) -> (b,b) -> a) -> (a) -> (SpatialSet b) -> (a)
qfold f r Abuit = r
qfold f r (Node x a1 a2 a3 a4) =  (qfold f (qfold f (qfold f (qfold f (f r x) a1) a2) a3) a4)

-- VERSIO ALTERNATIVA
-- qfold f r (Node x a1 a2 a3 a4) =  qfold f fol4 a4
--                                    where
--                                        fol1 = f r x
--                                        fol2 = qfold f fol1 a1
--                                        fol3 = qfold f fol2 a2
--                                        fol4 = qfold f fol3 a3

-- ============================================================================================

-- Donat un SpatialSet retorna el nombre de punts que conté

size :: (Num a) => (SpatialSet b) -> a
size a = qfold funFold (0) a

funFold :: (Num a) => a -> (Point b) -> a
funFold r _ = (r+1) 

-- ============================================================================================

-- Donat un SpatialSet, retorna la seva Bounding Box en forma de dos punts que indiquen
-- l'extrem esquerre superior i el dret inferior de la caixa respectivament

box :: (Num a) => (Ord a) => (SpatialSet a) -> ((Point a),(Point a))
box Abuit = ((0,0),(0,0))
box a = ((topLeft a),(bottomRight a))

-- =========

-- Calcula el punt amb la x minima i la y maxima d'entre tots els punts de que conté el SpatialSet 
-- es a dir, calcula la cantonada dreta inferior de la Bounding Box

topLeft :: (Num a) => (Ord a) => (SpatialSet a) -> (Point a)
topLeft (Node (x,y) a1 a2 a3 a4) = ( ( qfold funMinx x (Node (x,y) a1 a2 a3 a4) ) , ( qfold funMaxy y (Node (x,y) a1 a2 a3 a4) ) )

-- Calcula la x minima entre el minim guardat i el punt avaluat

funMinx :: (Ord a) => (a) -> (Point a) -> (a)
funMinx r (x,_) = if(x<r) then x 
		else r

-- Calcula la y maxima entre el maxim guardat i el punt avaluat

funMaxy :: (Ord a) => (a) -> (Point a) -> (a)
funMaxy r (_,y) = if(y>r) then y 
	      else r

-- =========

-- Calcula el punt amb la x maxima i la y minima d'entre tots els punts de que conté el SpatialSet
-- es a dir, calcula la cantonada dreta inferior de la Bounding Box

bottomRight :: (Num a) => (Ord a) => (SpatialSet a) -> (Point a)
bottomRight (Node (x,y) a1 a2 a3 a4) = ( ( qfold funMaxx x (Node (x,y) a1 a2 a3 a4) ) , ( qfold funMiny y (Node (x,y) a1 a2 a3 a4) ) )

-- Calcula la x maxima entre el maxim guardat i el punt avaluat

funMaxx :: (Ord a) => (a) -> (Point a) -> (a)
funMaxx r (x,_) = if(x>r) then x 
		else r

-- Calcula la y minima entre el minim guardat i el punt avaluat

funMiny :: (Ord a) => (a) -> (Point a) -> (a)
funMiny r (_,y) = if(y<r) then y 
	      else r

-- ==================================================== PROVA FUNCIONS =======================================================

punts :: [(Point Double)]
punts = [(3.0,2.1),(3.5,3.1),(3.5,2.1),(3.0,3.1),(3.0,0.0),(1.5,1.5),(3.3,2.5),(4.0,3.8),(3.1,4.8),(1.8,(-2.0))]
punts1 = [(3.0,2.1),(3.5,3.1),(3.5,2.1),(3.0,3.1),(3.0,0.0),(1.5,1.5),(3.3,2.5)]

arb = build Abuit punts
arb1 = build Abuit punts1

---------------GET_ALL

provaget = get_all (arb)

---------------CONTAINS

provacontains1 = contains (arb) (2.9,0.0)
provacontains2 = contains (arb) (2.8,0.0)

---------------REMOVE

provarem = remove (arb) (3.3,2.5)
showprovarem = get_all provarem

---------------NEAREST

provanear = nearest (arb) (3.1,2.1)

---------------IGUALS

provaiguals = iguals (arb) (arb)
provaiguals1 = iguals (arb1) (arb)

---------------SHOW

provaShow = putStrLn(showx (arb) (1))

---------------QMAP-----------------------

---------------TRANSLATION

provatr = translation (3) (3) (arb)
showprovaTr = get_all provatr

---------------ESCALATE

provasc = scale (2) (arb)
showprovaesc = get_all provasc

---------------QFOLD----------------------

---------------SIZE

provasize = size (arb)

---------------BOX

provabox = box (arb)

--------------------------------------------------------------------------------------------------------------------------------
