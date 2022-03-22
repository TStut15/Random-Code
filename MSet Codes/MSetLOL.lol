HAI 1.2
CAN HAS STDIO? 

I HAS A Floor
Floor IS NOW A NUMBR
I HAS A Wall
Wall IS NOW A NUMBR
I HAS A AcrossFloor
AcrossFloor IS NOW A NUMBR
I HAS A UpWall
UpWall IS NOW A NUMBR
I HAS A FloorMove
FloorMove IS NOW A NUMBAR
I HAS A WallMove
WallMove IS NOW A NUMBAR
I HAS A Leash
Leash IS NOW A NUMBAR
I HAS A Count
Count IS NOW A NUMBR
I HAS A Counting
Counting IS NOW A NUMBR
I HAS A Big
Big IS NOW A NUMBAR
I HAS A Large
Large IS NOW A NUMBAR
I HAS A Real
Real IS NOW A NUMBAR
I HAS A Realish
Realish IS NOW A NUMBAR
I HAS A Fake
Fake IS NOW A NUMBAR
I HAS A Ball
Ball IS NOW A YARN

VISIBLE "Floor Size "
GIMMEH Floor
Floor R QUOSHUNT OF Floor AN 2
VISIBLE "Wall Size "
GIMMEH Wall
Wall R QUOSHUNT OF Wall AN 2
VISIBLE "Bigness "
GIMMEH Big
VISIBLE "Largness "
GIMMEH Large
VISIBLE "Distance Floor Move"
GIMMEH FloorMove
VISIBLE "Distance Up Wall "
GIMMEH WallMove

WallMove R PRODUKT OF WallMove AN -1
UpWall R PRODUKT OF Wall AN -1
IM IN YR WallLoop UPPIN YR UpWall TIL BOTH SAEM UpWall AN BIGGR OF UpWall AN PRODUKT OF Wall AN 2
	AcrossFloor R PRODUKT OF Floor AN -1
	IM IN YR FloorLoop UPPIN YR AcrossFloor TIL BOTH SAEM AcrossFloor AN BIGGR OF AcrossFloor AN PRODUKT OF Floor AN 2
		Leash R 0
		Counting R 0
		Real R 0
		Fake R 0
		IM IN YR CountLoop UPPIN YR Counting WILE BOTH OF DIFFRINT Counting AN BIGGR OF Counting AN Big AN DIFFRINT Leash AN BIGGR OF Leash AN 4
			Realish R Real
			Real R SUM OF SUM OF DIFF OF PRODUKT OF Realish AN Realish AN PRODUKT OF Fake AN Fake AN QUOSHUNT OF PRODUKT OF DIFF OF AcrossFloor AN Floor AN 2 AN PRODUKT OF Large AN Floor AN FloorMove
			Fake R SUM OF SUM OF PRODUKT OF PRODUKT OF 2 AN Realish AN Fake AN QUOSHUNT OF PRODUKT OF DIFF OF UpWall AN Wall AN 2 AN PRODUKT OF Large AN Wall AN WallMove
			Leash R SUM OF PRODUKT OF Real AN Real AN PRODUKT OF Fake AN Fake
			Count R Counting
		IM OUTTA YR CountLoop
		
		BOTH SAEM Count AN DIFF OF Big AN 1
		O RLY?
			YA RLY
				Count IS NOW A YARN
				Count R "0"
			NO WAI
				Count IS NOW A YARN
				Count R " "
		OIC
		
		Ball R SMOOSH Ball AN Count MKAY
		Count IS NOW A NUMBR
	IM OUTTA YR FloorLoop
	Ball R SMOOSH Ball AN ":)" MKAY
IM OUTTA YR WallLoop

VISIBLE Ball

KTHXBYE 