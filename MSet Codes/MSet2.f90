program MSet2
	implicit none
	
	integer(16) :: x,y,m,n
	real(16) :: s,x1,y1,p,o,b,q
	complex(16) :: z
	integer, allocatable :: A(:,:)
																!Getting Inputs
	print *, 'x ='
	read(*,*) x
	print *, 'y = '
	read(*,*) y
	print *, 'resolution = '
	read(*,*) b
	print *, 'scale ='
	read(*,*) s
	print *, 'delta x ='
	read(*,*) x1
	print *, 'delta y ='
	read(*,*) y1
	y1=-y1														!Fixing y values being flipped do to direction of stepping through y
	allocate(A(2*x+1, 2*y+1))									!Setting up dimensions of A
	A(:,:) = 0													!Filling A with 0's
	do m=-y,y													!Stepping through y dimension
		do n=-x,x												!Stepping through x dimension
			p=0													!Clearing p
			o=0													!Clearing o
			z=cmplx(0,0)										!Clearing z
			do while (o<b.and.p<2)
				z=z**2+cmplx(n*(2/(s*x))+x1, m*2/(s*y)+y1)		!MSet Calculation
				o=o+1											!Counting itterations
				p=abs(z)										!Making sure abs(z)<2
			end do
			A(n+1+x, m+1+y)=log(o)/log(2.0)						!add o to the matrix devided by log2
		end do
	end do
	q=minval(A)													!Get the smallest value in A
																!Creating Image
	open(1, File="MSet Matrix P5")
	write(1,'(A)') 'P5'											!Labeling image as PGM using bytes
	write(1,'(I5)') 2*x+1										!Setting X dimension of the image
	write(1,'(I5)') 2*y+1										!Setting Y dimension of the image
	write(1,'(I5)') int(log(b)/log(2.0)-q)						!Setting maximum gray value
	do m=-y,y													!Stepping through y dimension of A
		do n=-x,x												!Stepping through x dimension of A
			if (int(log(b)/log(2.0)-q).gt.255) then				!Checking if one or two bytes are needed
				write(1,'(A,A)',advance="no")&
					char(floor(real((A(n+1+x, m+1+y)-q)/256))),&
					char(int(A(n+1+x, m+1+y)-q))				!converting from dec and writing values to file as two bytes
			else
				write(1,'(A)',advance="no")& 
					char(int(A(n+1+x, m+1+y)-q))				!converting from dec and writing values to file as one byte
			end if
		end do
	end do
	close(1)
end program MSet2