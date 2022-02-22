program MSet2Color
	implicit none
	
	integer(16) :: x,y,m,n
	real(16) :: s,x1,y1,p,o,b,q,r,pi
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
	y1=-y1																									!Fixing y values being flipped do to direction of stepping through y
	pi=3.1415926535897932384626433832795028841971693993751058209749											!Setting value of pi
	allocate(A(2*x+1, 2*y+1))																				!Setting up dimensions of A
	A(:,:) = 0																								!Filling A with 0's
	do m=-y,y																								!Stepping through y dimension
		do n=-x,x																							!Stepping through x dimension
			p=0																								!Clearing p
			o=0																								!Clearing o
			z=cmplx(0,0)																					!Clearing z
			do while (o<b.and.p<2)
				z=z**2+cmplx(n*(2/(s*x))+x1, m*2/(s*y)+y1)													!MSet Calculation
				o=o+1																						!Counting itterations
				p=abs(z)																					!Making sure abs(z)<2
			end do
			A(n+1+x, m+1+y)=log(o)/log(2.0)																	!With log2
			!A(n+1+x, m+1+y)=o 																				!Without log2
		end do
	end do
	q=minval(A)																								!Recording min value of the matrix
	r=maxval(A)/(2*pi)																						!Recording max value devided by 2*pi
																											!Creating image
	open(1, File="MSet Matrix P6")
	write(1,'(A)') 'P6'																						!Labeling image as PPM using bytes
	write(1,'(I5)') 2*x+1																					!Setting X dimension of the image
	write(1,'(I5)') 2*y+1																					!Setting Y dimension of the image
	write(1,'(I5)') int(log(b)/log(2.0)-q)																	!Setting maximum gray value
	do m=-y,y																								!Stepping through y dimension of A
		do n=-x,x																							!Stepping through x dimension of A
			if (int(log(b)/log(2.0)-q).gt.255) then															!Checking if one or two bytes are needed
				write(1,'(A,A,A,A,A,A)',advance="no")&
					char(floor(real((A(n+1+x, m+1+y)-q)/256))*floor(cos(A(n+1+x, m+1+y)/r)/2+1)),&
					char(int(A(n+1+x, m+1+y)-q)*floor(cos(A(n+1+x, m+1+y)/r)/2+1)),&
					char(floor(real((A(n+1+x, m+1+y)-q)/256))*floor(cos(A(n+1+x, m+1+y)/r+pi/3)/2+1)),&
					char(int(A(n+1+x, m+1+y)-q)*floor(cos(A(n+1+x, m+1+y)/r+pi/3)/2+1)),&
					char(floor(real((A(n+1+x, m+1+y)-q)/256))*floor(cos(A(n+1+x, m+1+y)/r+2*pi/3)/2+1)),&
					char(int(A(n+1+x, m+1+y)-q)*floor(cos(A(n+1+x, m+1+y)/r+2*pi/3)/2+1))					!A 2 byte mess setting the colors to cos funtions pi/3 out of phase from each other
			else
				write(1,'(A,A,A)',advance="no")&
					char(int(A(n+1+x, m+1+y)-q)*floor(cos(A(n+1+x, m+1+y)/r)/2+1)),&
					char(int(A(n+1+x, m+1+y)-q)*floor(cos(A(n+1+x, m+1+y)/r+pi/3)/2+1)),&
					char(int(A(n+1+x, m+1+y)-q)*floor(cos(A(n+1+x, m+1+y)/r+2*pi/3)/2+1))					!A 1 byte mess setting the colors to cos funtions pi/3 out of phase from each other
			end if
		end do
	end do
	close(1)
end program MSet2Color