program MSet
	implicit none
	
	integer(16) :: x,y,m,n
	real(16) :: s,x1,y1,p,o,b,q
	complex(16) :: z
	integer, allocatable :: A(:,:)
	!character, allocatable :: F(:)
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
	y1=-y1													!Fixing y values being flipped do to direction of stepping through y
	allocate(A(2*x+1, 2*y+1))								!Setting up dimensions of A
	A(:,:) = 0												!Filling A with 0's
	do m=-y,y												!Stepping through y dimension
		do n=-x,x											!Stepping through x dimension
			p=0												!Clearing p
			o=0												!Clearing o
			z=cmplx(0,0)									!Clearing z
			do while (o<b.and.p<2)
				z=z**2+cmplx(n*(2/(s*x))+x1, m*2/(s*y)+y1)	!MSet Calculation
				o=o+1										!Counting itterations
				p=abs(z)									!Making sure abs(z)<2
			end do
			A(n+1+x, m+1+y)=log(o)/log(2.0)					!With log2
			!A(n+1+x, m+1+y)=o 								!Without log2
		end do
	end do
	q=minval(A)
	!print *, A
															!Creating Image
	open(10, File='MSet Matrix') 
	write(10,'(A)') 'P2' 									!Labling image
	write(10,'(I5)') 2*x+1									!setting x dimmension of image
	write(10,'(I5)') 2*y+1 									!setting y dimemnsion of image
	write(10,'(I5)') int(log(b)/log(2.0)-q) 				!With log2
	!write(10,'(I5)') int(b-q) 								!Without log2
	write(10,'(I5)') int(A-q)
	deallocate(A)
	close(10)
end program MSet