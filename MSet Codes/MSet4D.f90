program MSet4D
	use Graphics
	use QuaternionMath
	implicit none
	
	integer :: w,x,y,z,k,l,m,n,o,c
	real :: s,w1,x1,y1,z1,p,b
	type(quaternion) :: q
	real, allocatable :: A(:,:)!,D(:,:)
	
	print *, 'w ='
	read(*,*) w
	print *, 'x = '
	read(*,*) x
	print *, 'y ='
	read(*,*) y
	print *, 'z = '
	read(*,*) z
	!print *, 'resolution = '
	!read(*,*) b
	b=100
	!print *, 'scale ='
	!read(*,*) s
	s=1
	!print *, 'delta w ='
	!read(*,*) w1
	w1=0
	!print *, 'delta x ='
	!read(*,*) x1
	x1=0
	!print *, 'delta y ='
	!read(*,*) y1
	y1=0
	!print *, 'delta z ='
	!read(*,*) z1
	z1=0
	
	allocate(A(5,(2*w+1)*(2*x+1)*(2*y+1)*(2*z+1)))
	A(:,:)=0
	
	do k=-w,w
		do l=-x,x
			do m=-y,y
				do n=-z,z
				o=0
				p=0
				q=quaternion(0,0,0,0)
					do while (o<b.and.p<2)
						q=(q.qxq.q).qtq.quaternion(k*2/(s*w)+w1,l*2/(s*x)+x1,m*2/(s*y)+y1,n*2/(s*z)+z1)
						o=o+1
						p=absQ(q)
					end do
				c=z+n+1
				c=c+(y+m)*(2*z+1)
				c=c+(x+l)*(2*z+1)*(2*y+1)
				c=c+(w+k)*(2*z+1)*(2*y+1)*(2*x+1)
				A(1,c)=k*2/(s*w)+w1
				A(2,c)=l*2/(s*x)+x1
				A(3,c)=m*2/(s*y)+y1
				A(4,c)=n*2/(s*z)+z1
				if (o.eq.b) then
					A(5,c)=1
				end if
				end do
			end do
		end do
	end do
!	!4D
!	do n=1,size(A,2)
!		q=quaternion(A(1,n),A(2,n),A(3,n),A(4,n))
!		p=A(5,n)
!		A(1,n)=q%i+sqrt(2.0)*(q%r-q%k)/2
!		A(2,n)=q%j+sqrt(2.0)*(q%r+q%k)/2
!		A(3,n)=p
!		if (p.ne.0) then
!			A(4,n)=4+q%i-q%r
!		else
!			A(4,n)=0
!		end if
!	end do
!	deallocate(A)
!	call plot3I(A,[2*x+1,2*y+1])
	!3D
	open(2,file="test.csv")
	do n=1,size(A,2)
		q=quaternion(A(1,n),A(2,n),A(3,n),A(4,n))
		p=A(5,n)
		A(1,n)=q%r+q%i
		A(2,n)=q%j+(q%r-q%i)/2
		A(3,n)=p
		if (p.ne.0) then
			A(4,n)=4+q%i-q%r
		else
			A(4,n)=0
		end if
	end do
	close(2)
	call plot3I(A,[4*x+1,4*y+1])
	
end program MSet4D