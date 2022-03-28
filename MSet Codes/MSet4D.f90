program MSet4D
	use Graphics
	use QuaternionMath
	implicit none
	
	integer :: w,x,y,z,k,l,m,n,o,c
	real :: s,w1,x1,y1,z1,p,b
	type(quaternion) :: q
	real, allocatable :: A(:,:)
	
	print *, 'w ='											!Get size of value space
	read(*,*) w
	print *, 'x = '
	read(*,*) x
	print *, 'y ='
	read(*,*) y
	print *, 'z = '
	read(*,*) z
	
	!print *, 'resolution = '								!Get Number of Iterations
	!read(*,*) b
	b=100
	
	!print *, 'scale ='										!Get zoom factor
	!read(*,*) s
	s=1
	
	!print *, 'delta w ='									!Gets ofsets from center
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
	
	allocate(A(5,(2*w+1)*(2*x+1)*(2*y+1)*(2*z+1)))			!Allocation and filling with 0s
	A(:,:)=0
	
	if ((z.ne.0).and.(w.ne.0)) then 						!4D\
	
		do k=-w,w											!Calculation of MSet using quaternions
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
							
						c=z+n+1								!c is used instead of using more array dimensions to allow working with Graphics module better
						c=c+(y+m)*(2*z+1)
						c=c+(x+l)*(2*z+1)*(2*y+1)
						c=c+(w+k)*(2*z+1)*(2*y+1)*(2*x+1)
						
						A(1,c)=k*2/(s*w)+w1					!Writing coordinates to array
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

		do n=1,size(A,2)									!Convert to isomorphic
			q=quaternion(A(1,n),A(2,n),A(3,n),A(4,n))
			p=A(5,n)
			A(1,n)=q%i+sqrt(2.0)*(q%r-q%k)/2
			A(2,n)=q%j+sqrt(2.0)*(q%r+q%k)/2
			A(3,n)=p*(4+q%i-q%r)
			if (p.ne.0) then
				A(4,n)=4+q%i-q%r
			else
				A(4,n)=0
			end if
		end do
		call plot3I(A,[4*x+1,4*y+1])
	
	elseif (w.eq.0) then									!3D imaginary
	
		do l=-x,x											!Calculation of MSet using quaternions but only using the imaginary values
			do m=-y,y
				do n=-z,z
					o=0
					p=0
					q=quaternion(0,0,0,0)
						do while (o<b.and.p<2)
							q=(q.qxq.q).qtq.quaternion(w1,l*2/(s*x)+x1,m*2/(s*y)+y1,n*2/(s*z)+z1)
							o=o+1
							p=absQ(q)
						end do
						
					c=z+n+1									!c is used instead of using more array dimensions to allow working with Graphics module better
					c=c+(y+m)*(2*z+1)
					c=c+(x+l)*(2*z+1)*(2*y+1)
					
					A(1,c)=w1								!Writing coordinates to array
					A(2,c)=l*2/(s*x)+x1
					A(3,c)=m*2/(s*y)+y1
					A(4,c)=n*2/(s*z)+z1
					if (o.eq.b) then
						A(5,c)=1
					end if
				end do
			end do
		end do

		do n=1,size(A,2)									!Convert to isomorphic
			q=quaternion(A(1,n),A(2,n),A(3,n),A(4,n))
			p=A(5,n)
			A(1,n)=q%k+q%i
			A(2,n)=q%j+(q%k-q%i)/2
			A(3,n)=p*(4+q%i-q%k)
			if (p.ne.0) then
				A(4,n)=4+q%i-q%k
			else
				A(4,n)=0
			end if
		end do
		call plot3I(A,[4*x+1,4*y+1])

	else 													!3D Real
	
		do k=-w,w											!Calculation of MSet using quaternions but only using the real and two of the imaginary values
			do l=-x,x
				do m=-y,y
					o=0
					p=0
					q=quaternion(0,0,0,0)
					
						do while (o<b.and.p<2)
							q=(q.qxq.q).qtq.quaternion(k*2/(s*w)+w1,l*2/(s*x)+x1,m*2/(s*y)+y1,z1)
							o=o+1
							p=absQ(q)
						end do
						
					c=1										!c is used instead of using more array dimensions to allow working with Graphics module better
					c=c+(y+m)*(2*z+1)
					c=c+(x+l)*(2*z+1)*(2*y+1)
					c=c+(w+k)*(2*z+1)*(2*y+1)*(2*x+1)
					
					A(1,c)=k*2/(s*w)+w1						!Writing coordinates to array
					A(2,c)=l*2/(s*x)+x1
					A(3,c)=m*2/(s*y)+y1
					A(4,c)=z1
					if (o.eq.b) then
						A(5,c)=1
					end if
				end do
			end do
		end do
	
		do n=1,size(A,2)									!Convert to isomorphic
			q=quaternion(A(1,n),A(2,n),A(3,n),A(4,n))
			p=A(5,n)
			A(1,n)=q%r+q%i
			A(2,n)=q%j+(q%r-q%i)/2
			A(3,n)=p*(4+q%i-q%r)
			if (p.ne.0) then
				A(4,n)=4+q%i-q%r
			else
				A(4,n)=0
			end if
		end do
		call plot3I(A,[4*x+1,4*y+1])
	
	end if

end program MSet4D