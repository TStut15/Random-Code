program display
	use Graphics
	use QuaternionMath
	implicit none
	
	!interface
	!	subroutine
	!		
	!	end subroutine
	!end interface

	real, allocatable :: test(:,:), testb(:,:)
	type(quaternion), allocatable :: testq(:)
	integer :: x,y,u,v,n,a,xx,yy
	type(quaternion) :: q
	real :: pi,ab,t,aba,abb
	complex :: z
	pi=3.1415926535897932384626433832795028841971693993751058209749
	!allocate(test(5,18))
	!test(1,:)=[0,0,0,0,0,1,2,3,4,4,4,4,4,3,2,1,0,0]
	!test(2,:)=[0,1,2,3,4,4,4,4,4,3,2,1,0,0,0,0,0,0]
	!test(3,:)=[1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,0]
	!test(4,:)=[0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0]
	!test(5,:)=[1,1,1,1,0,0,0,0,0,0,0,0,1,1,1,1,1,0]
	!call plot(test,[10,10])
	
	!allocate(test(5,1440))
	!do x=1,1440
	!	test(1,x)=x
	!	test(2,x)=450*sin(3.14*x/720)
	!	test(3,x)=450*cos(3.14*x/720)
	!	test(4,x)=450*cos(3.14*x/720+2*3.14/3)
	!	test(5,x)=450*cos(3.14*x/720+4*3.14/3)
	!end do
	!call plot(test,[1440,900])
	
	xx=1440*4
	yy=900*4
	allocate(test(5,xx*yy))
	do x=1,xx
		do y=1,yy
			test(1,yy*(x-1)+y)=(x-xx/2.0)/(xx/(4*pi))
			test(2,yy*(x-1)+y)=(y-yy/2.0)/(yy/(4*pi))
			z=complex((x-xx/2.0)/(xx/(4*pi)),(y-yy/2.0)/(yy/(4*pi)))
			z=sqrt(1-z**2)
			!z=z**3+z**2+1*z+1
			!z=((z**2-1)*(z-complex(2,1))**2)/(z**2+complex(2,2))

			!ab=-1/(1+exp(abs(z)))
				!No Magnitude
			!aba=1
			!abb=0
				!Value
			aba=log(abs(z)+1)
			abb=0
				!Luminance
			!ab=-abs(z)+1
			!aba=(-ab**2)/(ab**2+1)+1
			!abb=1/(1+exp(ab))
			
			!ab=(2/pi)*atan(abs(z))
			!ab=abs(z)**2/(abs(z)**2+1)
			t=atan2(real(z*complex(0,-1)),real(z))
			test(3,yy*(x-1)+y)=AngleToHue(t)*aba/2+abb
			test(4,yy*(x-1)+y)=AngleToHue(t+2*pi/3)*aba/2+abb
			test(5,yy*(x-1)+y)=AngleToHue(t+4*pi/3)*aba/2+abb
		end do
	end do
	!call linspaceAll2D(test,testb,8*pi)
	call plot(test,[xx,yy])
	
!	a=4096
!	allocate(testb(3,a*a),testq(a*a))
!
!	do u=1,a
!		do v=1,a
!			testb(1,a*(u-1)+v)=sin(2*pi*u/a)*(cos(2*pi*v/a)+2)
!			testb(2,a*(u-1)+v)=cos(2*pi*u/a)*(cos(2*pi*v/a)+2)
!			testb(3,a*(u-1)+v)=sin(2*pi*v/a)
!		end do
!	end do
!
!	testq=XYZtoQ(testb)
!
!	q%r=cos(pi/4)
!	q%i=0*sin(pi/4)
!	q%j=1*sin(pi/4)
!	q%k=0*sin(pi/4)
!	do n=1,size(testq)
!		testq(n)=q.qxq.testq(n)
!	end do
!
!	q%i=-q%i
!	q%j=-q%j
!	q%k=-q%k
!	do n=1,size(testq)
!		testq(n)=testq(n).qxq.q
!	end do
!
!	testb=QtoXYZ(testq)
!	deallocate(testq)
!	allocate(test(6,a*a))
!	do n=1,size(testb,2)
!		test(1,n)=testb(1,n)+testb(2,n)
!		test(2,n)=testb(3,n)+(testb(1,n)-testb(2,n))/2
!		!test(3,n)=testb(2,n)-testb(1,n)
!		test(6,n)=testb(2,n)-testb(1,n)
!	end do
!	deallocate(testb)
!
!	do u=1,a
!		do v=1,a
!	!		test(4,a*(u-1)+v)=u
!	!		test(5,a*(u-1)+v)=v
!	!		test(4,a*(u-1)+v)=sin(2*pi*u/a)*(cos(2*pi*v/a)+2)
!	!		test(5,a*(u-1)+v)=cos(2*pi*u/a)*(cos(2*pi*v/a)+2)
!			test(3,a*(u-1)+v)=AngleToHue(2*pi*u/a)*(AngleToHue(2*pi*v/a-pi/2)+2)
!			test(4,a*(u-1)+v)=AngleToHue(2*pi*u/a-pi/2)*(AngleToHue(2*pi*v/a-pi/2)+2)
!			test(5,a*(u-1)+v)=AngleToHue(2*pi*v/a)
!	
!		end do
!	end do
!	!call linspaceAll2D(test,testb,4.0)
!	call plot(testb,[a,a])

end program display
