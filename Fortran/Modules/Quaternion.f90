module QuaternionMath
implicit none
type quaternion
	real :: r
	real :: i
	real :: j
	real :: k
end type quaternion

 interface operator(.qxq.)
 module procedure QMultipyQ
 end interface
 interface operator(.qtq.)
 module procedure QPlusQ
 end interface
 
contains

function QMultipyQ(Q1,Q2)
	type(quaternion), intent(in) :: Q1,Q2
	type(quaternion) :: QMultipyQ
	QMultipyQ%r=Q1%r*Q2%r-Q1%i*Q2%i-Q1%j*Q2%j-Q1%k*Q2%k
	QMultipyQ%i=Q1%r*Q2%i+Q1%i*Q2%r+Q1%j*Q2%k-Q1%k*Q2%j
	QMultipyQ%j=Q1%r*Q2%j-Q1%i*Q2%k+Q1%j*Q2%r+Q1%k*Q2%i
	QMultipyQ%k=Q1%r*Q2%k+Q1%i*Q2%j-Q1%j*Q2%i+Q1%k*Q2%r
end function QMultipyQ

function QPlusQ(Q1,Q2)
	type(quaternion), intent(in) :: Q1,Q2
	type(quaternion) :: QPlusQ
	QPlusQ%r=Q1%r+Q2%r
	QPlusQ%i=Q1%i+Q2%i
	QPlusQ%j=Q1%j+Q2%j
	QPlusQ%k=Q1%k+Q2%k
end function QPlusQ

function XYZtoQ(XYZ)
	real, allocatable, intent(in) :: XYZ(:,:)
	type(quaternion), allocatable :: XYZtoQ(:)
	integer :: n
	allocate(XYZtoQ(size(XYZ,2)))
	do n=1,size(XYZ,2)
		XYZtoQ(n)%r=0.0
		XYZtoQ(n)%i=XYZ(1,n)
		XYZtoQ(n)%j=XYZ(2,n)
		XYZtoQ(n)%k=XYZ(3,n)
	end do
end function XYZtoQ

function QtoXYZ(Q)
	type(quaternion), allocatable, intent(in) :: Q(:)
	real, allocatable :: QtoXYZ(:,:)
	integer :: n
	allocate(QtoXYZ(3,size(Q)))
	do n=1,size(Q)
		QtoXYZ(1,n)=Q(n)%i
		QtoXYZ(2,n)=Q(n)%j
		QtoXYZ(3,n)=Q(n)%k
	end do
end function QtoXYZ

function absQ(Q)
	type(quaternion), intent(in) :: Q
	real :: absQ
	absQ=sqrt(q%r**2+q%i**2+q%j**2+q%k**2)
end function absQ

end module