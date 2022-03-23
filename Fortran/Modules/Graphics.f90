module Graphics
implicit none

contains

subroutine plot(plotIn,plotSize)	
	real, allocatable, intent(inout) :: plotIn(:,:)
	integer, dimension(2), intent(in) :: plotSize
	if (size(plotIn,1).eq.6) then
		call plotCI(plotIn,plotSize)
	else if (size(plotIn,1).eq.5) then
		call plotC(plotIn,plotSize)
	else if (size(plotIn,1).eq.4) then
		call plot3I(plotIn,plotSize)
	else if (size(plotIn,1).eq.3) then
		call plot3(plotIn,plotSize)
	else if (size(plotIn,1).eq.2) then
		call plot2(plotIn,plotSize)
	else
		write(*,*) 'Array format error'
	end if
end subroutine plot

subroutine plot2(plotIn,plotSize)
	
	real, allocatable, intent(inout) :: plotIn(:,:)
	integer, dimension(2), intent(in) :: plotSize
	integer, allocatable :: plotOut(:,:,:)
	integer :: n,m,o
	
	plotIn(1,:)=plotIn(1,:)-minval(plotIn(1,:))+1
	plotIn(2,:)=-plotIn(2,:)-minval(-plotIn(2,:))+1
	plotIn(1,:)=plotSize(1)*plotIn(1,:)/maxval(plotIn(1,:))
	plotIn(2,:)=plotSize(2)*plotIn(2,:)/maxval(plotIn(2,:))
	plotIn(1,:)=ceiling(plotIn(1,:))
	plotIn(2,:)=ceiling(plotIn(2,:))
	
	allocate(plotOut(1,plotSize(1),plotSize(2)))
	plotOut(:,:,:)=0

	do n=1,size(plotIn,2)
		m=plotIn(1,n)
		o=plotIn(2,n)
		plotOut(1,m,o)=65535
	end do
	
	open(1, File='Plot Output')
	
	write(1,'(A)') 'P5'
	write(1,'(I5)') plotSize(1)
	write(1,'(I5)') plotSize(2)
	write(1,'(I5)') 65535

	do m=1,plotSize(2)
		do n=1,plotSize(1)
			write(1,'(A,A)',advance="no") char(floor(plotOut(1,n,m)/256.0)),char(int(plotOut(1,n,m)))
		end do
	end do

	close(1)

end subroutine plot2

subroutine plot3(plotIn,plotSize)
	
	real, allocatable, intent(inout) :: plotIn(:,:)
	integer, dimension(2), intent(in) :: plotSize
	integer, allocatable :: plotOut(:,:,:)
	integer :: n,m,o
	
	plotIn(1,:)=plotIn(1,:)-minval(plotIn(1,:))+1
	plotIn(2,:)=-plotIn(2,:)-minval(-plotIn(2,:))+1
	plotIn(1,:)=plotSize(1)*plotIn(1,:)/maxval(plotIn(1,:))
	plotIn(2,:)=plotSize(2)*plotIn(2,:)/maxval(plotIn(2,:))
	plotIn(1,:)=ceiling(plotIn(1,:))
	plotIn(2,:)=ceiling(plotIn(2,:))

	plotIn(3,:)=plotIn(3,:)-minval(plotIn(3,:))
	plotIn(3,:)=65535*plotIn(3,:)/maxval(plotIn(3,:))

	allocate(plotOut(1,plotSize(1),plotSize(2)))
	plotOut(:,:,:)=0

	do n=1,size(plotIn,2)
		m=plotIn(1,n)
		o=plotIn(2,n)
		if (nint(plotIn(3,n)).gt.plotOut(1,m,o)) then
			plotOut(1,m,o)=nint(plotIn(3,n))
		end if
	end do
	
	open(1, File='Plot Output')
	
	write(1,'(A)') 'P5'
	write(1,'(I5)') plotSize(1)
	write(1,'(I5)') plotSize(2)
	write(1,'(I5)') 65535

	do m=1,plotSize(2)
		do n=1,plotSize(1)
			write(1,'(A,A)',advance="no") char(floor(plotOut(1,n,m)/256.0)),char(int(plotOut(1,n,m)))
		end do
	end do

	close(1)
	
end subroutine plot3

subroutine plot3I(plotIn,plotSize)
	
	real, allocatable, intent(inout) :: plotIn(:,:)
	integer, dimension(2), intent(in) :: plotSize
	integer, allocatable :: plotOut(:,:,:)
	integer :: n,m,o
	
	plotIn(1,:)=plotIn(1,:)-minval(plotIn(1,:))+1
	plotIn(2,:)=-plotIn(2,:)-minval(-plotIn(2,:))+1
	plotIn(1,:)=plotSize(1)*plotIn(1,:)/maxval(plotIn(1,:))
	plotIn(2,:)=plotSize(2)*plotIn(2,:)/maxval(plotIn(2,:))
	plotIn(1,:)=ceiling(plotIn(1,:))
	plotIn(2,:)=ceiling(plotIn(2,:))

	plotIn(3,:)=plotIn(3,:)-minval(plotIn(3,:))
	plotIn(3,:)=65535*plotIn(3,:)/maxval(plotIn(3,:))
	plotIn(4,:)=plotIn(4,:)-minval(plotIn(4,:))
	plotIn(4,:)=255*plotIn(4,:)/maxval(plotIn(4,:))
	allocate(plotOut(2,plotSize(1),plotSize(2)))
	plotOut(:,:,:)=0

	do n=1,size(plotIn,2)
		m=plotIn(1,n)
		o=plotIn(2,n)
		if (nint(plotIn(4,n)).gt.plotOut(2,m,o)) then
			plotOut(1,m,o)=nint(plotIn(3,n))
			plotOut(2,m,o)=plotIn(4,n)
		end if
	end do
	
	open(1, File='Plot Output')
	
	write(1,'(A)') 'P5'
	write(1,'(I5)') plotSize(1)
	write(1,'(I5)') plotSize(2)
	write(1,'(I5)') 65535

	do m=1,plotSize(2)
		do n=1,plotSize(1)
			write(1,'(A,A)',advance="no") char(floor(plotOut(1,n,m)/256.0)),char(int(plotOut(1,n,m)))
		end do
	end do

	close(1)
	
end subroutine plot3I

subroutine plotC(plotIn,plotSize)
	
	real, allocatable, intent(inout) :: plotIn(:,:)
	integer, dimension(2), intent(in) :: plotSize
	integer, allocatable :: plotOut(:,:,:)
	real :: color
	integer :: n,m,o
	
	plotIn(1,:)=plotIn(1,:)-minval(plotIn(1,:))+1
	plotIn(2,:)=-plotIn(2,:)-minval(-plotIn(2,:))+1
	plotIn(1,:)=plotSize(1)*plotIn(1,:)/maxval(plotIn(1,:))
	plotIn(2,:)=plotSize(2)*plotIn(2,:)/maxval(plotIn(2,:))
	plotIn(1,:)=ceiling(plotIn(1,:))
	plotIn(2,:)=ceiling(plotIn(2,:))

	color=minval(plotIn(3:5,:))
	plotIn(3,:)=plotIn(3,:)-color
	plotIn(4,:)=plotIn(4,:)-color
	plotIn(5,:)=plotIn(5,:)-color
	color=maxval(plotIn(3:5,:))
	plotIn(3,:)=65535*plotIn(3,:)/color
	plotIn(4,:)=65535*plotIn(4,:)/color
	plotIn(5,:)=65535*plotIn(5,:)/color

	allocate(plotOut(3,plotSize(1)+1,plotSize(2)+1))
	plotOut(:,:,:)=0

	do n=1,size(plotIn,2)
		m=plotIn(1,n)
		o=plotIn(2,n)
		if (nint(sum(plotIn(:,n))).gt.sum(plotOut(:,m,o))) then
			plotOut(1,m,o)=nint(plotIn(3,n))
			plotOut(2,m,o)=nint(plotIn(4,n))
			plotOut(3,m,o)=nint(plotIn(5,n))
		else if (nint(sum(plotIn(:,n))).eq.sum(plotOut(:,m,o))) then
			plotOut(1,m,o)=nint((plotIn(3,n)+plotOut(1,m,o))/2)
			plotOut(2,m,o)=nint((plotIn(4,n)+plotOut(2,m,o))/2)
			plotOut(3,m,o)=nint((plotIn(5,n)+plotOut(3,m,o))/2)
		end if
	end do

	open(1, File='Plot Output')
	
	write(1,'(A)') 'P6'
	write(1,'(I5)') plotSize(1)
	write(1,'(I5)') plotSize(2)
	write(1,'(I5)') 65535

	do m=1,plotSize(2)
		do n=1,plotSize(1)
			write(1,'(A,A,A,A,A,A)',advance="no") char(floor(plotOut(1,n,m)/256.0)),char(int(plotOut(1,n,m))),&
				char(floor(plotOut(2,n,m)/256.0)),char(int(plotOut(2,n,m))),char(floor(plotOut(3,n,m)/256.0)),char(int(plotOut(3,n,m)))
		end do
	end do

	close(1)
	
end subroutine plotC

subroutine plotCI(plotIn,plotSize)
	
	real, allocatable, intent(inout) :: plotIn(:,:)
	integer, dimension(2), intent(in) :: plotSize
	integer, allocatable :: plotOut(:,:,:)
	real :: color
	integer :: n,m,o
	
	plotIn(1,:)=plotIn(1,:)-minval(plotIn(1,:))+1
	plotIn(2,:)=-plotIn(2,:)-minval(-plotIn(2,:))+1
	plotIn(1,:)=plotSize(1)*plotIn(1,:)/maxval(plotIn(1,:))
	plotIn(2,:)=plotSize(2)*plotIn(2,:)/maxval(plotIn(2,:))
	plotIn(1,:)=ceiling(plotIn(1,:))
	plotIn(2,:)=ceiling(plotIn(2,:))

	color=minval(plotIn(3:5,:))
	plotIn(3,:)=plotIn(3,:)-color
	plotIn(4,:)=plotIn(4,:)-color
	plotIn(5,:)=plotIn(5,:)-color
	color=maxval(plotIn(3:5,:))
	plotIn(3,:)=65535*plotIn(3,:)/color
	plotIn(4,:)=65535*plotIn(4,:)/color
	plotIn(5,:)=65535*plotIn(5,:)/color
	
	plotIn(6,:)=plotIn(6,:)-minval(plotIn(6,:))
	plotIn(6,:)=255*plotIn(6,:)/maxval(plotIn(6,:))

	allocate(plotOut(4,plotSize(1)+1,plotSize(2)+1))
	plotOut(:,:,:)=0

	do n=1,size(plotIn,2)
		m=plotIn(1,n)
		o=plotIn(2,n)
		if (plotIn(6,n).gt.plotOut(4,m,o)) then
			plotOut(1,m,o)=nint(plotIn(3,n))
			plotOut(2,m,o)=nint(plotIn(4,n))
			plotOut(3,m,o)=nint(plotIn(5,n))
			plotOut(4,m,o)=plotIn(6,n)
		else if (plotIn(6,n).eq.plotOut(4,m,o)) then
			plotOut(1,m,o)=nint((plotIn(3,n)+plotOut(1,m,o))/2)
			plotOut(2,m,o)=nint((plotIn(4,n)+plotOut(2,m,o))/2)
			plotOut(3,m,o)=nint((plotIn(5,n)+plotOut(3,m,o))/2)
			plotOut(4,m,o)=plotIn(6,n)
		end if
	end do

	open(1, File='Plot Output')
	
	write(1,'(A)') 'P6'
	write(1,'(I5)') plotSize(1)
	write(1,'(I5)') plotSize(2)
	write(1,'(I5)') 65535

	do m=1,plotSize(2)
		do n=1,plotSize(1)
			write(1,'(A,A,A,A,A,A)',advance="no") char(floor(plotOut(1,n,m)/256.0)),char(int(plotOut(1,n,m))),&
				char(floor(plotOut(2,n,m)/256.0)),char(int(plotOut(2,n,m))),char(floor(plotOut(3,n,m)/256.0)),char(int(plotOut(3,n,m)))
		end do
	end do

	close(1)
	
end subroutine plotCI

function AngleToHue(Th)
	real, intent(in) :: Th
	real :: AngleToHue
	real :: pi, Ang
	pi=3.1415926535897932384626433832795028841971693993751058209749
	Ang=3*Th/pi
	AngleToHue=(abs(modulo(Ang,6.0)-3)+abs(modulo(Ang+2,6.0)-3)-2)/2.0
end function AngleToHue

function linspace(st,en,leng)
	real, intent(in) :: st, en
	integer, intent(in) :: leng
	real, allocatable :: linspace(:)
	integer :: n
	
	allocate(linspace(leng))
	do n=0, leng-1
		linspace(n+1)=st+n*(en-st)/leng
	end do
end function linspace

subroutine linspaceAll2D(listIn,listOut,S)
	real, allocatable, intent(in) :: listIn(:,:)
	real, intent(in) :: S
	real, allocatable, intent(out) :: listOut(:,:)
	integer :: n, m, o, p
	m=0
	do n=2,size(listIn,2)
		m=m+S*ceiling(sqrt((listIn(1,n)-listIn(1,n-1))**2+(listIn(2,n)-listIn(2,n-1))**2))
	end do
	allocate(listOut(size(listIn,1),m))
	o=1
	do n=2,size(listIn,2)
		p=S*ceiling(sqrt((listIn(1,n)-listIn(1,n-1))**2+(listIn(2,n)-listIn(2,n-1))**2))
		do m=1,size(listIn,1)
			listOut(m,o:)=linspace(listIn(m,n-1),listIn(m,n),p)
		end do
		o=o+p
	end do
	
	
end subroutine linspaceAll2D

end module Graphics