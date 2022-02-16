!I decided to randomy make a code to find a zero via Newton's method
!Code By: Tristan Stutsman
program Newton
	implicit none
	
	integer :: m,n
	real(16) :: x,x1
	
	interface
		Function F(x)
			real(16) :: F
			real(16), intent(in) :: x
		end Function 
		Function dFdy(x)
			real(16) :: dFdy
			real(16), intent(in) :: x
		end Function 
	end interface 
	
	write(*,*) "iterations = "			!ask for the max amount of itterations to try
	read(*,*) n
	write(*,*) "Starting x ="			!ask for a starting x value
	read(*,*) x
	m=0
	do while ((x1.ne.x).and.(m.lt.n))	!Newtons method iterations
		m=m+1
		x1=x
		x=x-(F(x)/dFdy(x))
	end do
	
	write(*,*) "iterations used = ",m	!Outputs
	write(*,*) "x = ",x
end program Newton

function F(x)							!I didn't find a way to execute strings, so implicit definitions will have to do
	implicit none
	
	real(16) :: F
	real(16), intent(in) :: x
	
	F=x**3-2*x+2						!Function
	
end function

function dFdy(x)						!I didn't find a way to execute strings, so implicit definitions will have to do
	implicit none
	
	real(16) :: dFdy
	real(16), intent(in) :: x
	
	dFdy=3*x**2-2						!Derivative of the function
	
end function
