Program RunDecToHex																						!The program itself is just to run the function
	implicit none
	
	interface
		function DecToHex(dec)
			character, allocatable :: DecToHex(:)
			integer, intent(in) :: dec
		end function
	end interface
	
	integer input
	real output
	
	print *, "Input = "																					!Asking for input
	read(*,*) input
	
	write(*,*) int(log(real(input))/log(16.0))+1														!Just to see how long the output should be
	write(*,*) DecToHex(input)																			!Writing the output
	
end program RunDecToHex
Function DecToHex(dec)
	character, allocatable :: DecToHex(:)
	integer, intent(in) :: dec
	integer, allocatable :: DecToHexExp(:), DecToHexCof(:)
	integer :: DecToHexN
	
	allocate(DecToHex(int(log(real(dec))/log(16.0))+1),DecToHexExp(int(log(real(dec))/log(16.0))+1),&
		DecToHexCof(int(log(real(dec))/log(16.0))+1))													!Setting the size of the arrays
		
	DecToHexCof(:)=0																					!Setting arrays to zero
	DecToHexExp(:)=0
	
	do DecToHexN=1, int(log(real(dec))/log(16.0))+1														!Filling array with values to be used as exponents
		DecToHexExp(DecToHexN)=DecToHexN-1
	end do
	
	do while (sum(DecToHexCof(:)*16**DecToHexExp(:))<dec)												!Filling the coefficents
		DecToHexCof(int(log(real(dec-sum(DecToHexCof(:)*16**DecToHexExp(:))))/log(16.0))+1)=&
			DecToHexCof(int(log(real(dec-sum(DecToHexCof(:)*16**DecToHexExp(:))))/log(16.0))+1)+1
	end do
	
	do DecToHexN=1, int(log(real(dec))/log(16.0))+1														!Writing out as Characters
		if (DecToHexCof(DecToHexN)==10) then
			DecToHex(int(log(real(dec))/log(16.0))+2-DecToHexN)='A'
		else if (DecToHexCof(DecToHexN)==11) then
			DecToHex(int(log(real(dec))/log(16.0))+2-DecToHexN)='B'
		else if (DecToHexCof(DecToHexN)==12) then
			DecToHex(int(log(real(dec))/log(16.0))+2-DecToHexN)='C'
		else if (DecToHexCof(DecToHexN)==13) then
			DecToHex(int(log(real(dec))/log(16.0))+2-DecToHexN)='D'
		else if (DecToHexCof(DecToHexN)==14) then
			DecToHex(int(log(real(dec))/log(16.0))+2-DecToHexN)='E'
		else if (DecToHexCof(DecToHexN)==15) then
			DecToHex(int(log(real(dec))/log(16.0))+2-DecToHexN)='F'
		else
			DecToHex(int(log(real(dec))/log(16.0))+2-DecToHexN)=char(DecToHexCof(DecToHexN)+48)
		end if
	end do
	
end function DecToHex
