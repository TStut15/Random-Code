program DecToHexToFile
	implicit none
	
	integer :: n
	
	open(1, File='hello output')
	write(*,*) "write '256' or larger to exit"	!How to stop the code
	
	do while (n.ge.256)							!Run till input is bigger than one byre
	
		write(*,*) "input = "					!Ask for input
		read(*,*) n								!Read input
			if (n.ge.256)						!Exit loop before write if input is greater than one byte
				exit
			end if
		write(*,*) char(n)						!Write output to ternminal
		write(1,'(A)', advance="no") char(n)	!Write output to file
		
	end do
	close(1)
end program hello
