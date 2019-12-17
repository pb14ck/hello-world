program Wake_up_Neo
	implicit none
	real :: a(5, 5), b(5, 5), max = 0, sum = 0, maxk = 0
	data a/1.0, 1.1, 1.2, 1.4, 1.1, 1.1, 1.2, 1.3, 1.2, 1.2, 1.2, 1.3, 1.4, 1.3, 1.3, 1.3, 1.0, 1.1, 1.2, 1.4, 1.1, 1.1, 1.2, 1.3, 1.2/
	integer i, j

	print*, 'input matrix:'	
	do i = 1, 5
		do j = 1, 5
			print '(" ", f5.3$)', a(i, j)
		end do
		print*, ' '	
	end do

	do i = 1, 5
		do j = 1, 5
		sum = sum + a(i, j) 
		if (max < sum) then
			max = sum
			maxk = j
		end if
		sum = 0	
		end do
	end do
	
	print*, "maxk = ", maxk

	do i = 1, 5
		do j = 1, 5
			b(i, j) = a(i, j)/maxk
		end do	
	end do
	print*, ' '
	do i = 1, 5
		do j = 1, 5
			print '(" 0", f4.3$)', b(i, j)
		end do
		print*, ' '	
	end do
			
end program Wake_up_Neo