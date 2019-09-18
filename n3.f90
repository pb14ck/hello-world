program Sum_of_Mass
implicit none
    real m, k, l, s
    write(*,*)'Input m, k and l'
    read(*,*)m, k, l
    s=sum(m, k, l)
    write(*,*)'sum=', s
  
 contains
    real function sum(m, k, l)
    implicit none
        real i, a, n, m, k, l 
        sum=0 
        write(*,*)'input n'
        read(*,*) n 
        
        do i=1, n
            write(*,*)'input a', i 
            read(*,*) a 
            if(mod(a, m).NE.0.AND.mod(a, k).NE.0.AND.mod(a, l).NE.0) then 
                sum = sum + a 
            end if 
        end do 
    end function sum
    
end program Sum_of_Mass
