program Euler
implicit none
    real(8) eps, c, c1, n
        write(*,*) 'input'
        read(*,*) eps
        n=2
        c=1.5-log(2.0)
        c1=1
        do while(c1-c.GT.eps)
            c1=c
            n=n+1
            c=c+1/n+log(n-1)-log(n)
        end do
        write(*,*) 'the needed n is', n
        write(*,*) 'The const of Euler is', c
        
end program Euler