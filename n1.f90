program Num_of_Fib
implicit none 
real n
    write(*,*)'enter n'
    read(*,*) n
    write(*,*)'Answer:', Bine(n)
    
contains

real function Bine(x)
implicit none
real x, s 
s= sqrt(5.0)
    Bine= (((1+s)/2)**x-((1-s)/2)**x)/s
end function Bine
end program Num_of_Fib
