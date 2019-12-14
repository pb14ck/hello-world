program lab7
implicit none
integer :: l = 40, n, i, k, j
real :: sup = -100000, inf = 100000, x0 = -2, xk=2, h = 0.1, u, v, t
real x(100), y(100)

n = aint((xk - x0)/h + 0.5) + 1
do i = 1, n
    u = x0 + (i - 1) * h
    v = f(u)
    x(i) = u
    y(i) = v
    print*,i, u, v
    if (sup < v) sup = v
    if (inf > v) inf = v
end do
t = (sup - inf)/l
print*, "sup:", sup, "inf:", inf, "t:", t
do i = 1, n
    print'("", f4.1$)', x0+h*(i-1)
    k = aint((y(i) - inf)/t + 0.5)
    if (abs(x0+h*(i-1)) < h/2.0) then
        do j = 1, aint(aint((sup - inf)/t + 0.5)/2.0)
            print'("", f4.1$)', inf+j*t*2
        end do
        print*, ''
    else
        do j = 1, k
            write(*,"(A2,$)") ''
        end do
        print*, '*'
    end if
end do
contains
real function f(x)
real x
f = -x**4+4*x**2
end function f
end program lab7
