program Tab 
real(8) :: x0 = 0.1, xk = 0.9, h = 0.1, E = 0.0000001, x, u, s, y, deltha
integer :: nmax = 100, imax, i, n

imax = aint((xk - x0)/h + 0.5) + 1
print*,'+===+=====+===+============+============+'
print*,'| i |  x  | n |      y     |   deltha   |'
print*,'+===+=====+===+============+============+'

do i = 1, imax
    x = x0 + (i-1) * h
    if(abs(x) <= 1) then
        u = -2*x*x
        s = -2*x*x
        do n = 2, nmax
            if (abs(u) > (E * abs(S))) then
                u = u*2*x/(1-n)
                s = s + u
            else
                y = x + s
                deltha = abs(y - x/exp(2*x))
                goto 187
            end if
        end do
        print*,'the end'
        stop
    else
        print*,'Warning'
        stop
    end if
187 print '(" |", i3.3, "|", f5.3, "|", i3.3, "|0", f11.10, "|0", f11.10, "|")', i, x, n, y, deltha
    print*,'+---+-----+---+------------+------------+'
end do
end program Tab