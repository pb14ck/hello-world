program One_point_crossing
implicit none

    real a1, b1, c1
    real a2, b2, c2
    real a3, b3, c3
    real m

    write(*, *) 'Enter a1, b1, c1'
    read(*, *) a1, b1, c1
    write(*, *) 'Enter a2, b2, c2'
    read(*, *) a2, b2, c2
    write(*, *) 'Enter a3, b3, c3'
    read(*, *) a3, b3, c3

    m = a1*b2*c3 + b1*c2*a3 + c1*a2*b3 - a1*c2*b3 - b1*a2*c3 - c1*b2*a3

    if ( m == 0 ) then
        write(*, *) 'There IS one-point crossing here'
    else
        write(*, *) 'There is NO one-point crossing here'
    endif
end program One_point_crossing
