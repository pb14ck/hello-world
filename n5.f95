program Rage_of_CG
implicit none
    real y, x, x0, xk, dx, y0, yk, dy, a
    integer i, j, im, jm
    write(*,*) 'input a'
    read(*,*) a
    write(*,*) 'input x0, xk, dx'
    read(*,*) x0, xk, dx
    write(*,*) 'input y0, yk, dy'
    read(*,*) y0, yk, dy
    
    im=int((xk-x0)/dx+0.5)+1
    jm=int((yk-y0)/dy+0.5)+1


    do j=1, jm
        y=yk-(j-1)*dy
        do i=1, im
            x=x0+(i-1)*dx
            if((x.eq.0).and.(y.ne.0)) then
                write(*, '(a)',advance='no') '  |' !line of "Oy"
            else
                if((y.le.a).and.(y.gt.0)) then
                    if((x.gt.0).and.(x.le.a*log((a+sqrt(a*a-y*y))/y)-sqrt(a*a-y*y)).and.(x.le.a*sqrt(3.0))) then
                        write(*, '(a)',advance='no') '  *'
                    else
                        write(*, '(a)',advance='no') '  .'
                    end if
                else
                    write(*, '(a)',advance='no') '  /'
                end if    
            end if
        end do
        write(*,'(/a)')
    end do
end program Rage_of_CG
