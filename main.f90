program orbit

    use calcs
    use initialize
    use step
    use output

    implicit none
    real*8, parameter :: e = 0.05, dt = 0.05

    real*8 :: x(1000), y(1000), Vx(1000), Vy(1000), Ax(1000), Ay(1000)

    real*8 :: m(1000)
    real*8 :: eng(1000)

    integer :: t


    t = 1

    call setup(x, y, Vx, Vy, Ax, Ay, e)

    m(t) = momentum(x, y, Vx, Vy, t)
    eng(t) = energy(x, y, Vx, Vy, t)


    do t=2,1000

      !call rk4(results, dt, timestep)
      call leapfrog(x, y, Vx, Vy, Ax, Ay, dt, t)
      m(t) = momentum(x, y, Vx, Vy, t)
      eng(t) = energy(x, y, Vx, Vy, t)

    enddo

    call conservation_out(m, eng, t)
    call data_out(x, y, Vx, Vy, Ax, Ay, t)

end program orbit
