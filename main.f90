program orbit

    use calcs
    use initialize
    use step
    use output

    implicit none
    real*8, parameter :: e = 0.8, dt = 0.05

    real*8 :: x(5000), y(5000), Vx(5000), Vy(5000), Ax(5000), Ay(5000)

    real*8 :: m(5000)
    real*8 :: eng(5000)

    integer :: t


    t = 1

    call setup(x, y, Vx, Vy, Ax, Ay, e)

    m(t) = momentum(x, y, Vx, Vy, t)
    eng(t) = energy(x, y, Vx, Vy, t)


    do t=2,5000

      !call rk4(results, dt, timestep)
      call leapfrog(x, y, Vx, Vy, Ax, Ay, dt, t)
      m(t) = momentum(x, y, Vx, Vy, t)
      eng(t) = energy(x, y, Vx, Vy, t)

    enddo

    call conservation_out(m, eng, t)
    call data_out(x, y, Vx, Vy, Ax, Ay, t)

end program orbit
