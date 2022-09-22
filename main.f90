program orbit

    use output
    use calcs
    use initialize
    use movement

    implicit none

    real :: x(1000), y(1000), Vx(1000), Vy(1000), Ax(1000), Ay(1000)

    real :: results(1000, 6)
    real :: dt, time
    integer :: timestep
    real, parameter :: e = 0.7

    real :: momentum(1000)
    real :: energy(1000)
    real :: times(1000)


    time = 0
    timestep = 1
    dt = 0.05
    times(1) = time

    call setup(results, e)

    momentum(timestep) = ang_mom(results, timestep)
    energy(timestep) = eng(results, timestep)



    do timestep=2,1000

      time = timestep*dt

      !call rk4(results, dt, timestep)
      call leapfrog(results, dt, timestep)

      times(timestep) = time
      momentum(timestep) = ang_mom(results, timestep)
      energy(timestep) = eng(results, timestep)

    enddo

    call conservation_out(momentum, energy, times, timestep)
    call data_out(results, timestep)

end program orbit
