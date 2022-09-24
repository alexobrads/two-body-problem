module calcs
  implicit none

contains

  function trig(x,y)
    real*8 :: trig, x, y

    trig = sqrt(x**2 + y**2)

  end function trig

  function dv_dt(p, x, y)
    real*8 :: dv_dt, p, x, y

    dv_dt = -1*p/trig(x,y)**3 

  end function dv_dt

  function momentum(x, y, Vx, Vy, t)
    real*8 :: momentum, x(:), y(:), Vx(:), Vy(:)
    integer :: t

    momentum = x(t)*Vy(t) - y(t)*Vx(t)

  end function momentum


  function energy(x, y, Vx, Vy, t)
    real*8 :: energy, x(:), y(:), Vx(:), Vy(:), r
    integer :: t

    r = trig(x(t), y(t))

    energy = 0.5*(Vx(t)**2 + Vy(t)**2) - 1/r

  end function energy

end module calcs
