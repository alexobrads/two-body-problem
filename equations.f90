module calcs
  implicit none

contains

  function trig(x,y)
    real :: trig, x, y

    trig = sqrt(x**2 + y**2)

  end function trig

  function dv_dt(pos, x, y)
    real :: dv_dt, pos, x, y

    dv_dt = -1*pos/trig(x,y)**3

  end function dv_dt


  function ang_mom(results, t)
    real :: ang_mom, results(:,:)
    integer :: t

    ang_mom = results(t, 1)*results(t,4) - results(t, 2)*results(t,3)

  end function ang_mom


  function eng(results, t)
    real :: eng, results(:,:), r
    integer :: t

    r = trig(results(t,1), results(t,2))

    eng = 0.5*(results(t, 3)**2 + results(t, 4)**2) - 1/r

  end function eng

  function dx_dt(vxy)
    real dx_dt, vxy

    dx_dt = vxy

  end function dx_dt


end module calcs
