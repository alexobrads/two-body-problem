module movement
  use calcs
  implicit none


contains



  subroutine leapfrog(results, dt, t)

    real, intent(inout) :: results(:,:)
    real, intent(in) :: dt
    integer, intent(in) :: t

    real :: x0, y0, vx0, vy0, ax0, ay0
    real :: x_new, y_new, vx_new, vy_new
    real :: accel_x_new, accel_y_new

    x0 = results(t-1,1)
    y0 = results(t-1,2)
    vx0 = results(t-1,3)
    vy0 = results(t-1,4)
    ax0 = results(t-1,5)
    ay0 = results(t-1,6)

    !x
    x_new = x0 + vx0*dt + 0.5*ax0*dt**2
    y_new = y0 + vy0*dt + 0.5*ay0*dt**2

    !accel_new
    accel_x_new = dv_dt(x_new, x_new, y_new)
    accel_y_new = dv_dt(y_new, x_new, y_new)

    !v
    vx_new = vx0 + 0.5*(ax0 + accel_x_new)*dt
    vy_new = vy0 + 0.5*(ay0 + accel_y_new)*dt

    results(t,1) = x_new
    results(t,2) = y_new
    results(t,3) = vx_new
    results(t,4) = vy_new
    results(t,5) = accel_x_new
    results(t,6) = accel_y_new

  end subroutine leapfrog


  subroutine rk4(results, dt, t)

    real, intent(inout) :: results(:,:)
    real, intent(in) :: dt
    integer, intent(in) :: t

    real :: x0, y0, vx0, vy0
    real :: k1, l1, kv1, lv1
    real :: k2, l2, kv2, lv2
    real :: k3, l3, kv3, lv3
    real :: k4, l4, kv4, lv4
    real :: x_new, y_new, vx_new, vy_new
    real :: ax_new, ay_new

    x0 = results(t-1, 1)
    vx0 = results(t-1, 3)

    y0 = results(t-1, 2)
    vy0 = results(t-1, 4)

    kv1 = dt*dv_dt(x0, x0, y0)
    lv1 = dt*dv_dt(y0, x0, y0)
    k1 = dt*dx_dt(vx0)
    l1 = dt*dx_dt(vy0)

    kv2 = dt*dv_dt(x0+0.5*k1, x0+0.5*k1, y0+0.5*l1)
    lv2 = dt*dv_dt(y0+0.5*l1, x0+0.5*k1, y0+0.5*l1)
    k2 = dt*dx_dt(vx0+0.5*kv1)
    l2 = dt*dx_dt(vy0+0.5*lv1)

    kv3 = dt*dv_dt(x0+0.5*k2, x0+0.5*k2, y0+0.5*l2)
    lv3 = dt*dv_dt(y0+0.5*l2, x0+0.5*k2, y0+0.5*l2)
    k3 = dt*dx_dt(vx0+0.5*kv2)
    l3 = dt*dx_dt(vy0+0.5*lv2)

    kv4 = dt*dv_dt(x0+k3, x0+k3, y0+l3)
    lv4 = dt*dv_dt(y0+l3, x0+k3, y0+l3)
    k4 = dt*dx_dt(vx0+kv3)
    l4 = dt*dx_dt(vy0+lv3)

    x_new = x0 + (k1 + 2*k2 + 2*k3 + k4)/6
    y_new = y0 + (l1 + 2*l2 + 2*l3 + l4)/6
    vx_new = vx0 + (kv1 + 2*kv2 + 2*kv3 + kv4)/6
    vy_new = vy0 + (lv1 + 2*lv2 + 2*lv3 + lv4)/6
    ax_new = dv_dt(x_new, x_new, y_new)
    ay_new = dv_dt(y_new, x_new, y_new)


    results(t, 1) = x_new
    results(t, 2) = y_new
    results(t, 3) = vx_new
    results(t, 4) = vy_new
    results(t, 5) = ax_new
    results(t, 6) = ay_new

  end subroutine rk4



end module movement
