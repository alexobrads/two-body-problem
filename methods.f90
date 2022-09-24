module step
  use calcs
  implicit none


contains

  subroutine leapfrog(x, y, Vx, Vy, Ax, Ay, dt, t)

    real*8, intent(inout) :: x(:), y(:), Vx(:), Vy(:), Ax(:), Ay(:)
    real*8, intent(in) :: dt
    integer, intent(in) :: t

    real*8 :: x0, y0, vx0, vy0, ax0, ay0
    real*8 :: x_new, y_new, vx_new, vy_new
    real*8 :: accel_x_new, accel_y_new

    x0 = x(t-1)
    y0 = y(t-1)
    vx0 = Vx(t-1)
    vy0 = Vy(t-1)
    ax0 = Ax(t-1)
    ay0 = Ay(t-1)

    x_new = x0 + vx0*dt + 0.5*ax0*dt**2
    y_new = y0 + vy0*dt + 0.5*ay0*dt**2

    accel_x_new = dv_dt(x_new, x_new, y_new)
    accel_y_new = dv_dt(y_new, x_new, y_new)

    vx_new = vx0 + 0.5*(ax0 + accel_x_new)*dt
    vy_new = vy0 + 0.5*(ay0 + accel_y_new)*dt

    x(t) = x_new
    y(t) = y_new
    Vx(t) = vx_new
    Vy(t) = vy_new
    Ax(t) = accel_x_new
    Ay(t) = accel_y_new

  end subroutine leapfrog


  subroutine rk4(x, y, Vx, Vy, Ax, Ay, dt, t)

    real*8, intent(inout) :: x(:), y(:), Vx(:), Vy(:), Ax(:), Ay(:)
    real*8, intent(in) :: dt
    integer, intent(in) :: t

    real*8 :: x0, y0, vx0, vy0
    real*8 :: k1, l1, kv1, lv1
    real*8 :: k2, l2, kv2, lv2
    real*8 :: k3, l3, kv3, lv3
    real*8 :: k4, l4, kv4, lv4
    real*8 :: x_new, y_new, vx_new, vy_new
    real*8 :: ax_new, ay_new

    x0 = x(t-1)
    vx0 = Vx(t-1)

    y0 = y(t-1)
    vy0 = Vy(t-1)

    kv1 = dt*dv_dt(x0, x0, y0)
    lv1 = dt*dv_dt(y0, x0, y0)
    k1 = dt*vx0
    l1 = dt*vy0

    kv2 = dt*dv_dt(x0+0.5*k1, x0+0.5*k1, y0+0.5*l1)
    lv2 = dt*dv_dt(y0+0.5*l1, x0+0.5*k1, y0+0.5*l1)
    k2 = dt*(vx0+0.5*kv1)
    l2 = dt**(vy0+0.5*lv1)

    kv3 = dt*dv_dt(x0+0.5*k2, x0+0.5*k2, y0+0.5*l2)
    lv3 = dt*dv_dt(y0+0.5*l2, x0+0.5*k2, y0+0.5*l2)
    k3 = dt*(vx0+0.5*kv2)
    l3 = dt*(vy0+0.5*lv2)

    kv4 = dt*dv_dt(x0+k3, x0+k3, y0+l3)
    lv4 = dt*dv_dt(y0+l3, x0+k3, y0+l3)
    k4 = dt*(vx0+kv3)
    l4 = dt*(vy0+lv3)

    x_new = x0 + (k1 + 2*k2 + 2*k3 + k4)/6
    y_new = y0 + (l1 + 2*l2 + 2*l3 + l4)/6
    vx_new = vx0 + (kv1 + 2*kv2 + 2*kv3 + kv4)/6
    vy_new = vy0 + (lv1 + 2*lv2 + 2*lv3 + lv4)/6
    ax_new = dv_dt(x_new, x_new, y_new)
    ay_new = dv_dt(y_new, x_new, y_new)


    x(t) = x_new
    y(t) = y_new
    Vx(t) = vx_new
    Vy(t) = vy_new
    Ax(t) = ax_new
    Ay(t) = ay_new

  end subroutine rk4



end module step
