module initialize
  use calcs
  implicit none

contains

    subroutine setup(x, y, Vx, Vy, Ax, Ay, e)
        real*8, intent(inout) :: x(:), y(:), Vx(:), Vy(:), Ax(:), Ay(:)
        real*8, intent(in) :: e

        x(1) = 1 - e
        y(1) = 0
        Vx(1) = 0
        Vy(1) = sqrt((1+e)/(1-e))
        Ax(1) = dv_dt(x(1), x(1), y(1))
        Ay(1) = dv_dt(y(1), x(1), y(1))

    end subroutine setup

end module initialize

