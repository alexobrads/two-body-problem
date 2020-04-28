module initialize
  use calcs
  implicit none

contains

    subroutine setup(results, e)
        real, intent(inout) :: results(:,:)
        real, intent(in) :: e

        !x
        results(1,1) = 1 - e
        !y
        results(1,2) = 0
        !Vx
        results(1,3) = 0
        !Vy
        results(1,4) = sqrt((1+e)/(1-e))
        !Ax
        results(1,5) = dv_dt(results(1,1), results(1,1), results(1,2))
        !Ay
        results(1,6) = dv_dt(results(1,2), results(1,1), results(1,2))

    end subroutine setup

end module initialize
