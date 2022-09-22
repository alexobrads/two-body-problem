module initialize
  use calcs
  implicit none

contains

    subroutine setup(results, e)
        real, intent(inout) :: results(:,:)
        real, intent(in) :: e

        ! initialise the calculation at perihelon in order to 
        ! simplify the calculation of inital conditions


        ! x position at perihelon
        results(1,1) = 1 - e
        
        ! y position at perihelon
        results(1,2) = 0
       
        ! Vx no velocity in x direction at perihelon
        results(1,3) = 0
        
        ! Vy can be derived from
        ! e = c/a
        ! ry = a - ae
        ! rx = a + ae
        ! conservation of angular momentum mVyry = mVxrx
        ! equal to the gravitational potenial energy
        ! some fancy algebra 
        ! we dont care about GM or a for this
        results(1,4) = sqrt((1+e)/(1-e))
        
        !Ax = -GMx/r^3
        results(1,5) = dv_dt(results(1,1), results(1,1), results(1,2))
        
        !Ay = -Gmy/r^3
        results(1,6) = dv_dt(results(1,2), results(1,1), results(1,2))

    end subroutine setup

end module initialize
