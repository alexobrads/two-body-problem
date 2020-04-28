module output
  implicit none

  integer, private :: icount = 0


contains

  subroutine data_out(results, time, timestep)
    real, intent(in) :: results(:,:)
    real, intent(in) :: time
    integer, intent(in) :: timestep

    integer :: n

    character(len=100) :: filename

    write(filename, "(a,i5.5)") 'snap',icount

    open(1, file=filename, status='replace')

    write(1,*)'x, y, vx, vy, ax, ay'
    write(1,*) time

    do n=1,timestep
      write(1,*) results(n,1), results(n,2), results(n,3), &
                  results(n,4), results(n,5), results(n,6)
    enddo

    close(1)

    icount = icount+1

  end subroutine data_out



  subroutine conservation_out(momentum, energy, times, t)
    real, intent(in) :: momentum(:), energy(:), times(:)
    integer, intent(in) :: t

    integer :: n

    open(1, file="conservation.dat", status='replace')
    write(1, *) "w, e, t"

    do n = 1,t
      write(1,*) momentum(n), energy(n), times(n)
    enddo
    close(1)

  end subroutine conservation_out

end module output
