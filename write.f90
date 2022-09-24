module output

  implicit none

  integer, private :: icount = 0


contains

  subroutine data_out(x, y, Vx, Vy, Ax, Ay, t)
    real*8, intent(in) :: x(:), y(:), Vx(:), Vy(:), Ax(:), Ay(:)
    integer, intent(in) :: t
    integer :: n

    open(1, file="results", status='new')

    write(1,*)'time, x, y, vx, vy, ax, ay'

    do n=1,t
      write(1,*) n, x(n), y(n), Vx(n), &
                  Vy(n), Ax(n), Ay(n)
    enddo

    close(1)

    icount = icount+1

  end subroutine data_out



  subroutine conservation_out(momentum, energy, t)
    real*8, intent(in) :: momentum(:), energy(:)
    integer, intent(in) :: t

    integer :: n

    open(1, file="conservation.dat", status='replace')
    write(1, *) "w, e, t"

    do n = 1,t
      write(1,*) momentum(n), energy(n), n
    enddo
    close(1)

  end subroutine conservation_out

end module output
