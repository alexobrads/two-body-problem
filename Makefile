FC=gfortran
FFLAGS=-O3 -Wall -Wextra -fdefault-real-8
SRC= write.f90 equations.f90 begin.f90 methods.f90 main.f90
OBJ=${SRC:.f90=.o}


%.o: %.f90
		$(FC) $(FFLAGS) -o $@ -c $<

run: $(OBJ)
	$(FC) $(FFLAGS) -o $@ $(OBJ)

clean:
	rm *.o *.mod *.dat *snap*
