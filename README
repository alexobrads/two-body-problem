This code solves the equations of motion for a two body problem using either the leapfrog or rk4 intigration method. 


STRUCTURE:

main.f90
Runs the intigration loop.
eccentricity (line 13) and timestep (line 22) are parameters that can be adjusted.
Swapping between using the leapfrog and rk4 methods requires commenting/uncommenting lines 36/37

begin.f90 
Contains the sub routine for setting up the particles initial position, velocity, and acceleration.

equations.f90
Contains funcations for calculating veleocity, momentum, and energy.

methods.f90 
Contains the sub routines the for leapfrog and RK4 intigration methods.

write.f90 
Contains sub routines for writing out data files during calculation. 


RUN:

make clean
make
./run

PLOTTING:

splash snap*
