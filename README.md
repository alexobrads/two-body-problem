To run this code using the following commands

make clean
make
./run

you can then plot the results using

splash snap*


In the main.f90 file you are able to adjust

eccentricity - e
step size - dt

You can also select what sort of integrator to use.

Lines 36 and 37 have options for either rk4 or leapfrog 
