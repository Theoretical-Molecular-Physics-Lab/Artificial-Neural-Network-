##### This folder contains the trained artificial neural networks and a code to load them.
###
###### To execute the trained neural network it is necessary to compile:
####### xx_ANN.f90 NN_XX.dat and dV_XX.dat.
###### where XX = O2, N2 or S0
#
###### However, these codes to run it is necessary to have your computer installed in the Fast Artificial Neural Network (FANN) Library. For such, the download can be realized in the page below. There, all instructions to install this library is given in details.

###### [link: FANN](http://leenissen.dk/fann/wp/)

###### To run do it

####### gfortran -o name.x -lfann -lm  fann.F03  xx_ANN.f90

###### Maybe be necessary to change fann.F03 to fann.f90
