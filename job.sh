# ==========================================================
# Magnetic Field Inside cilynder
# ==========================================================

# Name of program 

time gfortran -g -fcheck=all -Wall campo_B.f90 -o campo_B -fopenmp
./campo_B

rm -r *.dSYM 