# ==========================================================
# Magnetic Field Inside cilynder
# ==========================================================

# Name of program :w

echo "compile the program"
time gfortran -g -fcheck=all -Wall openmp.f90 -o openmp -fopenmp
echo "execute the program"
./openmp 

rm -r *.dSYM 
