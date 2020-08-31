# =============================
# Makefile to run magnetic cylinder problem
# =============================

PROGRAM= magnetic_cylinder
COMPILER= gfortran
NAME= campo

# Build compiler flas
CFLAGS= -g -fcheck=all -Wall


# Use OpenMP parallelization ?
OPENMP = N


ifeq ($(OPENMP),Y)
CFLAGS+= -fopenmp
endif 


all: compile run

compile: campo_B.f90
	@$(COMPILER) $(CFLAGS) $< -o $(NAME) 

run: compile
	@time ./$(NAME)
	@rm -r $(NAME) *.dSYM
	@python plots.py

clean: 
	@rm *.png *.dat 
