# =============================

# =============================

PROGRAM= magnetic_cylinder
COMPILER= gfortran
NAME= campo

# Build compiler flas
CFLAGS= -g -fcheck=all


# Use OpenMP parallelization ?
OPENMP = Y


ifeq ($(OPENMP),Y)
CFLAGS+= -fopenmp
endif 


all: compile run
compile: campo_B.f90
	@$(COMPILER) $(CFLAGS) $< -o $(NAME) 

run: compile
	@./$(NAME)
	@rm -r $(NAME) *.dSYM 
