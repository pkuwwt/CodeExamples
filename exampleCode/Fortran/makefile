##
# @file      makefile
# @author    Mitch Richling <http://www.mitchr.me/>
# @Copyright Copyright 1996,1998,2006,2009,2012 by Mitch Richling.  All rights reserved.
# @brief     @EOL
# @Keywords  
# @Std       GNU Make

################################################################################################################################################################
# Sun Compiler
# FC       = sunf95
# FFLAGS   = -w4 -errtags=yes -erroff=COMMENT_0399,CAUTION_1103_SUN
# %: %.f90
# 	 $(FC)  $(FFLAGS) $(TARGET_ARCH) $< -o $@
# %: %.f95
# 	 $(FC)  $(FFLAGS) $(TARGET_ARCH) $< -o $@
# %: %.f03
# 	 $(FC)  $(FFLAGS) $(TARGET_ARCH) $< -o $@
# %: %.f08
# 	 $(FC)  $(FFLAGS) $(TARGET_ARCH) $< -o $@

################################################################################################################################################################
# GCC
FC       = gfortran
FFLAGS   = -Wall 
%: %.f90
	 $(FC)                       $(FFLAGS) $(TARGET_ARCH) $< -o $@
%: %.f95
	 $(FC) -pedantic --std=f95   $(FFLAGS) $(TARGET_ARCH) $< -o $@
%: %.f03
	 $(FC) -pedantic --std=f2003 $(FFLAGS) $(TARGET_ARCH) $< -o $@
%: %.f08
	 $(FC) -pedantic --std=f2008 $(FFLAGS) $(TARGET_ARCH) $< -o $@

################################################################################################################################################################
# INTEL
# FC = ifort
# %: %.f90
# 	rm -f tmp.f90; ln -s $< tmp.f90; $(FC) $(FFLAGS)             tmp.f90 -o $@; rm -f tmp.f90
# %: %.f95
# 	rm -f tmp.f90; ln -s $< tmp.f90; $(FC) $(FFLAGS) -std95 -e95 tmp.f90 -o $@; rm -f tmp.f90
# %: %.f03
# 	rm -f tmp.f90; ln -s $< tmp.f90; $(FC) $(FFLAGS) -std03 -e03 tmp.f90 -o $@; rm -f tmp.f90

# Put targets here
TARGETS  = func_opt_arg func_recursive overloading 
#TARGETS += prog_struct
TARGETS += case_statement loop_do loop_forall loopless_where
TARGETS += real_kinds real_ieee int_kind int_kind_c 
TARGETS += no_advance_print format  
TARGETS += proc_env proc_args 
TARGETS += array_dynamic array_dynamic2 array_sections array_elemental
TARGETS += array_reduction array_linear_alg array_init2003 array_init1995
TARGETS += pointers_linked_list
TARGETS += variable_decl

all : $(TARGETS)
	@echo Make Complete

clean :
	rm -rf a.out *~ *.bak *.mod $(TARGETS)
	@echo Clean Complete

prog_struct : mod_struct.f95 prog_struct.f95 
		 $(FC) -pedantic --std=f95   $(FFLAGS) $(TARGET_ARCH) mod_struct.f95 prog_struct.f95 -o $@
