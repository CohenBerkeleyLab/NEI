FC=ifort

exe : read_test.f90 set_namelist_opts.o
	$(FC) set_namelist_opts.o read_test.f90 -o test.exe

set_namelist_opts.o : set_namelist_opts.f90
	$(FC) -c set_namelist_opts.f90

.PHONY: clean
clean:
	rm -f *.o *.mod *.exe

.PHONY: realclean
realclean:
	rm -f al2radm2.outp *.o *.mod *.exe wrfem* scratem
