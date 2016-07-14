FC=ifort

exe : emiss_v04.F set_namelist_opts.o
	mpif90 -axSSE3 -free -convert big_endian set_namelist_opts.o emiss_v04.F -o emiss_v04.exe
	cp -u namelist.wps namelist.wps.backup
	ln -sf ../../../WPS/namelist.wps

set_namelist_opts.o : ../wpsread/set_namelist_opts.f90
	$(FC) -c ../wpsread/set_namelist_opts.f90

.PHONY: clean
clean:
	rm -f *.o *.mod *.exe

.PHONY: realclean
realclean:
	rm -f al2radm2.outp *.o *.mod *.exe wrfem* scratem
