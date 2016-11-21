FC=mpif90

ifeq ($(FC),mpif90)
    EMISSOPTS="-axSSE3 -free -convert big_endian"
else ifeq ($(FC),pgf90)
    EMISSOPTS="-w -byteswapio -Mfree -Mlfs"
else
    $(error The only tested fortran compilers for emiss_v04.F are mpif90 and pgf90. Please make the FC variable one of these)
endif

exe : emiss_v04.F set_namelist_opts.o
	$(FC) -axSSE3 -free -convert big_endian set_namelist_opts.o emiss_v04.F -o emiss_v04.exe
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
