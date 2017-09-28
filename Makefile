# Makefile for KGEN-generated kernel

FC := 
FC_0 := pgf90
FC_FLAGS := 
FC_FLAGS_SET_0 := -O3 -byteswapio -Mfree -r8 -m64 -Mnofma -Mvect=noaltcode -Minfo=accel -acc -ta=tesla

ALL_OBJS := mpas_atm_time_integration.o mpas_atm_dimensions.o mpas_kind_types.o mpas_atm_iau.o mpas_constants.o kernel_driver.o kgen_utils.o tprof_mod.o

run: build
	./kernel.exe

build: ${ALL_OBJS}
	${FC_0} ${FC_FLAGS_SET_0}   -o kernel.exe $^

mpas_atm_time_integration.o: mpas_atm_time_integration.F mpas_atm_dimensions.o mpas_kind_types.o mpas_atm_iau.o mpas_constants.o kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

mpas_atm_dimensions.o: mpas_atm_dimensions.F kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

mpas_kind_types.o: mpas_kind_types.F kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

mpas_atm_iau.o: mpas_atm_iau.F kgen_utils.o tprof_mod.o mpas_constants.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

mpas_constants.o: mpas_constants.F kgen_utils.o tprof_mod.o mpas_kind_types.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

kernel_driver.o: kernel_driver.f90 mpas_atm_time_integration.o mpas_atm_dimensions.o mpas_kind_types.o mpas_atm_iau.o mpas_constants.o kgen_utils.o tprof_mod.o
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

kgen_utils.o: kgen_utils.f90
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

tprof_mod.o: tprof_mod.f90
	${FC_0} ${FC_FLAGS_SET_0} -c -o $@ $<

clean:
	rm -f kernel.exe *.mod ${ALL_OBJS}
