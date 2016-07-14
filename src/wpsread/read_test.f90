PROGRAM READ_TEST

USE read_namelist_opts

TYPE(wps_config) :: config_flags

CALL read_namelist(config_flags)

WRITE(*,'(A, I8)') "parent_id =", config_flags%parent_id
WRITE(*,'(A, I8)') "parent_grid_ratio =", config_flags%parent_grid_ratio
WRITE(*,'(A, I8)') "i_parent_start =", config_flags%i_parent_start
WRITE(*,'(A, I8)') "j_parent_start =", config_flags%j_parent_start
WRITE(*,'(A, I8)') "e_we =", config_flags%e_we
WRITE(*,'(A, I8)') "e_sn =", config_flags%e_sn
WRITE(*,'(A, F8.2)') "dx =", config_flags%dx
WRITE(*,'(A, F8.2)') "dy =", config_flags%dy
WRITE(*,'(A, A)') "map_proj =", config_flags%map_proj
WRITE(*,'(A, F8.2)') "ref_lat =", config_flags%ref_lat
WRITE(*,'(A, F8.2)') "ref_lon =", config_flags%ref_lon
WRITE(*,'(A, F8.2)') "truelat1 =", config_flags%truelat1
WRITE(*,'(A, F8.2)') "truelat2 =", config_flags%truelat2
WRITE(*,'(A, F8.2)') "stand_lon =", config_flags%stand_lon


END PROGRAM
