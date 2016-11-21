MODULE READ_NAMELIST_OPTS

IMPLICIT NONE

    TYPE :: wps_config
        INTEGER         :: parent_id
        INTEGER         :: parent_grid_ratio
        INTEGER         :: isnest
        INTEGER         :: i_parent_start
        INTEGER         :: j_parent_start
        INTEGER         :: e_we
        INTEGER         :: e_sn
        REAL            :: dx
        REAL            :: dy
        CHARACTER(255)  :: map_proj
        REAL            :: ref_lat
        REAL            :: ref_lon
        REAL            :: truelat1
        REAL            :: truelat2
        REAL            :: stand_lon
    END TYPE wps_config

CONTAINS

!    SUBROUTINE READ_NAMELIST()
    SUBROUTINE READ_NAMELIST(config_flags)
!        wps_config intent(out) :: config_flags
        TYPE(wps_config), intent(out) :: config_flags

        CHARACTER(255)      :: namelist_file
        INTEGER             :: file_unit
        INTEGER             :: iostatus, ind
        CHARACTER(255)      :: line, optname, optval, fn
        INTEGER             :: tmpint
        REAL                :: tmpreal
        LOGICAL             :: exiterror
    
        namelist_file = "namelist.wps"
        file_unit = 2
        OPEN(unit = file_unit, file = namelist_file)

        DO
            READ(file_unit, '(A)', IOStat=iostatus) line
            IF(iostatus .NE. 0) THEN
                EXIT
            END IF

!           Get the option name. Must remove LEADING whitespace for comparison
!           to work.
            ind = SCAN(line, "=")
            optname = ADJUSTL(line(1:ind-1))
            optval = ADJUSTL(line(ind+1:255))

            ind = SCAN(optval, ",")
            IF ( ind .NE. 0 ) THEN
                optval = optval(1:ind-1)
            ENDIF


            IF ( optname .EQ. "parent_id" ) then
                READ( optval, *) tmpint
                config_flags%parent_id = tmpint
            ELSEIF ( optname .EQ. "parent_grid_ratio" ) then
                READ( optval, * ) tmpint
                config_flags%parent_grid_ratio = tmpint
            ELSEIF ( optname .EQ. "i_parent_start" ) then
                READ( optval, * ) tmpint
                config_flags%i_parent_start = tmpint
            ELSEIF ( optname .EQ. "j_parent_start" ) then
                READ( optval, * ) tmpint
                config_flags%j_parent_start = tmpint
            ELSEIF ( optname .EQ. "e_we" ) then
                READ( optval, * ) tmpint
                config_flags%e_we = tmpint
            ELSEIF ( optname .EQ. "e_sn" ) then
                READ( optval, * ) tmpint
                config_flags%e_sn = tmpint
            ELSEIF ( optname .EQ. "dx" ) then
                READ( optval, * ) tmpreal
                config_flags%dx = tmpreal
            ELSEIF ( optname .EQ. "dy" ) then
                READ( optval, * ) tmpreal
                config_flags%dy = tmpreal
            ELSEIF ( optname .EQ. "map_proj" ) then
                config_flags%map_proj = TRIM(optval)
            ELSEIF ( optname .EQ. "ref_lat" ) then
                READ(optval, * ) tmpreal
                config_flags%ref_lat = tmpreal
            ELSEIF ( optname .EQ. "ref_lon" ) then
                READ(optval, * ) tmpreal
                config_flags%ref_lon = tmpreal
            ELSEIF ( optname .EQ. "truelat1" ) then
                READ(optval, * ) tmpreal
                config_flags%truelat1 = tmpreal
            ELSEIF ( optname .EQ. "truelat2" ) then
                READ(optval, * ) tmpreal
                config_flags%truelat2 = tmpreal
            ELSEIF ( optname .EQ. "stand_lon" ) then
                READ(optval, * ) tmpreal
                config_flags%stand_lon = tmpreal
            ENDIF
            
        END DO

!       Two cleanup elements to do: make sure that truelat1 is north of truelat2
!       and set the isnest value
        IF ( config_flags%truelat1 .LT. config_flags%truelat2 ) then
            tmpreal = config_flags%truelat2
            config_flags%truelat2 = config_flags%truelat1
            config_flags%truelat1 = tmpreal
        ENDIF

        IF ( config_flags%parent_grid_ratio .GT. 1 ) then
            config_flags%isnest = 1
        ELSE
            config_flags%isnest = 0
        ENDIF

!       Some of the options that can be set independently in WPS are not totally
!       independent in the emiss program. Therefore, we will check them and
!       issue errors if any of the assumptions that emiss_v0x.F makes are
!       violated.

        exiterror = .false.        

        IF ( config_flags%dx .NE. config_flags%dy ) then
            WRITE(*,"(A, F12.2, A, F12.2, A)") "emiss_v0x.F assumes that dx == dy. This is not true in your namelist (dx = ", config_flags%dx, ", dy =", config_flags%dy, ")"
            exiterror = .true.
        ENDIF
        IF ( config_flags%map_proj .NE. "'lambert'" .AND. config_flags%map_proj .NE. 'polar' ) then
            WRITE(*,"(A, A, A)") "The map projection ", config_flags%map_proj, " is not supported. Only lambert and polar map projections are supported by emiss_v0x"
            exiterror = .true.
        ENDIF
        IF ( ABS(config_flags%stand_lon - config_flags%ref_lon) .GT. 0.01 ) then
            IF ( config_flags%map_proj .EQ. "'lambert'" ) then
                WRITE(*, "(A)") "For lambert projections emiss_v0x.F assumes that stand_lon == ref_lon. That is not the case in your namelist"
                WRITE(*, "(A, F8.2, A, F8.2, A)") "     (ref_lon = ", config_flags%ref_lon, ", stand_lon = ", config_flags%stand_lon, ")"
                exiterror = .true.
            ELSE
                WRITE(*, "(A)") "WARNING: stand_lon is not equal to ref_lon. You should check that your emissions are in the proper place before running WRF-Chem (if this is not important for polar projections you may ignore this message)"
            ENDIF
        ENDIF

        IF ( exiterror ) then
            WRITE(*, "(A)") "One or more issues with namelist.wps have been detected. Correct them (and reproduce meteorology as needed) then re-run emiss_v0x"
            STOP 1
        ENDIF

    END SUBROUTINE READ_NAMELIST

END MODULE
