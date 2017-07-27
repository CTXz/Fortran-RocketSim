! Copyright 2017 Patrick Pedersen <ctx.xda@gmail.com>
!
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
!
!     http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

! Author:                   Patrick Pedersen
! Last modification Date:   7/27/2017 12:15

! Compiled with:            gfortran 7.1.1 x86_64, part of the GNU Compiler Collection
! Written and Tested on:    GNU/Linux 4.12.0-next-20170714-ARCH+
! Build:                    See README.md

! DESCRIPTION:
! The following file stores the main program code for the Rocket Trajectory Simulator.

! Exit application with error message
SUBROUTINE ERR(msg)
  CHARACTER(LEN=*), INTENT(IN) :: msg
  PRINT '(A)', msg
  STOP
END SUBROUTINE ERR

! Exit application with usage guide
SUBROUTINE USAGE_ERR()
  PRINT '(A)', "rocketsim [-h | --help] to display this usage guide"
  PRINT '(A)', "rocketsim [-v <velocity>] [-a <angle>] [-g <gravity>] [-o <output>]"                                          &
  // NEW_LINE('A') // NEW_LINE('A') //                                                                                        &
  "Arguments: "                                                                                                               &
  // NEW_LINE('A') //                                                                                                         &
  "-v <velocity> " // char(9) // "Defines the initial velocity in m/s" // char(9) // char(9) // "Default value: 88"           &
  // NEW_LINE('A') //                                                                                                         &
  "-a <angle>" // char(9) // "Defines the initial angle" // char(9) // char(9) // char(9) // "Default value: 45"              &
  // NEW_LINE('A') //                                                                                                         &
  "-g <gravity>" // char(9) // "Defines applied gravity" // char(9) // char(9) // char(9) // char(9) // "Default value: 9.81" &
  // NEW_LINE('A') //                                                                                                         &
  "-o <output>" // char(9) // "Defines file to which table is writtent to" // char(9) // "Default value: Table.txt"
STOP
END SUBROUTINE usage_err

! - Main - !
PROGRAM RocketSim
  USE calc                ! Import mathematical functions
  USE table               ! Import abstract table subroutines

  IMPLICIT NONE

  INTEGER                 :: errno, I = 1

  REAL                    :: land_time, v_x, v_y, x, y = 1, g = 9.81, ang = 45, v = 88, time = 0

  CHARACTER(LEN=255)      :: ARG, v_str, ang_str, g_str, fname = "table.txt"
  CHARACTER(LEN=512000)   :: table_str

  DO WHILE (I <= IARGC())
    CALL GETARG(i, arg)

    ! Check if parameter has argument assigned
    IF (I + 1 <= IARGC()) THEN
      SELECT CASE(arg)

      ! Set custom velocity
      CASE ('-v')
        CALL GETARG(I + 1, v_str)
        READ(v_str, *, IOSTAT=ERRNO) v

        IF (ERRNO /= 0 .OR. v <= 0) THEN
          CALL ERR("Invalid velocity specified")
        END IF

      ! Set custom anglechar
      CASE ('-a')
        CALL GETARG(I + 1, ang_str)
        READ(ang_str, *, IOSTAT=ERRNO) ang

        IF (ERRNO /= 0 .OR. ang <= 0) THEN
          CALL ERR("Invalid angle specified")
        END IF

      ! Set custom gravity
      CASE ('-g')
        CALL GETARG(I + 1, g_str)
        READ(ang_str, *, IOSTAT=ERRNO) g

        IF (ERRNO /= 0 .OR. g <= 0) THEN
          CALL ERR("Invalid gravity specified")
        END IF

      ! Set custom output file name
      CASE ('-o')
        CALL GETARG(I + 1, fname)

        IF (TRIM(fname) == '') THEN
          CALL ERR("Invalid filename")
        END IF

      ! Invalid parameter
      CASE DEFAULT
        PRINT '(A)', "No such option: " // arg
        CALL USAGE_ERR()
      END SELECT

      I = I + 2 ! Move to next param

    ELSE
      PRINT '(A)', "Missing parameter argument"
      CALL USAGE_ERR()
    END IF
  END DO

  ! Convert from DEG to RAD
  ang = calc_deg2rad(ang)

  ! Initialize abstract table
  CALL TABLE_INIT(table_str)

  land_time = calc_land(g, ang, v)

  DO WHILE (.TRUE.)

    ! Avoid running into negatives
    IF (time > land_time) time = land_time

    ! Velocity
    v_x = calc_vx(ang, v)          ! Obtain horizontal velocity
    v_y = calc_vy(g, ang, v, time) ! Obtain vertical velocity

    ! Co-ordinates
    x   = calc_x(ang, v, time)     ! Obtain horizontal co-ordinates
    y   = calc_y(g, ang, v, time)  ! Obtain vertical co-ordinates

    ! Add row of data to table
    CALL TABLE_ADD_ROW(table_str, time, v_x, v_y, x, y)

    ! Space ship has landed
    IF (time == land_time) EXIT

    time = time + 0.1
  END DO

  CALL TABLE_EXPORT(table_str, fname)
END PROGRAM RocketSim
