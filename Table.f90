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
! Last modification Date:   7/26/2017 00:17

! Compiled with:            gfortran 7.1.1 x86_64, part of the GNU Compiler Collection
! Written and Tested on:    GNU/Linux 4.12.0-next-20170714-ARCH+
! Build:                    See README.md

! DESCRIPTION:
! The following file provides the necessary subroutines to efficiently generate a gnuplot compatible table of data.
! The term "abstract table" is used to describe a large string (512kb in size), storing the entire table until the verry end,
! where it is dumpled as one big chunk onto a file. This way we ensure that any table modifications or expansions take place on RAM,
! rather than on much slower and resource heavier file I/O. Only once the table has reached its end, it is exported to a file.

MODULE table
IMPLICIT NONE

CONTAINS
  ! Initializes the provided abstract table by defining columns
  SUBROUTINE table_init(table_str)
    CHARACTER(LEN=*), INTENT(OUT) :: table_str
    WRITE(table_str,'(A)') "#          Time     Velocity X	   Velocity Y      X              Y"
  END SUBROUTINE table_init

  ! Adds new data row to abstract table
  SUBROUTINE table_add_row(table_str, time, col1, col2, col3, col4)
    CHARACTER(LEN=*), INTENT(OUT) :: table_str
    REAL            , INTENT(IN)  :: time, col1, col2, col3, col4

    WRITE(table_str, '(A, A, F15.2, F15.7, F15.7, F15.7, F15.7)') TRIM(table_str), NEW_LINE('A'), time, col1, col2, col3, col4
  END SUBROUTINE table_add_row

  ! Export abstract table to file
  SUBROUTINE table_export(table_str, fname)
    CHARACTER(LEN=*), INTENT(OUT) :: fname
    CHARACTER(LEN=*), INTENT(OUT) :: table_str

    ! Open file
    OPEN(10, file=fname)
    WRITE(10, '(A)') TRIM(table_str)
    CLOSE(10)
  END SUBROUTINE table_export

END MODULE table
