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
! The underlying module provides all necessary mathematical and physical functions necessary for the trajectory calculation
! of a moving projectile, in form of functional fortran code.

MODULE calc
IMPLICIT NONE

CONTAINS
  ! Convert radiant to deg
  REAL FUNCTION calc_deg2rad(deg)
    REAL, INTENT(IN) :: deg
    calc_deg2rad = 3.1415926 * deg / 180
  END FUNCTION calc_deg2rad

  ! Obtain time on which y = 0
  REAL FUNCTION calc_land(g, deg, vel)
    REAL, INTENT(IN) :: g, deg, vel
    calc_land = vel * sin(deg) / 0.5 / g
  END FUNCTION

  ! Get vertical velocity
  REAL FUNCTION calc_vx(deg, vel)
    REAL, INTENT(IN) :: deg, vel
    calc_vx = vel * cos(deg)
  END FUNCTION calc_vx

  ! Get horizontal velocity
  REAL FUNCTION calc_vy(g, deg, vel, time)
    REAL, INTENT(IN) :: g, deg, vel, time
    calc_vy = vel * sin(deg) - (g * time)
  END FUNCTION calc_vy

  ! Get horizontal co-ordinates
  REAL FUNCTION calc_x(deg, vel, time)
    REAL, INTENT(IN) :: deg, vel, time
    calc_x = vel * cos(deg) * time
  END FUNCTION calc_x

  ! Get horizontal co-ordinates
  REAL FUNCTION calc_y(g, deg, vel, time)
    REAL, INTENT(IN) :: g, deg, time, vel
    calc_y = vel * sin(deg) * time - 0.5 * g * time**2
  END FUNCTION calc_y

END MODULE calc
