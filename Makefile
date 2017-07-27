# Copyright 2017 Patrick Pedersen <ctx.xda@gmail.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Author:                   Patrick Pedersen
# Last modification Date:   7/26/2017 00:17
# Written and Tested on:    GNU/Linux 4.12.0-next-20170714-ARCH+

CC              := gfortran
CC_FALGS 	:=

TARGETS         := Calc.f90 Table.f90 RocketSim.f90
TARGETS_OUT 	:= Calc.o RocketSim.o Table.o

RocketSim: $(TARGETS_OUT)
	$(CC) $(CC_FALGS) $(TARGETS_OUT) -o RocketSim


$(TARGETS_OUT): $(TARGETS)
	$(CC) $(CC_FALGS) $(TARGETS) -c $(TARGETS)

clean:
	rm -f *.mod
	rm -f $(TARGETS_OUT)
