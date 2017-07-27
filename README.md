# RocketSim
Rocket Sim is my attempt at the final project of the Oxford Royale Computer Science Course 2017. The primary goal was to provide robust, readable and efficient software to calculate a projectiles, or more specific, a rocket's motion trajectory in a vacuum with a given velocity, angle and gravity.

### Description
Upon launch, RocketSim will run a series of calculations based upon either the default, or optionally provided parameters. Once the projectiles trajectory has been calculated, the retrieved data is provided in form of an ASCII table text file, which may be imported into GNUPLOT for graphical visualization.

### Build

To build rocket sim, ensure the following packages have been installed

- GNUMAKE
- GCC

 Once certain that all dependecies are satisfied, proceed by starting the build
 ```
 make RocketSim
 ```

 Once complete, RocketSim may be launched
 ```
 ./RocketSim
 ```

### Usage

```
rocketsim [-v <velocity>] [-a <angle>] [-g <gravity>] [-o <output>]
```

|Argument|Parameter  |Description                                |Default Value|
|--------|-----------|-------------------------------------------|-------------|
|-v      |velocity   |Defines the initial velocity in m/s        |88           |
|-a      |angle	     |Defines the initial angle			             |45           |
|-g      |gravity	   |Defines applied gravity				             |9.81         |
|-o      |output	   |Defines file to which table is writtent to |Table.txt    |

Once executed, the user will be left off with a plain text file holding the data table.
