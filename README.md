# NavierStokesSimulation
Plugin Navier-Stokes for fluids simulations on the GiD pre/post processor.

## Software Requirements

- R (tested on v3.5.1)
- Unix based system

## Installation

- `$ git clone https://github.com/LuisMario01/NavierStokesSimulation`
- `$ cd NavierStokesSimulation/R/`
- `$ pwd`
- Open and change the line 13 of alphaSolver.R to the absolute path of the R folder (you can copy and paste the output of the previous command)
- `$ ln -s /absolute/path/to/git/repo/NavierStokesSimulation/R /absolute/path/to/git/NavierStokesSimulation/alphaSolver.gid/exec/R`
- `$ ln -s /absolute/path/to/git/repo/NavierStokesSimulation/alphaSolver.gid /absolute/path/to/gid_installation/problemtypes/alphaSolver.gid`

## Common problems

- Try `chmod +x` on R/alphaSolver.R and on alphaSolver.gid/alphaSolver.unix.bat
- Try running GiD as root
