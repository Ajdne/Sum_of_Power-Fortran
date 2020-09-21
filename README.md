# Power Flow Claculation with Current Summation Algorithm in Radial Distribution Networks (Fortran)
# :hammer: README is currently in the making :hammer:

## Table of Contents
 * [About the program](#about-the-program)
 * [Files](#files)
 * [Program setup](#program-setup)
 * [Heads up](#heads-up) :warning:

## About the program
Before understanding the program, the theoretical background of Power systems and its analysis is required.
There are some good sources which explain power flow analasys in-depth. To view them, click [here](https://electrisim.com/load-flow-power-flow.html) or [here](https://www.intechopen.com/books/computational-models-in-engineering/power-flow-analysis).
  
## Files

**Module_Global_Variables.f90**

**Module_Input_Data.f90**

**Module_Allocation.f90**

**Module_Normalisation.f90**

**Module_Calculation.f90**

**Module_Output_Data.f90**

**Main.f90**

**Setup.txt** contains
              :: Selection of units of length with Units_Selector (meters or feet)
              :: Maximum number of iterations named Max_Iter (needed in case when the program is not able to reach the solution)
              :: Iteration criteria value named Epsilon

**Grid_Layout.txt**

**Power_Line_Parameters.txt**

**Predmetni_projekat.pdf** is additional documentation written in Serbian language explaining the theoretical background of the Power systems and this calculation.

## Program setup

## Heads up
