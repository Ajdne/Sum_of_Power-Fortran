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

**Setup.txt** contains:
 - Selection of units of length with **Units_Selector** (meters or feet)
 - Maximum number of iterations named **Max_Iter** (needed in case the program is not able to reach the solution)
 - Iteration criteria value named **Epsilon**

**Grid_Layout.txt** contains:
 - Total **Number_of_Nodes** in the given power grid
 - **Number_of_Consumer_Nodes** are nodes with some power consumption (where apparent power S is not 0)

**Power_Line_Parameters.txt** contains:
 - Line impedance value represented as direct(positive) symmetrical component named **Zd**
 - Power line admittance value named **Y0**\
 Units of **Zd** and **Y0** are [ Ω / unit of length] and [ mS / unit of length], respectively.
 - Real and imaginary part of apparent power of each consumer node represented as **S[i]**, where "i" represents the consumer node index
 - Line voltage value in root node named **V1**
 - Base value of apparent power named **Sb**\
 This value is needed for normalisation procedures (converting all units into relative values, so calculations are done more easly...).

**Predmetni_projekat.pdf** is additional documentation written in Serbian language explaining the theoretical background of the Power systems and this calculation.

----------------ADMIN NOTES------------------
STAVITI PRVO IME PODATKA - PA OBJASNJENJE
---------------------------------------------

## Program setup

## Heads up
