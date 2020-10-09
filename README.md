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
  Modules are listed in this order to represent their **"hierarchy"**. The first module - **Module_Global_Variables**, is included in the 2nd module - **Module_Input_data**, which is then included in the next module - **Module_Allocation**, and so on, ending with **Main**.
  
 - ### Module_Global_Variables.f90
   Contains all variables needed for the program. They are visible in the whole program, because this module is included in every other module.
  
 - ### Module_Input_Data.f90

 - ### Module_Allocation.f90

 - ### Module_Normalisation.f90

 - ### Module_Calculation.f90

 - ### Module_Output_Data.f90

 - ### Main.f90

 - ### Setup.txt
 Contains:
  - **Units_Selector** - Selection of units of length (meters or feet)
  - **Max_Iter** - Maximum number of iterations (needed in case the program is not able to reach the solution)
  - **Epsilon** - Iteration criteria value

 - ### Grid_Layout.txt
 Contains:
  - **Number_of_Nodes** - Total number of nodes in the given power grid
  - **Number_of_Consumer_Nodes** - Number of nodes with some power consumption (where apparent power S is not 0)

 - ### Power_Line_Parameters.txt
 Contains:
  - **Zd** - Line impedance value represented as direct(positive) symmetrical component
  - **Y0** - Power line admittance value
    Units of **Zd** and **Y0** are [ Ω / unit of length] and [ mS / unit of length], respectively.
  - **S[i]** - Real and imaginary part of apparent power of each consumer node, where "i" represents the consumer node index
  - **V1** - Line voltage value in root node
  - **Sb** - Base value of apparent power
    This value is needed for normalisation procedures (converting all units into relative values, so calculations are done more easly...).

 - ### Output.txt

 - ### Predmetni_projekat.pdf
 This is additional documentation written in **Serbian** language explaining the theoretical background of the Power systems and this calculation.

## Program setup

## Heads up
