# Power Flow Claculation with Current Summation Algorithm in Radial Distribution Networks (Fortran)
# :hammer: README is currently in the making :hammer:

## Table of Contents
 * [About the program](#about-the-program)
 * [Program setup](#program-setup)
 * [Files](#files)
 * [Heads up :warning:](#heads-up-:warning:) :warning:

## About the program
Before understanding the program, the theoretical background of Power systems and its analysis is required.  
There are some good sources which explain power flow analasys in-depth. To view them, click [here](https://electrisim.com/load-flow-power-flow.html) or [here](https://www.intechopen.com/books/computational-models-in-engineering/power-flow-analysis).
 
## Program setup
 
## Files
   There is a certain **"hierarchy"** in module organisation. The first module - **Module_Global_Variables**, is included in the 2nd module - **Module_Input_data**, which is then included in the next module - **Module_Allocation**, and so on, ending with **Main**.
  
 - ### Module_Global_Variables.f90
    Contains all variables needed for the program. They are visible in the whole program, because this module is included in every other module.
  
 - ### Module_Input_Data.f90
    Contains subroutines for reading data from **Setup.txt**, **Grid_Layout.txt** and **Power_Line_Parameters.txt**.
    
 - ### Module_Allocation.f90
    Contains subroutines for memory **allocation** and **deallocation**, required for dynamic arrays. Calling **Array_Allocation** subroutine is mandatory before using an array. Subroutine **Array_Deallocation** is called at the very end of the program, to free up memory taken by those dynamic arrays.
    
 - ### Module_Normalisation.f90
    Contains subroutines for transforming absolute units into relative units and for setting the voltage value to base value, needed for first iteration.
    
 - ### Module_Calculation.f90
    This is where the magic happens. Calculates line impedance, line admittance, summarieses shunt admittance for each node.\
    Contains **main subroutines for the Power Flow Claculation with Current Summation Algorithm** (injected current, branch current and node voltage calculation, as well as "injected" power of each node). Besides that, it finds power debalance of each node and maximum power debalance. These suborutines are called in **Iteration** subroutine until the iteration condition is met.
    
 - ### Module_Output_Data.f90
    This module prints out relevant data (voltage and apparent power of each node) in **Output.txt** file.

 - ### Main.f90
    Main file of the program which calls all the necessary subroutines from other modules.

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
    This is the solution output file. It shows number of needed iterations, node index, its voltage and apparent power represented in a table.
    
 - ### Predmetni_projekat.pdf
    This is additional documentation written in **Serbian** language explaining the theoretical background of the Power systems and this calculation.

## Heads up :warning:
 Be careful when entering input data, since altering the number format (even entering 1 more or less digit than defined) will result in compile error. With that in mind, if we want to enter a different number format, we have to define that in program code (**Module_Input_Data.f90**).\
Be careful about output formating aswell. When output format is incorrect, there will be no compile error, however there is a chance that the results will not be displayed correctly.
 
 
