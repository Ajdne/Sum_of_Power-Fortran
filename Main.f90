program Main

    Use Calculation
    Use Output_Data
    Implicit none

!____________Loading____________
    Call Load_Setup
    Call Load_Nodes
        Write(*, *) "Setup loaded!"
    Call Array_Allocation
    Call Load_Branch_Layout   
    Call Load_Grid_Parameters

    Write(*, *) "Proceed?"
    Read(*, *)

!___________Line Parameters_____________
    Call Calculate_Line_Impedance
    Write(*, *)
    Call Calculate_Line_Admittance
    Print *, "Line parameters calculated!"
    Print *

!____________Normalisation_______________
    Call Base_Values
    Call Relative_Impedance
    Call Relative_Admittance
    Call Relative_Apparent_Power
    Call Relative_Voltage
    Write(*, *) "Normalisation complete!"
    Print *

!_____________Calculation________________
    Call Summarise_Shunt_Admittance
    print *
    print *, "Proceed?"
    Read(*, *)

    Call Iteration
    Call Return_to_Absolute_Units
    Write(*, *) "Calculations complete!"
    Write(*, *)
    Write(*, *) "Maximum active power debalance:", Max_Active_Debalance
    Write(*, *) "Maximum reactive power debalance:", Max_Reactive_Debalance
    Print *
!______________Output__________________
    Call Output_Results
    Write(*, *) "Number of needed iterations, node voltage and power are presented in Output.txt file."
    Read(*, *)
    Call Array_Deallocation

end program Main

