Module Output_Data

    Use Calculation
    Implicit None

    Contains
!=======================================================================
        Subroutine Output_Results

            Open (11, file="Output.txt")
                
                Write(11, *) "_________________________________________"
                Write(11, *) "|     Number of iterations:", h, "|"
                Write(11, *) "*****************************************"
                Write(11, *) "_____________________________________________________________________"
                Write(11, *) "|   Node  |     Absolute voltage [kV]    |   Injected power [MVA]   |"
                Write(11, *) "|-------------------------------------------------------------------|"

                Result_Table: Do i = 1, Number_of_Nodes - 1
                    Write(11, '( " |", 1x, i4, 4x, "|", 3x, f8.5, 1x, "+j ", e12.5, 3x, "|", 4x, f8.5, 1x, "+j", f8.5, 3x, "|")'), i, (Absolute_V(i) / 1000), (Absolute_S(i)  / 1000000)
                    Write(11, *) "|-------------------------------------------------------------------|"
                End Do Result_Table     ! Dividing voltage with 1000 and power with 1 000 000 to present them in [kv] and [MVA]

            Close (11)

        End Subroutine Output_Results
!======================================================================
End Module Output_Data