Module Normalisation

    Use Allocation
    Implicit None

    Real, Protected :: Ub, Zb, Yb   !Using PROTECTED to make sure these variables can be used, but not changed outside this module
                                    !With PRIVATE atribute, variables can not be used outside this module
    Contains
!=========================================================================
        Subroutine Base_Values
            Implicit None

            Ub = V1
            Zb = Ub**2/Sb
            Yb = 1/Zb

            print *
        End Subroutine Base_Values
!--------------------------------------------------------------------
        Subroutine Relative_Impedance
            Implicit None

            Do i = 1, size(Relative_Z)     
                Relative_Z(i) = Line_Zd(i) / Zb
            End Do

        End Subroutine Relative_Impedance
!--------------------------------------------------------------------
        Subroutine Relative_Admittance
            Implicit None

            Do i = 1, size(Relative_Y)           
                Relative_Y(i) = Line_Y0(i) / Yb
            End Do

        End Subroutine Relative_Admittance
!--------------------------------------------------------------------
        Subroutine Relative_Apparent_Power
            Implicit None

            Do i = 1, Number_of_Consumer_Nodes
                Relative_s(i) = S(i) / Sb
            End Do

        End Subroutine Relative_Apparent_Power
!--------------------------------------------------------------------
        Subroutine Relative_Voltage
            Implicit None

            Do i = 1, Number_of_Nodes - 1
                Relative_v(i) = 1     !V1 / Ub     !For first iterration, all node voltages are equal to root voltage V1
            End Do

        End Subroutine Relative_Voltage
!--------------------------------------------------------------------
End Module Normalisation