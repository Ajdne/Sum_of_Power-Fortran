Module Calculation

    Use Normalisation
    Implicit None

    Real, Parameter :: Ft_to_Mi = 0.000189393939   !Converting feet to mile (constant value)
                                    !When using PARAMETER atribute, giving a value to it is mandatory
    Contains
!========================================================
    Subroutine Calculate_Line_Impedance

        Implicit None
        
        If (Units_Selector == 2) Then
            Do i = 1, Number_of_Nodes - 1   
                Line_Zd(i) = Zd * Line_Length(i) * Ft_to_Mi     !Converting feett to miles (1ft = 1/5280 miles)
            End Do
        Else
            Do i = 1, Number_of_Nodes - 1     
                Line_Zd(i) = (Zd * Line_Length(i)) / 1000       !Converting from meters to km
            End Do 
        End If

    End Subroutine Calculate_Line_Impedance
! --------------------------------------------------------
    Subroutine Calculate_Line_Admittance

        Implicit None
        
        If (Units_Selector == 2) Then
            Do i = 1, Number_of_Nodes - 1                                      
                Line_Y0(i) = 0.5 * Cmplx(0, Y0) * Line_Length(i) * Ft_to_Mi     !Converting foot to mile
            End Do                          !Y0 is a real number, so converting it to complex is needed
        Else                                ! Also, it only has imaginary component - thats why we place 0 as its   
            Do i = 1, Number_of_Nodes - 1   ! real component, and Y0 as imaginary with CMPLX command
                Line_Y0(i) = (0.5 * Cmplx(0, Y0) * Line_Length(i)) / 1000   !Dividing with 1000 since the parameter unit is Ohms per kilometer
            End Do 
        End If                              ! Y0 could have been a complex number from the start

    End Subroutine Calculate_Line_Admittance
! --------------------------------------------------------
    Subroutine Summarise_Shunt_Admittance
        Implicit None

        Do i = 1, Number_of_Nodes - 1
               
            Sum_y(i) = Relative_y(i)

            Do j = 1, Number_of_Nodes - 1
                If((Line_Index(i) == B_E(i)) .AND. (B_E(i) == B_S(j))) Then
                    Sum_y(i) = Sum_y(i) + Relative_y(j) 
                End If 
            End Do     
        End Do                          

    End Subroutine Summarise_Shunt_Admittance
! --------------------------------------------------------
    Subroutine Calculate_Injected_Current
        Implicit None       !All done with relative values

        Do i = 1, Number_of_Nodes - 1    
                                     
            Injected_i(i) = Sum_Y(i) * Relative_v(i)        !When h == 1 ==> Relative_v(i) = 1
                                                    
            Do j = 1, Number_of_Consumer_Nodes

                If (i == Consumer_Node(j)) Then         
                    Injected_i(i) = Injected_i(i) &     ! "&" is used to extend the command to the next line
                    + Conjg( (Relative_S(j)) / Relative_v(i) )  !Complex conjugate of S in relative units
                End If                              !Relative_v is 1 as root voltage
            End Do                                  
        End Do

    End Subroutine Calculate_Injected_Current
! --------------------------------------------------------
    Subroutine Calculate_Branch_Current
        Implicit None

        Do i = Number_of_Nodes - 1, 1, -1

            Branch_j(i) = Injected_i(i)         
                                                
            If (i == Number_of_Nodes - 1) Then  !If its the last node, its Branch current is only equal   
                Go To 100                       ! to its Injected current, so we skip the next loop
            End If                              !For some reason, this was necessary

            Do j = 1, Number_of_Nodes - 1
                If(B_E(i) == B_S(j)) Then
                    Branch_j(i) = Branch_j(i) + Branch_j(j)
                End If                                         
            End Do

            100 Continue     
        End Do

    End Subroutine Calculate_Branch_Current
! --------------------------------------------------------
    Subroutine Calculate_Node_Voltage
        Implicit None

        Do i = 1, Number_of_Nodes - 1
           
            If((h == 1) .AND. (B_S(i) == 0)) Then
                
                Relative_v(i) = 1 - Branch_j(i) * Relative_z(i)
                Cycle       !With CYCLE, we skip the commands coming after it if the IF statement is true 
                            ! It exits the current iteration and starts a new one (i + 1)
            Else            ! This helps speed up the program
                Do j = 1, Number_of_Nodes - 1

                    If(B_S(i) == B_E(j)) Then

                        Relative_v(i) = Relative_v(j) - Branch_j(i) * Relative_z(i)
                    End If
                End Do
            End If
        End Do

    End Subroutine Calculate_Node_Voltage
! --------------------------------------------------------
    Subroutine Injected_Node_Power
        Implicit None

        Do i = 1, Number_of_Nodes - 1

            Injected_s(i) = Relative_v(i) * Conjg(Injected_i(i)) - Conjg(Sum_y(i)) * (Abs(Relative_v(i)))**2

        End Do

    End Subroutine Injected_Node_Power
! --------------------------------------------------------
    Subroutine Power_Debalance
        Implicit None

        Do i = 1, Number_of_Nodes - 1
            Finding_Consumers:    Do j = 1, Number_of_Consumer_Nodes    !All constructions can be named

                If (i == Consumer_Node(j)) Then                         !The REAL command is used to convert a number of some other kind
                    Delta_p(i) = Real(Injected_s(i) - Relative_s(j))    ! to a real number of a given [kind], but when used on a complex
                    Delta_q(i) = Imag(Injected_s(i) - Relative_s(j))    ! number, it extracts only the real part of complex number
                    Go to 220                        !This is needed to get out of the
                Else                                 ! Finding_Consumers loop.
                    Delta_p(i) = Real(Injected_s(i)) ! - (0 +j0) since there is no Relativie_S
                    Delta_q(i) = Imag(Injected_s(i)) ! because it is not a consumer node
                End If                               
            End Do Finding_Consumers

            220 Continue                              
        End Do

    End Subroutine Power_Debalance
! --------------------------------------------------------
    Subroutine Maximum_Debalance
        Implicit None

        Max_Active_Debalance = MAXVAL(Abs(Delta_p))     !MAXVAL(x) returns the maximum  value of array x 
        Max_Reactive_Debalance = MAXVAL(Abs(Delta_q))

    End Subroutine Maximum_Debalance
! --------------------------------------------------------      
    Subroutine Iteration
        Implicit None
                            
        h = 1
        Max_Active_Debalance = 1
        Max_Reactive_Debalance = 1

        Iterations: Do !While ( ((Max_Active_Debalance >= Epsilon) .OR. (Max_Reactive_Debalance >= Epsilon)) &
!                    .AND. (h <= Max_Iter) )

            Call Calculate_Injected_Current

            Call Calculate_Branch_Current

            Call Calculate_Node_Voltage

            Call Injected_Node_Power

            Call Power_Debalance

            Call Maximum_Debalance

            If ( ( (Max_Active_Debalance <= Epsilon) .AND. (Max_Reactive_Debalance <= Epsilon) ) &
                        .OR. (h == Max_Iter) ) Exit     !If these conditions are met, it exits
                h = h + 1                               ! the Iterations DO loop

        End Do Iterations           !This could also be done with the DO WHILE loop above and
                                    ! this simple code below
!        If (h == 1) Then
!              h = 1           !In case the number of iterations is only 1 (which is highly unlikely)
!        Else
!            h = h - 1         ! If we dont do this, the number of iterations (h) in the output
!        End If                ! file will be 1 higher than the actual number of needed iterations
    
    End Subroutine Iteration
! --------------------------------------------------------
    Subroutine Return_to_Absolute_Units
        Implicit None

        Do i = 1, Number_of_Nodes - 1
            Absolute_V(i) = Relative_v(i) * Ub
            Absolute_S(i) = Relative_v(i) * Conjg(Injected_i(i)) * Sb
        End Do

    End Subroutine Return_to_Absolute_Units
! --------------------------------------------------------
!    Subroutine Phase_Angle 
!        Implicit None
!        
!        Do i = 1, Number_of_Nodes - 1
!            Theta(i) = ATAN(IMAG(Absolute_V(i)) / REAL(Absolute_V(i)))
!            Write(*, *) "Phase angle (theta):", Theta(i)
!        End Do
!
!    End Subroutine Phase_Angle
! --------------------------------------------------------
End Module Calculation
