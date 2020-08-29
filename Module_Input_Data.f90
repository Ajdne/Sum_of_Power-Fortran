Module Input_Data

    Use Global_Variables
    Implicit None       !This helps the compiler to find mistakes in declaration of variables

    Contains
!=============================================================================
        Subroutine Load_Setup
            Implicit none
                                                            !With STATUS, we determine whether we will create a new file
            Open (1, File = "Setup.txt", Status = 'Old')    ! or open an existing one. If unmentioned, STATUS is UNKNOWN by default
                                                            ! which means it will open that file or create it if it does not exist 
                Read (1, '(1x, i1)') Units_Selector         !For strings and text, using (" ") or (' ') is both fine, as long they go in pairs

                If ((Units_Selector /= 1) .AND. (Units_Selector /= 2)) Then
                    Write(*, *) "Wrong number! Please select number 1 or 2."
                    Write(*, *) "The program will now exit."
                    Stop                            
                Endif
                
                Read(1, '(1x, i2)') Max_Iter
                Read(1, '(1x, e7.0)') Epsilon

            Close (1)       !Closing Setup.txt

        End Subroutine Load_Setup
!----------------------------------------------------------------------------------
        Subroutine Load_Nodes
            Implicit None

            Open(2, file = "Grid_Layout.txt")

                Read(2, '(1x, i2)') Number_of_Nodes
                Read(2, '(1x, i1)') Number_of_Consumer_Nodes

        End Subroutine Load_Nodes
!----------------------------------------------------------------------------------
        Subroutine Load_Branch_Layout
            Implicit None

                Do i = 1, 4     ! Grid_Layout.txt is still open
                    Read(2, *)
                End Do
                Do i = 1, Number_of_Nodes - 1
                    Read(2, '(3x, i1, 9x, i4, 8x, i1)') B_S(i), Line_Length(i), B_E(i)
                End Do 
                           
            Close(2)    !Closing Grid_Layout.txt

        End Subroutine Load_Branch_Layout
!----------------------------------------------------------------------------------
        Subroutine Load_Grid_Parameters

            Implicit None

            Open (3, file="Power_Line_Parameters.txt")
                
                Do i = 1, 3 
                    Read(3, *)                  ! Using this to skip table characters
                End Do                                
                Read(3, '(5x, f5.3, 7x, f7.4)') Zd      ! f is for real numbers
                                                        ! 5 and 7 are for total number of characters
                Read(3, *)                              ! 3 and 4 are for number of decimals

                Read(3, '(15x, e11.2)') Y0           !The shunt admittance component of the power line (Y0  =  Y1d = Y2d = Y1i = Y2i = Y1o = Y2o)
                                                     !Number 11 in e11.2 represents the total number of characters in that data type, including the mandatory +/- in front of the number,
                                                     !decimal separator, exponent number and letter E 
                Do i = 1, 5
                    Read(3, *)               !Empty space in txt file
                End Do

                Do i = 1, Number_of_Consumer_Nodes          !Not possible before allocated
                    Read(3, '(4x, i2, 4x, e14.4, 3x, e14.5)') Consumer_Node(i), S(i)
                    Read(3, *)
                End Do
                          
                Read(3, *)              !Empty space in txt file
                Read(3, '(1x, e10.2)') V1

                Read(3, '(1x, e10.0)') Sb

            Close(3)

        End Subroutine Load_Grid_Parameters
! ---------------------------------------------------------------------------------
!        Subroutine Balance_and_Symmetry     !This could be a potential expansion
!                                            ! for the program
!            Implicit None . . .
!
!        End Subroutine Balance_and_Symmetry
! ---------------------------------------------------------------------------------
End Module Input_Data