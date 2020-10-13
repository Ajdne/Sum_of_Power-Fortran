Module Allocation

    Use Input_Data

    Implicit None

Contains
!=============================================================================
    Subroutine Array_Allocation     !Since all arrays used in program are deferred-shape arrays
        Implicit None               ! the memory for them is allocated only when specified
                                    ! using the command ALLOCATE
        Write(*, *) " - - - - - - - - - - - - - - - - - - - - - - - - - "
        Allocate (Line_Index(Number_of_Nodes - 1), STAT = Alloc_Check)
        If (Alloc_Check .NE. 0) Then                !.NE. (or /=) is a relation operator, meaning NOT EQUAL
            Write(*, *) "Error with Line_Index allocation!" ! It is used for all variables except logical 
            Stop
        Else
            Do i = 1, Size(Line_Index)      !No value can be given to an unallocated array, since no memory has been reserved for it
                Line_Index(i) = i           ! That is why procedure Array_Allocation is called before
            End Do                          ! loading the system parameters in the main programe
        End If

        Allocate (Line_Length(size(Line_Index)), STAT = Alloc_Check) !If there are some problems, Alloc_Check will have a random value
        If (Alloc_Check .NE. 0) Then                                 ! Thats why if Alloc_Check is not equal to 0, which is its default value,
            Write(*, *) "Error with Line_Length allocation!"         ! after this message, the program will stop entirely
            Stop
        End If
    
        If (.NOT. Allocated (Line_Zd)) Then         !Command ALLOCATED is used to check if an array is already allocated
            Allocate (Line_Zd(Number_of_Nodes - 1), STAT = Alloc_Check)
            If (Alloc_Check .NE. 0) Then
                Write(*, *) "Error with Line_Zd allocation!"
                Stop
            End If
        End If

!        Allocate (Line_Zi(Number_of_Nodes - 1), STAT = Alloc_Check)
!        If (Alloc_Check .NE. 0) Then
!            Write(*, *) "Error with Line_Zi allocation!"
!            Stop
!        End If        ! This is currently unnecessary
!                      ! It can be a potential expansion to the program
!        Allocate (Line_Zo(Number_of_Nodes - 1), STAT = Alloc_Check)
!        If (Alloc_Check .NE. 0) Then
!            Write(*, *) "Error with Line_Zo allocation!"
!            Stop
!        End If

        Allocate (S(Number_of_Consumer_Nodes), STAT = Alloc_Check)
        If (Alloc_Check .NE. 0) Then
            Write(*, *) "Error with Line_Zo allocation!"
            Stop
        End If

        Allocate (Line_Y0(Number_of_Nodes - 1), STAT = Alloc_Check)
        If (Alloc_Check .NE. 0) Then
            Write(*, *) "Error with Line_Y0 allocation!"
            Stop
        End If

        Allocate (Relative_Z(Number_of_Nodes - 1), STAT = Alloc_Check)
        If (Alloc_Check .NE. 0) Then
            Write(*, *) "Error with Relative_Z allocation!"
            Stop
        End If
        
        Allocate (Relative_Y(Number_of_Nodes - 1), STAT = Alloc_Check)
        If (Alloc_Check .NE. 0) Then
            Write(*, *) "Error with Relative_Y allocation!"
            Stop
        End If

        Allocate (Relative_s(Size(S)), STAT = Alloc_Check)  !SIZE(x) returns a number equal to the size of x-array
        If (Alloc_Check .NE. 0) Then
            Write(*, *) "Error with Relative_s allocation!"
            Stop
        End If

        Allocate (B_S(Number_of_Nodes - 1), STAT = Alloc_Check)
        If (Alloc_Check .NE. 0) Then
            Write(*, *) "Error with B_S allocation!"
            Stop
        End If

        Allocate (B_E(Number_of_Nodes - 1), STAT = Alloc_Check)
        If (Alloc_Check .NE. 0) Then
            Write(*, *) "Error with B_E allocation!"
            Stop
        End If

        Allocate (Consumer_Node(Number_of_Consumer_Nodes), STAT = Alloc_Check)
        If (Alloc_Check .NE. 0) Then
            Write(*, *) "Error with Consumer_Node allocation!"
            Stop
        End If

        Allocate (Sum_Y(Number_of_Nodes), STAT = Alloc_Check)
        If (Alloc_Check .NE. 0) Then
            Write(*, *) "Error with Sum_Y allocation!"
            Stop
        End If

        Allocate (Injected_i(Number_of_Nodes - 1), STAT = Alloc_Check)
        If (Alloc_Check .NE. 0) Then
            Write(*, *) "Error with Injected_i allocation!"
            Stop
        End If

        Allocate (Branch_j(Number_of_Nodes - 1), STAT = Alloc_Check)
        If (Alloc_Check .NE. 0) Then
            Write(*, *) "Error with Branch_j allocation!"
            Stop
        End If

        Allocate (Relative_v(Number_of_Nodes - 1), STAT = Alloc_Check)
        If (Alloc_Check .NE. 0) Then      
            Write(*, *) "Error with Relative_v allocation!"
            Stop
        End If                

        Allocate (Injected_s(Number_of_Nodes - 1), STAT = Alloc_Check)
        If (Alloc_Check .NE. 0) Then     
            Write(*, *) "Error with Injected_s allocation!"
            Stop
        End If

        Allocate (Delta_p(Number_of_Nodes - 1), STAT = Alloc_Check)
        If (Alloc_Check .NE. 0) Then     
            Write(*, *) "Error with Delta_p allocation!"
            Stop
        End If

        Allocate (Delta_q(Number_of_Nodes - 1), STAT = Alloc_Check)
        If (Alloc_Check .NE. 0) Then     
            Write(*, *) "Error with Delta_q allocation!"
            Stop
        End If

        Allocate (Absolute_V(Number_of_Nodes - 1), STAT = Alloc_Check)
        If (Alloc_Check .NE. 0) Then
            Write(*, *) "Error with Absolute_V allocation!"
            Stop
        End If

        Allocate (Absolute_S(Number_of_Nodes - 1), STAT = Alloc_Check)
        If (Alloc_Check .NE. 0) Then
            Write(*, *) "Error with Absolute_S allocation!"
            Stop
        End If

!        Allocate (Theta(Number_of_Nodes - 1), STAT = Alloc_Check)
!        If (Alloc_Check .NE. 0) Then
!            Write(*, *) "Error with Theta allocation!"
!            Stop
!        End If

        Write(*, *) "Memory allocation completed."
        Write(*, *) " - - - - - - - - - - - - - - - - - - - - - - - - - "

    End Subroutine Array_Allocation
!------------------------------------------------------------------------------
    Subroutine Array_Deallocation       !When deallocating arrays, only mentioning the name of the array is enough.
                                        ! Specifying deallocation size is not needed, it is done automatically
        Implicit None

        Write(*, *) " - - - - - - - - - - - - - - - - - - - - - - - - - "
        Deallocate (Line_Index, STAT = Dealloc_Check)
        If (Dealloc_Check .NE. 0) Then
            Stop ("Error with memory deallocation for Line_Index!")
        End IF

        Deallocate (Line_Length, STAT = Dealloc_Check)
        If (Dealloc_Check .NE. 0) Then
            Stop ("Error with memory deallocation for Line_Length!")
        End IF

        Deallocate (Line_Zd, STAT = Dealloc_Check)
        If (Dealloc_Check .NE. 0) Then
            Stop ("Error with memory deallocation for Line_Zd!")
        End If

!        Deallocate (Line_Zi, STAT = Dealloc_Check)
!        If (Dealloc_Check .NE. 0) Then
!            Stop ("Error with memory deallocation for Line_Zi!")
!        End IF
!
!        Deallocate (Line_Zo, STAT = Dealloc_Check)
!        If (Dealloc_Check .NE. 0) Then
!            Stop ("Error with memory deallocation for Line_Zo!")
!        End IF

        Deallocate (S, STAT = Dealloc_Check)
        If (Dealloc_Check .NE. 0) Then
            Stop ("Error with memory deallocation for S!")
        End IF

        Deallocate (Line_Y0, STAT = Dealloc_Check)
        If (Dealloc_Check .NE. 0) Then
            Stop ("Error with memory deallocation for Line_Y0!")
        End If

        Deallocate (Relative_Z, STAT = Dealloc_Check)
        If (Dealloc_Check .NE. 0) Then
            Stop ("Error with memory deallocation for Relative_Z!")
        End If

        Deallocate (Relative_Y, STAT = Dealloc_Check)
        If (Dealloc_Check .NE. 0) Then
            Stop ("Error with memory deallocation for Relative_Y!")
        End If

        Deallocate (Relative_s, STAT = Dealloc_Check)
        If (Dealloc_Check .NE. 0) Then
            Stop ("Error with memory deallocation for Relative_s!")
        End If

        Deallocate (B_S, STAT = Dealloc_Check)
        If (Dealloc_Check .NE. 0) Then
            Stop ("Error with memory deallocation for B_S!")
        End If

        Deallocate (B_E, STAT = Dealloc_Check)
        If (Dealloc_Check .NE. 0) Then
            Stop ("Error with memory deallocation for B_E!")
        End If

        Deallocate (Consumer_Node, STAT = Dealloc_Check)
        If (Dealloc_Check .NE. 0) Then
            Stop ("Error with memory deallocation for Consumer_Node!")
        End If

        Deallocate (Sum_Y, STAT = Dealloc_Check)
        If (Dealloc_Check .NE. 0) Then
            Stop ("Error with memory deallocation for Sum_Y!")
        End If

        Deallocate (Injected_i, STAT = Dealloc_Check)
        If (Dealloc_Check .NE. 0) Then
            Stop ("Error with memory deallocation for Injected_i!")
        End If

        Deallocate (Branch_j, STAT = Dealloc_Check)
        If (Dealloc_Check .NE. 0) Then
            Stop ("Error with memory deallocation for Branch_j!")
        End If

        Deallocate (Relative_v, STAT = Dealloc_Check)
        If (Dealloc_Check .NE. 0) Then
            Stop ("Error with memory deallocation for Relative_v!")
        End If

        Deallocate (Injected_s, STAT = Dealloc_Check)
        If (Dealloc_Check .NE. 0) Then
            Stop ("Error with memory deallocation for Injected_s!")
        End If

        Deallocate (Delta_p, STAT = Dealloc_Check)
        If (Dealloc_Check .NE. 0) Then
            Stop ("Error with memory deallocation for Delta_p!")
        End If

        Deallocate (Delta_q, STAT = Dealloc_Check)
        If (Dealloc_Check .NE. 0) Then
            Stop ("Error with memory deallocation for Delta_q!")
        End If

        Deallocate (Absolute_V, STAT = Dealloc_Check)
        If (Dealloc_Check .NE. 0) Then
            Stop ("Error with memory deallocation for Absolute_V!")
        End If

        Deallocate (Absolute_S, STAT = Dealloc_Check)
        If (Dealloc_Check .NE. 0) Then
            Stop ("Error with memory deallocation for Absolute_S!")
        End If

!        Deallocate (Theta, STAT = Dealloc_Check)
!        If (Dealloc_Check .NE. 0) Then
!            Stop ("Error with memory deallocation for Theta!")
!        End If

        Write(*, *) "Memory deallocation completed."
        Write(*, *) " - - - - - - - - - - - - - - - - - - - - - - - - - "

    End Subroutine Array_Deallocation
!------------------------------------------------------------------------------
End Module Allocation
