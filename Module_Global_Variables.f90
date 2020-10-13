Module Global_Variables
    Implicit None
!================== Data from input files ===================================  
    Integer              :: Units_Selector         !Meters or feet
    Integer              :: Number_of_Nodes, Number_of_Consumer_Nodes
    Real                 :: Epsilon                  !Iteration criteria
    Integer              :: Max_Iter                 !Max number of iterations

    Integer, Allocatable :: Line_Length(:)  !ALLOCATABLE goes with dynamic arrays
                                            !For static arrays, atribute DIMENSION is used
    Integer, Allocatable :: B_S(:), B_E(:)  !Branch start and end node index
    
    Complex              :: Zd  !, Zi, Zo  !Direct, inverse and zero impedance (Ohms per mile)
    Real                 :: Y0             !Admittance per legth (Siemens per mile)

    Integer, Allocatable :: Consumer_Node(:)  !Consumer node index                      ! (:) - represents the dimension of an array
    Complex, Allocatable :: S(:)              !Apparent power in nodes                  ! In this case, the array rang is 1 (vector)
    Real                 :: V1, Sb            !Line voltage, base apperent power        ! (:, :) would be a two-dimensional array (matrix)
                                                                                        ! Maximum array dimension possible in Fortran is 7
!================== Data calculated in program ===================================      ! In this program, only one-dimensional arrays are used, since
    Complex, Allocatable :: Line_Zd(:)  ! Line_Zi(:), Line_Zo(:) - Potential expansion  ! the code is easier to follow and program execution is faster
    Complex, Allocatable :: Line_Y0(:)

    Integer, Allocatable :: Line_Index(:)       !The program could actually work without this
    Integer              :: i, j, h                        !Counters and iterators
    Integer              :: Alloc_Check, Dealloc_Check     !For allocation and deallocation testing
  
    !_____________Normalisation________________
    Complex, Allocatable :: Relative_z(:)   !Relative values - marked with small letters. Fortran does no make a difference
    Complex, Allocatable :: Relative_y(:)   ! between uppercase and lowercase, but for the sake of consistency I have marked all relative values with lowercase
    Complex, Allocatable :: Relative_s(:)   !Apparent power of a consumer node (in relative values)

    Complex, Allocatable :: Sum_y(:)        !Summarised shunt admittance in nodes

    !______________Iterations__________________
    Complex, Allocatable :: Injected_i(:)   !Injected current in relative units
    Complex, Allocatable :: Branch_j(:)     !Current in branches (also relative units)
    Complex, Allocatable :: Relative_v(:)   !Node voltage in relative units
    Complex, Allocatable :: Injected_s(:)   !Injected node power (in relative values)
    
    !____________Iteration conditions_______________________
    Real, Allocatable    :: Delta_p(:), Delta_q(:) !
    Real                 :: Max_Active_Debalance, Max_Reactive_Debalance   !Start value needs to be some integer (like 1) for iterator to work                                                                          

    !____________Returning from relative values_____________
    Complex, Allocatable :: Absolute_V(:)  !Absolute node voltage value
    Complex, Allocatable :: Absolute_S(:)  !Absolute node injected complex power
    
    !Real, Allocatable    :: Theta(:)       !Phase angle of node voltage    -   Potential expansion
!_______________________________________________________________________________
End Module Global_Variables
