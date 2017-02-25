module unsigned2_module
    
    implicit none

    integer*1, parameter :: bytesSigned2 = 2 !Number of bytes
    integer, parameter :: valuesSigned2 = 2**(bytesSigned2*8) !Total of numbers to represent with its data
    integer, parameter :: maxSigned2 = 2**(bytesSigned2*8-1) - 1 !Maximum signed value
    integer, parameter :: minSigned2 = -maxSigned2 - 1 !Minimum signed value
    integer, parameter :: maxUnsigned2 = valuesSigned2 - 1 !Maximum unsigned value

    !Array of Unsigned Integer (kind=2) type
    type unsigned2_array_type
        integer :: size = 0
        integer*2, pointer, dimension (:) :: int
    end type unsigned2_array_type


    interface assignment(=)
        module procedure copy_unsigned2_array, set_array2_values
    end interface 

    contains

    !Check if the given number can be represented with Unsigned Integer(kind=2)
    function unsigned2_admitted(testValue)
        implicit none
        integer*8, intent(in) :: testValue
        logical :: unsigned2_admitted
        unsigned2_admitted = testValue.ge.0.and.testValue.lt.valuesSigned2
    end function unsigned2_admitted

    !Receive an unsigned number and transform it to a Signed Integer (kind=2)
    function Unsigned2ToSigned2(number)
        implicit none
        integer, intent(in) :: number
        integer*2 :: Unsigned2ToSigned2
        Unsigned2ToSigned2 = number - maxSigned2 - 1
    end function Unsigned2ToSigned2

    !Receive a Signed Integer(kind=2) and transform it to a Unsigned Integer (kind=2)
    function Signed2ToUnsigned2(number)
        implicit none
        integer*2, intent(in) :: number
        integer :: Signed2ToUnsigned2
        Signed2ToUnsigned2 = number + maxSigned2 + 1
    end function Signed2ToUnsigned2

    !Free the memory allocated by an array of Unsigned Integer (kind=2)
    subroutine deallocate_unsigned2_array(array)
        implicit none
        type (unsigned2_array_type), intent (inout) :: array
        integer :: err
        deallocate(array%int, stat=err)
        array%size = 0
    end subroutine deallocate_unsigned2_array

    !Allocate an array of Unsigned Integer (kind=2) for 'size' numbers
    subroutine allocate_unsigned2_array(array, size)
        implicit none
        type (unsigned2_array_type), intent (inout) :: array
        integer, intent(in) :: size
        integer :: err
        call deallocate_unsigned2_array(array)
        if (size.gt.0) then
            allocate(array%int(size), stat = err)
            if (err.eq.0) then
                write(*,*) 'ERROR in unsigned2 allocation'
            end if
            array%size = size
        end if
    end subroutine allocate_unsigned2_array

    !Returns an array of 'size' Unsigned Integer (kind=2) numbers
    function new_unsigned2_array(size)
        implicit none
        integer, intent(in) :: size
        type (unsigned2_array_type) :: new_unsigned2_array
        call allocate_unsigned2_array(new_unsigned2_array, size)
    end function new_unsigned2_array

    !Copy and array of Unsigned Integer (kind=2) to another one
    subroutine copy_unsigned2_array(array_out, array_in)
        implicit none
        type (unsigned2_array_type), intent(out) :: array_out
        type (unsigned2_array_type), intent(in) :: array_in
        integer :: i
        if (array_out%size.ne.array_in%size) then
            call allocate_unsigned2_array(array_out, array_in%size)
        end if
        forall (i = 1 : array_in%size)
            array_out%int(i) = array_in%int(i)
        end forall
    end subroutine copy_unsigned2_array

    !Initialize all the positions of the array to the same value
    subroutine set_array2_values(array, number)
        implicit none
        type (unsigned2_array_type), intent(inout) :: array
        integer, intent(in) :: number
        integer :: i
        integer :: unsigned
        forall ( i=1: array%size)
            array%int(i) = number
        end forall
    end subroutine set_array2_values

    !Set an Unsigned Integer (kind=2) value in the given 
    !position of the array
    subroutine set_Unsigned2_value(array, position, number)
        implicit none
        type (unsigned2_array_type), intent(inout) :: array
        integer, intent(in) :: position
        integer, intent(in) :: number
        array%int(position) = Unsigned2ToSigned2(number)
    end subroutine set_Unsigned2_value

    !Get the Unsigned Integer (kind=2) value at the given 
    !position of the array
    function get_Unsigned2_value(array, position)
        implicit none
        type (unsigned2_array_type), intent(in) :: array
        integer, intent(in) :: position
        integer :: get_Unsigned2_value
        get_Unsigned2_value = Signed2ToUnsigned2(array%int(position))
    end function get_Unsigned2_value

    !Verify that all the numbers that can be represented with the
    !Unsigned Integer (kind=2) type are correct
    subroutine test_signed2()
        implicit none
        integer :: i, j
        character*32 :: string
        write(string,*) maxUnsigned2
        write(*,*) 'Unsigned2: From 0 to ' // trim(adjustl(string))
        do i = 0, maxUnsigned2
            j = signed2ToUnsigned2(Unsigned2ToSigned2(i))
            if (i-j.ne.0) then
                write(*,*) 'ERROR', i, j
            end if
        end do
    end subroutine test_signed2



end module unsigned2_module