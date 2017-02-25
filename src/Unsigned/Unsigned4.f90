module unSigned4_module
    
    implicit none

    integer*1, parameter :: bytesSigned4 = 4 !Number of bytes
    integer*8, parameter :: valuesSigned4 = 2**(bytesSigned4*8) !Total of numbers to represent with its data
    integer, parameter :: maxSigned4 = 2**(bytesSigned4*8-1) - 1 !Maximum signed value
    integer, parameter :: minSigned4 = -maxSigned4 - 1 !Minimum signed value
    integer*8, parameter :: maxUnSigned4 = valuesSigned4 - 1 !Maximum unsigned value

    !Array of Unsigned Integer (kind=4) type
    type unSigned4_array_type
        integer :: size = 0
        integer*4, pointer, dimension (:) :: int
    end type unSigned4_array_type


    interface assignment(=)
        module procedure copy_unSigned4_array, set_array2_values
    end interface 

    contains

    !Check if the given number can be represented with Unsigned Integer(kind=4)
    function unSigned4_admitted(testValue)
        implicit none
        integer*8, intent(in) :: testValue
        logical :: unSigned4_admitted
        unSigned4_admitted = testValue.ge.0.and.testValue.lt.valuesSigned4
    end function unSigned4_admitted

    !Receive an unsigned number and transform it to a Signed Integer (kind=4)
    function UnSigned4ToSigned4(number)
        implicit none
        integer*8, intent(in) :: number
        integer*4 :: UnSigned4ToSigned4
        UnSigned4ToSigned4 = number - maxSigned4 - 1
    end function UnSigned4ToSigned4

    !Receive a Signed Integer(kind=4) and transform it to a Unsigned Integer (kind=4)
    function Signed4ToUnSigned4(number)
        implicit none
        integer*4, intent(in) :: number
        integer*8 :: Signed4ToUnSigned4
        Signed4ToUnSigned4 = int8(number) + maxSigned4 + 1
    end function Signed4ToUnSigned4

    !Free the memory allocated by an array of Unsigned Integer (kind=4)
    subroutine deallocate_unSigned4_array(array)
        implicit none
        type (unSigned4_array_type), intent (inout) :: array
        integer :: err
        deallocate(array%int, stat=err)
        array%size = 0
    end subroutine deallocate_unSigned4_array

    !Allocate an array of Unsigned Integer (kind=4) for 'size' numbers
    subroutine allocate_unSigned4_array(array, size)
        implicit none
        type (unSigned4_array_type), intent (inout) :: array
        integer, intent(in) :: size
        integer :: err
        call deallocate_unSigned4_array(array)
        if (size.gt.0) then
            allocate(array%int(size), stat = err)
            if (err.eq.0) then
                write(*,*) 'ERROR in unSigned4 allocation'
            end if
            array%size = size
        end if
    end subroutine allocate_unSigned4_array

    !Returns an array of 'size' Unsigned Integer (kind=4) numbers
    function new_unSigned4_array(size)
        implicit none
        integer, intent(in) :: size
        type (unSigned4_array_type) :: new_unSigned4_array
        call allocate_unSigned4_array(new_unSigned4_array, size)
    end function new_unSigned4_array

    !Copy and array of Unsigned Integer (kind=4) to another one
    subroutine copy_unSigned4_array(array_out, array_in)
        implicit none
        type (unSigned4_array_type), intent(out) :: array_out
        type (unSigned4_array_type), intent(in) :: array_in
        integer :: i
        if (array_out%size.ne.array_in%size) then
            call allocate_unSigned4_array(array_out, array_in%size)
        end if
        forall (i = 1 : array_in%size)
            array_out%int(i) = array_in%int(i)
        end forall
    end subroutine copy_unSigned4_array

    !Initialize all the positions of the array to the same value
    subroutine set_array2_values(array, number)
        implicit none
        type (unSigned4_array_type), intent(inout) :: array
        integer, intent(in) :: number
        integer :: i
        integer :: unsigned
        forall ( i=1: array%size)
            array%int(i) = number
        end forall
    end subroutine set_array2_values

    !Set an Unsigned Integer (kind=4) value in the given 
    !position of the array
    subroutine set_UnSigned4_value(array, position, number)
        implicit none
        type (unSigned4_array_type), intent(inout) :: array
        integer, intent(in) :: position
        integer*8, intent(in) :: number
        array%int(position) = UnSigned4ToSigned4(number)
    end subroutine set_UnSigned4_value

    !Get the Unsigned Integer (kind=4) value at the given 
    !position of the array
    function get_UnSigned4_value(array, position)
        implicit none
        type (unSigned4_array_type), intent(in) :: array
        integer, intent(in) :: position
        integer :: get_UnSigned4_value
        get_UnSigned4_value = Signed4ToUnSigned4(array%int(position))
    end function get_UnSigned4_value

    !Verify that all the numbers that can be represented with the
    !Unsigned Integer (kind=4) type are correct
    subroutine test_Signed4()
        implicit none
        integer*8 :: i, j
        character*32 :: string
        write(string,*) maxUnSigned4
        write(*,*) 'UnSigned4: From 0 to ' // trim(adjustl(string))
        do i = 0, maxUnSigned4
            j = Signed4ToUnSigned4(UnSigned4ToSigned4(i))
            if (i-j.ne.0) then
                write(*,*) 'ERROR', i, j
                stop
            end if
        end do
    end subroutine test_Signed4



end module unSigned4_module