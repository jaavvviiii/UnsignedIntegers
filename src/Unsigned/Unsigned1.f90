module unsigned1_module
    
    implicit none

    integer*1, parameter :: bytessigned1 = 1 !Number of bytes
    integer*2, parameter :: valuessigned1 = 2**(bytessigned1*8) !Total of numbers to represent with its data
    integer, parameter :: maxsigned1 = 2**(bytessigned1*8-1) - 1 !Maximum signed value
    integer, parameter :: minsigned1 = -maxsigned1 - 1 !Minimum signed value
    integer*2, parameter :: maxUnsigned1 = valuessigned1 - 1 !Maximum unsigned value

    !Array of Unsigned Integer (kind=4) type
    type unsigned1_array_type
        integer*8 :: size = 0
        integer*1, pointer, dimension (:) :: int
    end type unsigned1_array_type


    interface assignment(=)
        module procedure copy_unsigned1_array, set_array1_values
    end interface 

    contains

    !Check if the given number can be represented with Unsigned Integer(kind=1)
    function unsigned1_admitted(testValue)
        implicit none
        integer*8, intent(in) :: testValue
        logical :: unsigned1_admitted
        unsigned1_admitted = testValue.ge.0.and.testValue.lt.valuessigned1
    end function unsigned1_admitted

    !Receive an unsigned number and transform it to a Signed Integer (kind=1)
    function Unsigned1Tosigned1(number)
        implicit none
        integer*2, intent(in) :: number
        integer*1 :: Unsigned1Tosigned1
        Unsigned1Tosigned1 = number - maxsigned1 - 1
    end function Unsigned1Tosigned1

    !Receive a Signed Integer(kind=1) and transform it to a Unsigned Integer (kind=1)
    function signed1ToUnsigned1(number)
        implicit none
        integer*1, intent(in) :: number
        integer*2 :: signed1ToUnsigned1
        signed1ToUnsigned1 = number + maxsigned1 + 1
    end function signed1ToUnsigned1

    !Free the memory allocated by an array of Unsigned Integer (kind=1)
    subroutine deallocate_unsigned1_array(array)
        implicit none
        type (unsigned1_array_type), intent (inout) :: array
        integer :: err
        deallocate(array%int, stat=err)
        array%size = 0
    end subroutine deallocate_unsigned1_array

    !Allocate an array of Unsigned Integer (kind=1) for 'size' numbers
    subroutine allocate_unsigned1_array(array, size)
        implicit none
        type (unsigned1_array_type), intent (inout) :: array
        integer*8, intent(in) :: size
        integer :: err
        call deallocate_unsigned1_array(array)
        if (size.gt.0) then
            allocate(array%int(size), stat = err)
            if (err.ne.0) then
                write(*,*) 'ERROR in unsigned1 allocation'
            end if
            array%size = size
        end if
    end subroutine allocate_unsigned1_array

    !Returns an array of 'size' Unsigned Integer (kind=1) numbers
    function new_unsigned1_array(size)
        implicit none
        integer*8, intent(in) :: size
        type (unsigned1_array_type) :: new_unsigned1_array
        call allocate_unsigned1_array(new_unsigned1_array, size)
    end function new_unsigned1_array

    !Copy and array of Unsigned Integer (kind=1) to another one
    subroutine copy_unsigned1_array(array_out, array_in)
        implicit none
        type (unsigned1_array_type), intent(out) :: array_out
        type (unsigned1_array_type), intent(in) :: array_in
        integer :: i
        if (array_out%size.ne.array_in%size) then
            call allocate_unsigned1_array(array_out, array_in%size)
        end if
        forall (i = 1 : array_in%size)
            array_out%int(i) = array_in%int(i)
        end forall
    end subroutine copy_unsigned1_array

    !Initialize all the positions of the array to the same value
    subroutine set_array1_values(array, number)
        implicit none
        type (unsigned1_array_type), intent(inout) :: array
        integer*1, intent(in) :: number
        integer :: i
        integer :: unsigned
        forall ( i=1: array%size)
            array%int(i) = number
        end forall
    end subroutine set_array1_values

    !Set an Unsigned Integer (kind=1) value in the given 
    !position of the array
    subroutine set_Unsigned1_value(array, position, number)
        implicit none
        type (unsigned1_array_type), intent(inout) :: array
        integer*8, intent(in) :: position
        integer*2, intent(in) :: number
        array%int(position) = Unsigned1Tosigned1(number)
    end subroutine set_Unsigned1_value

    !Get the Unsigned Integer (kind=1) value at the given 
    !position of the array
    function get_Unsigned1_value(array, position)
        implicit none
        type (unsigned1_array_type), intent(in) :: array
        integer*8, intent(in) :: position
        integer :: get_Unsigned1_value
        get_Unsigned1_value = signed1ToUnsigned1(array%int(position))
    end function get_Unsigned1_value

    !Returns the number of bytes required to store the Unsigned Integer (kind=1) array
    function get_bytes_unsigned1_array(array)
        implicit none
        type (unsigned1_array_type), intent (in) :: array
        integer*8 :: get_bytes_unsigned1_array
        type (unsigned1_array_type) :: tmp
        get_bytes_unsigned1_array = sizeof(tmp) + (bytessigned1 * array%size)
    end function get_bytes_unsigned1_array

    !Verify that all the numbers that can be represented with the
    !Unsigned Integer (kind=1) type are correct
    subroutine test_signed1()
        use print_module, only: get_memory_units
        implicit none
        integer*2 :: i, j
        character*32 :: string
        type (unSigned1_array_type) :: array
        write(string,*) maxUnsigned1
        write(*,*) 'Unsigned1: From 0 to ' // trim(adjustl(string))
        do i = 0, maxUnsigned1
            j = signed1ToUnsigned1(Unsigned1Tosigned1(i))
            if (i-j.ne.0) then
                write(*,*) 'ERROR', i, j
            end if
        end do
        array%size = maxUnSigned1
        write(*,*) 'Size of ' // trim(adjustl(string)) // ' Unsigned1 array : ' // get_memory_units(get_bytes_unsigned1_array(array))
    end subroutine test_signed1

end module unsigned1_module