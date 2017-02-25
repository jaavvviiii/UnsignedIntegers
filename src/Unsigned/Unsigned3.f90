module unsigned3_module
    
    implicit none

    integer*1, parameter :: BytesSigned3 = 3 !Number of bytes
    integer, parameter :: valuessigned3 = 2**(bytessigned3*8)
    integer, parameter :: maxsigned3 = 2**(bytessigned3*8-1) - 1 !Maximum signed value
    integer, parameter :: minsigned3 = -maxsigned3 - 1 !Minimum signed value
    integer, parameter :: maxUnsigned3 = valuessigned3 - 1 !Maximum unsigned value

    !Unsigned Integer (kind=3) type
    type unsigned3_type
        character :: field(bytessigned3) = ' '
    end type unsigned3_type

    !Array of Unsigned Integer (kind=3) type
    type unsigned3_array_type
        integer :: size = 0
        type (unsigned3_type), pointer, dimension (:) :: int
    end type unsigned3_array_type


    interface assignment(=)
        module procedure copy_unsigned3, numberToUnsigned3, Unsigned3ToNumber, copy_unsigned3_array, set_array3_values
    end interface 

    contains

    !Check if the given number can be represented with Unsigned Integer(kind=3)
    function unsigned3_admitted(testValue)
        implicit none
        integer*8, intent(in) :: testValue
        logical :: unsigned3_admitted
        unsigned3_admitted = testValue.ge.0.and.testValue.lt.valuessigned3
    end function unsigned3_admitted

    !Receive an Unsigned Number and transform it to a Unsigned Integer (kind=3)
    function set_unsigned3_value(number)
        implicit none
        integer, intent(in) :: number
        type (unsigned3_type) :: set_unsigned3_value
        integer :: i
        forall (i = 0 : BytesSigned3-1)
            set_unsigned3_value%field(i+1) = achar(ibits(number, 8*i, 8))
        end forall
    end function set_unsigned3_value

    !Receive an Unsigned Integer (kind=3) and transform it to an Unsigned Number
    function get_unsigned3_value(number)
        implicit none
        type (unsigned3_type), intent(in) :: number
        integer :: get_unsigned3_value
        integer :: i, tmp_number
        get_unsigned3_value = 0
        do i = 1, BytesSigned3
            tmp_number = ichar(number%field(i))
            tmp_number = ishft(tmp_number, 8*(i-1)) 
            get_unsigned3_value = get_unsigned3_value .or. tmp_number
        end do
    end function get_unsigned3_value

    !Copy an Unsigner Integer (kind=3) number to another one
    subroutine copy_unsigned3(number_out, number_in)
        implicit none
        type (unsigned3_type), intent(out) :: number_out
        type (unsigned3_type), intent(in) :: number_in
        integer :: i
        forall (i=1:BytesSigned3)
            number_out%field(i) = number_in%field(i)
        end forall
    end subroutine copy_unsigned3

    !Assign a number to an Unsigned Integer (kind=3)
    subroutine numberToUnsigned3(number_out, number_in)
        implicit none
        type (unsigned3_type), intent(out) :: number_out
        integer, intent(in) :: number_in
        number_out = set_unsigned3_value(number_in)
    end subroutine numberToUnsigned3

    !Assign an Unsigned Integer (kind=3) to a number 
    subroutine Unsigned3ToNumber(number_out, number_in)
        implicit none
        integer, intent(out) :: number_out
        type (unsigned3_type), intent(in) :: number_in
        number_out = get_unsigned3_value(number_in)
    end subroutine Unsigned3ToNumber


    !Free the memory allocated by an array of Unsigned Integer (kind=3)
    subroutine deallocate_unsigned3_array(array)
        implicit none
        type (unsigned3_array_type), intent (inout) :: array
        integer :: err
        deallocate(array%int, stat=err)
        array%size = 0
    end subroutine deallocate_unsigned3_array

    !Allocate an array of Unsigned Integer (kind=3) for 'size' numbers
    subroutine allocate_unsigned3_array(array, size)
        implicit none
        type (unsigned3_array_type), intent (inout) :: array
        integer, intent(in) :: size
        integer :: err
        call deallocate_unsigned3_array(array)
        if (size.gt.0) then
            allocate(array%int(size), stat = err)
            if (err.eq.0) then
                write(*,*) 'ERROR in unsigned3 allocation'
            end if
            array%size = size
        end if
    end subroutine allocate_unsigned3_array

    !Returns an array of 'size' Unsigned Integer (kind=3) numbers
    function new_unsigned3_array(size)
        implicit none
        integer, intent(in) :: size
        type (unsigned3_array_type) :: new_unsigned3_array
        call allocate_unsigned3_array(new_unsigned3_array, size)
    end function new_unsigned3_array

    !Copy and array of Unsigned Integer (kind=3) to another one
    subroutine copy_unsigned3_array(array_out, array_in)
        implicit none
        type (unsigned3_array_type), intent(out) :: array_out
        type (unsigned3_array_type), intent(in) :: array_in
        integer :: i
        if (array_out%size.ne.array_in%size) then
            call allocate_unsigned3_array(array_out, array_in%size)
        end if
        do i = 1, array_in%size
            array_out%int(i) = array_in%int(i)
        end do
    end subroutine copy_unsigned3_array

    !Initialize all the positions of the array to the same value
    subroutine set_array3_values(array, number)
        implicit none
        type (unsigned3_array_type), intent(inout) :: array
        integer, intent(in) :: number
        integer :: i
        type (unsigned3_type) :: temp
        temp = number
        do i=1, array%size
            array%int(i) = temp
        end do
    end subroutine set_array3_values

    !Set an Unsigned Integer (kind=3) value in the given 
    !position of the array
    subroutine set_signed3_value(array, position, number)
        implicit none
        type (unsigned3_array_type), intent(inout) :: array
        integer, intent(in) :: position
        integer, intent(in) :: number
        array%int(position) = number
    end subroutine set_signed3_value

    !Get the Unsigned Integer (kind=3) value at the given 
    !position of the array
    function get_signed3_value(array, position)
        implicit none
        type (unsigned3_array_type), intent(in) :: array
        integer, intent(in) :: position
        integer :: get_signed3_value
        get_signed3_value = array%int(position)
    end function get_signed3_value

    !Verify that all the numbers that can be represented with the
    !Unsigned Integer (kind=3) type are correct
    subroutine test_signed3()
        implicit none
        integer :: i, j
        character*32 :: string
        write(string,*) maxUnsigned3
        write(*,*) 'Unsigned3: From 0 to ' // trim(adjustl(string))
        do i = 0, maxUnsigned3
            j = get_unsigned3_value(set_unsigned3_value(i))
            if (i-j.ne.0) then
                write(*,*) 'ERROR', i, j
            end if
        end do
    end subroutine test_signed3


end module unsigned3_module