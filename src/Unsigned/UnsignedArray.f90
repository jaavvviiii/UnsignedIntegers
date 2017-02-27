module unsigned_array_module
    
    use unsigned1_module
    use unsigned2_module
    use unsigned3_module
    use unsigned4_module

    implicit none

    type unsigned_array_type
        integer*1 :: Bytes = 1 !Number of bytes
        type (unsigned1_array_type) :: single
        type (unsigned2_array_type) :: short
        type (unsigned3_array_type) :: triple
        type (unsigned4_array_type) :: medium       
    end type unsigned_array_type

    interface assignment(=)
        module procedure copy_unsigned_array, set_array_values
    end interface 

    contains

    !Free the memory allocated by an array of Unsigned Integer
    subroutine deallocate_unsigned_array(array)
        implicit none
        type (unsigned_array_type), intent (inout) :: array
        array%Bytes = 1
        call deallocate_unsigned1_array(array%single)
        call deallocate_unsigned2_array(array%short)
        call deallocate_unsigned3_array(array%triple)
        call deallocate_unsigned4_array(array%medium)
    end subroutine deallocate_unsigned_array

    !Allocate an array of Unsigned Integer for 'size' numbers
    !The type of data stored depends on the precision required for the 'maxValue'
    subroutine allocate_unsigned_array(array, size, maxValue)
        implicit none
        type (unsigned_array_type), intent (inout) :: array
        integer*8, intent(in) :: size
        integer*8, intent(in) :: maxValue
        integer :: err
        call deallocate_unsigned_array(array)
        if (size.gt.0) then
            if (unsigned1_admitted(maxValue)) then
                array%Bytes = 1
                array%single = new_unsigned1_array(size)
            else if (unsigned2_admitted(maxValue)) then  
                array%Bytes = 2
                array%short = new_unsigned2_array(size)
            else if (unsigned3_admitted(maxValue)) then   
                array%Bytes = 3
                array%triple = new_unsigned3_array(size)
            else if (unsigned4_admitted(maxValue)) then 
                array%Bytes = 4  
                array%medium = new_unsigned4_array(size)
            else
                write(*,*) 'ERROR: maximum value exceeded'
            end if    
        end if
    end subroutine allocate_unsigned_array

    !Returns an array of 'size' Unsigned Integer numbers
    !The type of data stored depends on the precision required for the 'maxValue'
    function new_unsigned_array(size, maxValue)
        implicit none
        integer*8, intent(in) :: size
        integer*8, intent(in) :: maxValue
        type (unsigned_array_type) :: new_unsigned_array
        call allocate_unsigned_array(new_unsigned_array, size, maxValue)
    end function new_unsigned_array

    !Returns the number of Unsigned Integer numbers allocated in the array
    function get_unsigned_array_size(array)
        implicit none
        type (unsigned_array_type), intent(in) :: array
        integer*8 :: get_unsigned_array_size
        select case(array%Bytes)
            case(1)
                get_unsigned_array_size = array%single%size
            case(2)
                get_unsigned_array_size = array%short%size
            case(3)
                get_unsigned_array_size = array%triple%size
            case(4)
                get_unsigned_array_size = array%medium%size
            case default
                get_unsigned_array_size = 0
        end select
    end function get_unsigned_array_size

    !Copy and array of Unsigned Integer to another one
    subroutine copy_unsigned_array(array_out, array_in)
        implicit none
        type (unsigned_array_type), intent(out) :: array_out
        type (unsigned_array_type), intent(in) :: array_in
        array_out%Bytes = array_in%Bytes
        select case(array_out%Bytes)
            case(1)
                array_out%single = array_in%single
            case(2)            
                array_out%short = array_in%short
            case(3)
                array_out%triple = array_in%triple
            case(4)
                array_out%medium = array_in%medium
        end select
    end subroutine copy_unsigned_array

    !Initialize all the positions of the array to the same value
    subroutine set_array_values(array, number)
        implicit none
        type (unsigned_array_type), intent(inout) :: array
        integer*8, intent(in) :: number
        integer*2 :: short
        integer*4 :: medium
        select case(array%Bytes)
            case(1)
                short = number
                array%single = Unsigned1Tosigned1(short)
            case(2)  
                medium = number          
                array%short = Unsigned2ToSigned2(medium)
            case(3)
                medium = number   
                array%triple = medium
            case(4)
                array%medium = Unsigned4Tosigned4(number)
        end select
    end subroutine set_array_values

    !Set an Unsigned Integer value in the given 
    !position of the array
    subroutine set_Unsigned_value(array, position, number)
        implicit none
        type (unsigned_array_type), intent(inout) :: array
        integer*8, intent(in) :: position
        integer*8, intent(in) :: number
        integer*2 :: short
        integer*4 :: medium
        select case(array%Bytes)
            case(1)
                short = number
                call set_Unsigned1_value(array%single, position, short)
            case(2)            
                medium = number
                call set_Unsigned2_value(array%short, position, medium)
            case(3)
                medium = number
                call set_unsigned3_value_position(array%triple, position, medium)
            case(4)
                call set_UnSigned4_value(array%medium, position, number)
        end select
    end subroutine set_Unsigned_value

    !Get the Unsigned Integer value at the given 
    !position of the array
    function get_Unsigned_value(array, position)
        implicit none
        type (unsigned_array_type), intent(in) :: array
        integer*8 :: get_Unsigned_value
        integer*8, intent(in) :: position
        select case(array%Bytes)
            case(1)
                get_Unsigned_value = get_Unsigned1_value(array%single, position)
            case(2)            
                get_Unsigned_value = get_Unsigned2_value(array%short, position)
            case(3)
                get_Unsigned_value = get_unsigned3_value_position(array%triple, position)
            case(4)
                get_Unsigned_value = get_Unsigned4_value(array%medium, position)
        end select
    end function get_Unsigned_value

    !Returns the number of bytes required to store the Unsigned Integer array
    function get_bytes_unsigned_array(array)
        implicit none
        type (unsigned_array_type), intent (in) :: array
        integer*8 :: get_bytes_unsigned_array
        type (unsigned_array_type) :: tmp
        get_bytes_unsigned_array = sizeof(tmp) + get_bytes_unsigned1_array(array%single) + get_bytes_unsigned2_array(array%short) + get_bytes_unsigned3_array(array%triple) + get_bytes_unsigned4_array(array%medium)
    end function get_bytes_unsigned_array


    !Verify that an Unsigned Integer array is allocated according to the bytes
    !required for representing the maximum value and check the memory required
    subroutine test_unsigned_array()
        use print_module, only: get_memory_units
        use unsigned1_module, only: maxUnSigned1
        use unsigned2_module, only: maxUnSigned2
        use unsigned3_module, only: maxUnSigned3
        use unsigned4_module, only: maxUnSigned4
        implicit none
        type (unsigned_array_type) :: array
        character*32 :: string, stringSize
        integer*8, parameter :: size = maxUnSigned2
        integer*8 :: maxVal

        write(stringSize,*) size

        maxVal = maxUnSigned1
        array = new_unsigned_array(size, maxVal)
        write(string,*) maxVal
        write(*,*) 'Size for ' // trim(adjustl(stringSize)) // ' numbers and maxVal = ' // trim(adjustl(string)) // ' Unsigned array : ' // get_memory_units(get_bytes_unsigned_array(array))
        call deallocate_unsigned_array(array)

        maxVal = maxUnSigned2
        array = new_unsigned_array(size, maxVal)
        write(string,*) maxVal
        write(*,*) 'Size for ' // trim(adjustl(stringSize)) // ' numbers and maxVal = ' // trim(adjustl(string)) // ' Unsigned array : ' // get_memory_units(get_bytes_unsigned_array(array))
        call deallocate_unsigned_array(array)

        maxVal = maxUnSigned3
        array = new_unsigned_array(size, maxVal)
        write(string,*) maxVal
        write(*,*) 'Size for ' // trim(adjustl(stringSize)) // ' numbers and maxVal = ' // trim(adjustl(string)) // ' Unsigned array : ' // get_memory_units(get_bytes_unsigned_array(array))
        call deallocate_unsigned_array(array)

        maxVal = maxUnSigned4
        array = new_unsigned_array(size, maxVal)
        write(string,*) maxVal
        write(*,*) 'Size for ' // trim(adjustl(stringSize)) // ' numbers and maxVal = ' // trim(adjustl(string)) // ' Unsigned array : ' // get_memory_units(get_bytes_unsigned_array(array))
        call deallocate_unsigned_array(array)                                     

    end subroutine test_unsigned_array



end module unsigned_array_module