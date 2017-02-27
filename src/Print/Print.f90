module print_module

    implicit none

    contains
    
    !Return a text that parse the given number of bytes to the best magnitude 
    function get_memory_units(bytes)
        implicit none
        integer*8, intent(in) :: bytes
        character (len=:), allocatable ::get_memory_units
        !Local variables
        integer :: exp
        complex :: tmp_complex
        character*124 temp_string
        real :: tmp_real
        exp = 0 !bytes
        tmp_real = bytes
        do while(tmp_real/1024.gt.1)
            tmp_real = tmp_real/1024
            exp = exp + 1
            if (exp.eq.8) exit
        end do
        write(temp_string, *) tmp_real
        select case(exp)
            case (0)
                get_memory_units = trim(adjustl(temp_string)) // ' Bytes'
            case (1)
                get_memory_units = trim(adjustl(temp_string)) // ' KBytes'
            case (2)
                get_memory_units = trim(adjustl(temp_string)) // ' MBytes'
            case (3)
                get_memory_units = trim(adjustl(temp_string)) // ' GBytes'
            case (4)
                get_memory_units = trim(adjustl(temp_string)) // ' TBytes'
            case (5)
                get_memory_units = trim(adjustl(temp_string)) // ' PBytes'
            case (6)
                get_memory_units = trim(adjustl(temp_string)) // ' EBytes'
            case (7)
                get_memory_units = trim(adjustl(temp_string)) // ' ZBytes'
            case default
                get_memory_units = trim(adjustl(temp_string)) // ' YBytes'
            end select
    end function get_memory_units

end module print_module