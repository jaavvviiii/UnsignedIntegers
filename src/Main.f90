
    program Integers
    
        use unsigned1_module, only: test_signed1
        use unsigned2_module, only: test_signed2
        use unsigned3_module, only: test_signed3
        use unsigned4_module, only: test_signed4

    implicit none
        
        call test_signed1()
        call test_signed2()
        call test_signed3()
        call test_signed4()

        
    end program Integers

