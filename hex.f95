module hex
  implicit none

contains
  !Read a word in from the user
  subroutine readWord(w)
    character(len = 16), intent(out) :: w ! Where we will store the input
    write(*,1000)
    read(*,*) w

    1000 format(' Please enter a word: ')
  end subroutine readWord

  ! Converts a word to hexadecimal
  subroutine word2hex(w,h,l)
    character(len = 16), intent(inout) :: w ! Word to convert
    integer, dimension(0:31), intent(out) :: h ! Array to store the hexadecimal
    integer, intent(out) :: l ! length of the word
    integer :: remainder, ascii, quotient ! For calculating the hex value of a character
    integer :: i ! For iterating through the loop

    ! Convert each individual character to it's hex and store it in h
    do i = 1,l
      ascii = ichar(w(i:i))
      quotient = ascii/16
      remainder = mod(ascii,16)

      h( (i-1)*2 ) = quotient ! Set first value of the char to the quotient
      h( ( (i-1)*2) + 1 ) = remainder ! Set the second value to the remainder
    end do

    ! Fill the rest of h with 0
    do i = l*2,31
      h(i) = 0
    end do
  end subroutine word2hex

  subroutine printhex(h,l)
    integer, dimension(0:31), intent(out) :: h ! The word in hexadecimal
    integer, intent(inout) :: l ! The length of the word in text
    integer :: i ! This is used to iterate our loop
    character(len = l*2) :: w ! A string so we can print the word with letters

    ! Loop through every character
    do i = 1,l*2 ! (l*2) because each character is represented by 2 characters in hex
      select case(h(i-1))
      ! For all values >= 10, we need to convert to the hex letter
      case(10)
        w(i:i) = 'A'
      case(11)
        w(i:i) = 'B'
      case(12)
        w(i:i) = 'C'
      case(13)
        w(i:i) = 'D'
      case(14)
        w(i:i) = 'E'
      case(15)
        w(i:i) = 'F'
      ! Otherwise we can just store the integer as a character
      case default
        w(i:i) = char(h(i-1)+48)
      end select
    end do

    ! Write the word
    write(*,1000)
    write(*,*) w
    1000 format(' Hexadecimal word: ')
  end subroutine printhex

end module hex
