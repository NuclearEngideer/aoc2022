program daythree

  use aocmod, only : countLines

  implicit none
  
  integer(8) :: num_sacks, i, score
  character(256) :: filename='inputs/day3.inp'
  character(256) :: fstring, half_length, compartmentone, compartmenttwo, line

  num_sacks = countLines(filename)
  
  open(1, file=filename, status='old')
  do i=1,num_sacks
    read(1, '(A)') line
    write(half_length, *) len_trim(line)/2
    fstring = "(A"//trim(adjustl(half_length))//",A"//trim(adjustl(half_length))//")"
    read(line, fmt=fstring) compartmentone, compartmenttwo
    score= score + matchitem(compartmentone, compartmenttwo, len_trim(line)/2)
  end do

  write(*,'(A,i0)') 'The final score is: ', score
  contains

  integer function matchitem(compone, comptwo, length)
    character(256), intent(in) :: compone, comptwo
    character(52) :: scorearray
    integer(4), intent(in) :: length
    integer(4) :: sharedindex, j

    scorearray='abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'

    do j=1, length
      if (index(compone, comptwo(j:j)) /= 0) sharedindex = index(compone, comptwo(j:j))
    end do
    matchitem=index(scorearray, compone(sharedindex:sharedindex))
  end function matchitem

end program
