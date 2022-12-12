program daythree

  use aocmod, only : countLines

  implicit none
  
  integer(8) :: num_sacks, i, score, group, elf_count=1
  character(256) :: filename='inputs/day3.inp'
  character(256) :: fstring, half_length, compartmentone, compartmenttwo, line
  character(256) :: group_lines(3,1)

  num_sacks = countLines(filename)
  
  open(1, file=filename, status='old')
  do i=1,num_sacks
    read(1, '(A)') line
    write(half_length, *) len_trim(line)/2
    fstring = "(A"//trim(adjustl(half_length))//",A"//trim(adjustl(half_length))//")"
    read(line, fmt=fstring) compartmentone, compartmenttwo
    score= score + matchitem(compartmentone, compartmenttwo, len_trim(line)/2)
  end do

  write(*,'(A,i0)') 'The final score for bagged items is: ', score

  score=0
  ! Do part 2
  rewind(1)
  do i=1,num_sacks/3
    ! note, use list directed read more frequently for full-input read without 
    ! a "do" loop. Allocate an array with rows=num_lines, 1 column. Would read
    ! in one "read" statement. However, part 1 required different fstring per line
    read(1, '(A)') group_lines
    score = score+matchgroupitem(group_lines) 
  end do

  write(*,'(A,i0)') 'The final score for group items is: ', score
  
  close(1)

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

  integer function matchgroupitem(sacks)

    character(256), intent(in) :: sacks(3,1)
    character(52) :: scorearray
    integer(4) :: sack_num, item_num
    
    scorearray='abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'

    do item_num=1, len_trim(sacks(1,1))
      if ( index(sacks(2,1), sacks(1,1)(item_num:item_num)) /= 0 .and. &
         & index(sacks(3,1), sacks(1,1)(item_num:item_num)) /= 0) then
        exit
      end if
    end do
    matchgroupitem=index(scorearray, sacks(1,1)(item_num:item_num))

  end function matchgroupitem

end program
