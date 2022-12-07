program dayone
  
  implicit none
  character(256) :: CTMP
  integer :: FID=1
  
  character(256), allocatable :: input(:)
  integer :: I=0, IERR=0, NUM_LINES=0, num_elves=0

  integer(8), allocatable :: per_elf(:)
  integer :: j=1, temp

  open(unit=FID, file='inputs/day1.inp', status='old')
  do while (IERR==0)
    read(FID,'(A8)',iostat=IERR,end=100) CTMP
    if (CTMP == ' ') then
      num_elves=num_elves+1
    end if
    NUM_LINES=NUM_LINES+1
    100 continue
  end do
  
  write(*, '(A,I0)') "Number of lines = ", NUM_LINES
  write(*, '(A,I0)') "Number of elves = ", num_elves

  allocate(input(NUM_LINES))
  allocate(per_elf(num_elves))
  
  rewind(FID)
  do I=1, NUM_LINES
    read(FID, '(A)') input(i)
    if (input(i) /= ' ') then
      read(input(i), '(i8)') temp
      per_elf(j)=per_elf(j)+temp
    else
      j=j+1
    end if
  end do

  close(FID)
  
  write(*,'(2(A,I0),A)') 'Elf number ', maxloc(per_elf), ' carries the most with ', per_elf(maxloc(per_elf)), ' calories'

  call shittysort(per_elf, size(per_elf))

  deallocate(per_elf)
  deallocate(input)

  contains 
  subroutine shittysort(array, array_len)
    implicit none
    integer(4), intent(in) :: array_len
    integer(8), intent(in) :: array(array_len)
    integer(8) :: max_val, second_max, third_max
    integer(8) :: max_val_loc, second_max_loc, third_max_loc
    integer :: i


    max_val=array(1)
    second_max=array(1)
    third_max=array(1)

    do i=1, array_len
      if (array(i) > max_val) then
        third_max=second_max
        second_max=max_val
        max_val=array(i)
      else if (array(i) > second_max) then
        third_max=second_max
        second_max=array(i)
      else if (array(i) > third_max) then 
        third_max=array(i)
      end if
    end do

    write(*,'(3(A,i0,A,i0,A))') 'Elf ', findloc(array, value=max_val), ' is carrying ', max_val, ' calories. ',&
                        &'Elf ', findloc(array, value=second_max), ' is carrying ', second_max, ' calories. ',&
                        &'Elf ', findloc(array, value=third_max), ' is carrying ', third_max, ' calories.'
    write(*,'(A,i0,A)') 'In total, they are carrying ', max_val+second_max+third_max, ' calories.'
  end subroutine shittysort
end program dayone
  
