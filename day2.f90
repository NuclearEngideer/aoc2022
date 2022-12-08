program daytwo
  
  implicit none

  character(256) :: CTMP
  integer :: FID=1
  
  integer :: I=0, IERR=0, num_rounds=0
  character(1), allocatable :: opponent(:), you(:)
  integer(8) :: score

  open(unit=FID, file='inputs/day2.inp', status='old')
  do while (IERR==0)
    read(FID,'(A8)',iostat=IERR,end=100) CTMP
    num_rounds=num_rounds+1
    100 continue
  end do

  allocate(opponent(num_rounds))
  allocate(you(num_rounds))

  rewind(FID)
  do i=1, num_rounds
    read(FID, '(A1,x,A1)') opponent(i), you(i)
  end do
  close(FID)

! part 1
! A=rock, B=Paper, C=sCissors
! X=rock, Y=Paper, Z=scissors
! draw=3 points, win=6, lose=0
! rock=1, paper=2, scissors=3
  score=0
  do i=1, num_rounds
    if (opponent(i) == 'A') then
      if (you(i) == 'X') score=score+3+1
      if (you(i) == 'Y') score=score+6+2
      if (you(i) == 'Z') score=score+3
    else if (opponent(i) == 'B') then
      if (you(i) == 'X') score=score+1
      if (you(i) == 'Y') score=score+3+2
      if (you(i) == 'Z') score=score+6+3
    else if (opponent(i) == 'C') then
      if (you(i) == 'X') score=score+6+1
      if (you(i) == 'Y') score=score+2
      if (you(i) == 'Z') score=score+3+3
    end if
  end do        

  write(*,'(A,i0)') 'The score following the strategy guide correctly is ', score

  score=0

! Part 2
! If X, lose round. If Y, draw. If Z, win.
  do i=1, num_rounds
    if (opponent(i)=='A') then
      if (you(i)=='X') score=score+0+3
      if (you(i)=='Y') score=score+3+1
      if (you(i)=='Z') score=score+6+2
    else if (opponent(i)=='B') then
      if (you(i)=='X') score=score+0+1
      if (you(i)=='Y') score=score+3+2
      if (you(i)=='Z') score=score+6+3
    else if (opponent(i)=='C') then
      if (you(i)=='X') score=score+0+2
      if (you(i)=='Y') score=score+3+3
      if (you(i)=='Z') score=score+6+1
    end if
  end do
    
  write(*,'(A,i0)') 'The score following the strategy guide correctly is ', score
  

end program
