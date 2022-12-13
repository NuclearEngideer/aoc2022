program dayfour

  use aocmod, only : countLines, strtoint

  implicit none

  ! File import variables
  character(256) :: filename='inputs/day4.inp'
  integer(8) :: numlines
  character(32), allocatable :: lines(:,:)
  ! Loop and processing variables
  character(32) :: line
  integer(8) :: i, grouponel, grouponeu, grouptwol, grouptwou
  integer(8) :: firstdash, comma, seconddash
  integer(8) :: fully_contain, any_overlap

  numlines = countlines(filename)
  allocate(lines(numlines,1))

  ! each line may have several digits. List directed read not so simple.
  ! First, read in the file, then parse each unique line.
  open(1, file=filename, status='old')
  read(1, '(A)') lines
  close(1)

  any_overlap=0
  fully_contain=0

  do i=1, numlines
    line=lines(i,1)
    ! process first group
    firstdash = index(line, '-')
    comma = index(line, ',')
    grouponel=strtoint(line(1:firstdash-1),'(i2)')
    grouponeu=strtoint(line(firstdash+1:comma-1),'(i2)')
    ! process second group
    seconddash = index(line(firstdash+1:),'-')+firstdash
    grouptwol=strtoint(line(comma+1:seconddash-1), '(i2)')
    grouptwou=strtoint(line(seconddash+1:), '(i2)')
    ! Are groups fully contained?
    if (grouponel >= grouptwol .and. grouponeu <= grouptwou .or. &
     &  grouponel <= grouptwol .and. grouponeu >= grouptwou) fully_contain=fully_contain+1
    ! Do the groups overlap at all?
    if (grouponel <= grouptwol .and. grouponeu <= grouptwou .and. grouponeu >= grouptwol .or. &
      & grouponeu >= grouptwou .and. grouponel >= grouptwol .and. grouponel <= grouptwou .or. &
      & grouptwol <= grouponel .and. grouptwou <= grouponeu .and. grouptwou >= grouponel .or. &
      & grouptwou >= grouponeu .and. grouptwol >= grouponel .and. grouptwol <= grouponeu .or. &
      & grouponel >= grouptwol .and. grouponeu <= grouptwou .or. &
      & grouponel <= grouptwol .and. grouponeu >= grouptwou) any_overlap=any_overlap+1
  end do
   
   write(*,'(A,i0,A)') 'There are ', fully_contain, ' fully contained groups'
   write(*,'(A,i0,A)') 'There are ', any_overlap, ' overlapping groups'

end program dayfour
