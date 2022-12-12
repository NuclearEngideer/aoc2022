module aocmod

  contains

  integer(8) function countLines(filename)
    implicit none
    character(256), intent(in) :: filename
    character(256) :: ctmp
    integer :: FID=1, ierr=0, number=0


    open(unit=FID, file=filename, status='old')
    do while (ierr==0)
      read(FID, *, iostat=ierr, end=100) ctmp
      number=number+1
      100 continue
    end do
    close(FID)
    countLines=number
  end function countLines

end module aocmod
