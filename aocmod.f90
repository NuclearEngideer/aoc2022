module aocmod

  contains

  integer(8) function countLines(filename)
    implicit none
    character(*), intent(in) :: filename
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

  integer(8) function strtoint(intchar, fstring)
    implicit none
    character(*), intent(in) :: intchar
    character(*), intent(in) :: fstring
    read(intchar, fstring) strtoint
  end function strtoint

  character(256) function inttostr(intchar, fstring)
    implicit none
    integer(8), intent(in) :: intchar
    character(*), intent(in) :: fstring
    write(inttostr, fstring) intchar
  end function inttostr

end module aocmod
