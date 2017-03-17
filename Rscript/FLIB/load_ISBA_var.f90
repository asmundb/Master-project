module load_ISBA
contains
subroutine load_ISBA_var(path, varname, val)

  use netcdf

  implicit none

  character(len=300)             :: path
  character(len=10)              :: varname
  character(len=300),allocatable :: filename(:)
  character(len=500)             :: lscommand
  real(kind=8),allocatable       :: tmp1(:,:,:), tmp2(:,:,:)
  real(kind=8)                   :: val(:)
  real                           :: r
  integer                        :: nfiles, k,kk, ncid, status, VarId, xx, yy, time
  logical                        :: exist

  !!!! List directories !!!!

  write(lscommand,*) 'ls -d -1 ', trim(path), '/*/ISBA_PROGNOSTIC* > tmp.txt'
!  print*, trim(lscommand)
  call system(trim(lscommand))
  open(11, file='tmp.txt', action="read")
  k = 0
  do 
    read(11, fmt='(a)', iostat=status) r
    if  (status /= 0) exit
    k = k+1
  enddo
  nfiles = k
  allocate(filename(nfiles))
  rewind(11)
  do k = 1,nfiles
    read(11, '(a)') filename(k)
  enddo
  close(11)

  !!!! Read files !!!!
  status = nf90_open(trim(filename(1)), NF90_NOWRITE, ncid)
  status = nf90_inq_varid(ncid, varname, VarId)
  status = nf90_inquire_dimension(ncid, 1, len=xx)
  status = nf90_inquire_dimension(ncid, 2, len=yy)
  status = nf90_inquire_dimension(ncid, 4, len=time)
  status = nf90_close(ncid)
!  print*, xx,yy,time

  allocate(tmp1(xx,yy,time))
  allocate(tmp2(xx,yy,nfiles*time))
  tmp2 = 666

  kk = 1
  do k=1,nfiles
    status = nf90_open(trim(filename(k)), NF90_NOWRITE, ncid)
    if (status /= 0 ) exit
    status = nf90_inq_varid(ncid, "WG2", VarId)
    if (status /= 0 ) exit
    status = nf90_get_var(ncid, VarId, tmp1, start=(/1,1,1,1/), count=(/xx,xx,1,time/))
    if (status /= 0 ) exit
    status = nf90_close(ncid)
    if (status /= 0 ) exit
    tmp2(:,:,kk:(kk+5)) = tmp1
  enddo

  val = reshape(tmp2, (/xx*yy*time*nfiles/))

end subroutine load_ISBA_var
end module load_ISBA

program test
 
  use load_ISBA

  implicit none

  character(len=300) :: path
  character(len=10)  :: varname
  real(kind=8)       :: val(111*111*32*6)

  !!!! input !!!!
  path = "/lustre/storeB/users/asmundb/surfex/RESULTS/2016/lowcloud/SEKF/ISBA/"
  varname = "WG2"
  val = 777
 
   
  call load_ISBA_var(path, varname, val)

  print*, val

end program test
