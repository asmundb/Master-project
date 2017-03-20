subroutine nn_lon_lat(lon0, lat0, lon, lat, nlon, nlat, ij_out)
!program test
! find nearest neighbour to point (lon0, lat0) in the grid (lon, lat)
! returns index 


  implicit none
  
  integer        :: nlon, nlat
  real(kind=8)   :: lon(nlon) ,lat(nlat)
  real(kind=8)   :: lon0, lat0
  integer        :: ij_out(2)

  ij_out(1)= minloc(abs(lon(:)-lon0),dim=1)
  ij_out(2)= minloc(abs(lat(:)-lat0),dim=1)


!end program test
end subroutine nn_lon_lat
