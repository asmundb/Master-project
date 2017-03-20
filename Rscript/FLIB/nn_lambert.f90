subroutine nn_lambert(lon0, lat0, lon, lat, npnt, ij_out)
!program test
! find nearest neighbour to point (lon0, lat0) in the grid (lon, lat)
! returns index
! length(lon) == length(lat), pairs


  implicit none
  
  integer        :: npnt
  real(kind=8)   :: lon(npnt) ,lat(npnt)
  real(kind=8)   :: lon0, lat0
  integer        :: ij_out

  ij_out = minloc((lon-lon0)**2 + (lat-lat0)**2,dim=1)
  

!end program test
end subroutine nn_lambert
