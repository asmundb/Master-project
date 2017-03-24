subroutine nn_lambert(lon0, lat0, lon, lat, npnt, dist, ij_out)
!program test
! find nearest neighbour to point (lon0, lat0) in the grid (lon, lat)
! returns index
! length(lon) == length(lat), pairs


  implicit none
  
  integer        :: npnt
  real(kind=8)   :: lon(npnt) ,lat(npnt), dist(npnt)
  real(kind=8)   :: lon0, lat0
  integer        :: ij_out


  dist = (lon-lon0)**2 + (lat-lat0)**2
  ij_out = minloc(dist,dim=1)
  

!end program test
end subroutine nn_lambert
