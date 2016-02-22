MODULE types
  USE kindprecision
  IMPLICIT NONE

  TYPE attribute
    REAL(kind=DP)                               :: values
  END TYPE attribute

  TYPE vertex
    TYPE(attribute),DIMENSION(:),ALLOCATABLE    :: attributes     ! extra attributes of vertex
    REAL(kind=DP)                               :: x,y            ! x, y coordinates of vertex
    LOGICAL                                     :: isbound = .FALSE.
  END TYPE vertex

  TYPE segment
    INTEGER(kind=IP)                            :: ep1,ep2        ! indices to the vertices on the endpoints of the segment
    LOGICAL                                     :: isbound = .FALSE.
  END TYPE segment

  TYPE hole
    REAL(kind=DP)                               :: x,y            ! x, y coordinates of hole's seedpoint
  END TYPE hole

  TYPE geometry
    TYPE(vertex),DIMENSION(:),ALLOCATABLE       :: vertices
    TYPE(segment),DIMENSION(:),ALLOCATABLE      :: segments
    TYPE(hole),DIMENSION(:),ALLOCATABLE         :: holes
  END TYPE geometry

CONTAINS

  SUBROUTINE deallocate_geometry(this_geometry)
    IMPLICIT NONE
    TYPE(geometry), INTENT(INOUT)               :: this_geometry
    INTEGER                                     :: array_length,i

    array_length = SIZE(this_geometry%vertices)
    IF(ALLOCATED(this_geometry%vertices)) THEN
      DO i=1,array_length
        IF(ALLOCATED(this_geometry%vertices(i)%attributes)) DEALLOCATE(this_geometry%vertices(i)%attributes)
      END DO
      DEALLOCATE(this_geometry%vertices)
    END IF
    IF(ALLOCATED(this_geometry%segments)) DEALLOCATE(this_geometry%segments)
    IF(ALLOCATED(this_geometry%holes)) DEALLOCATE(this_geometry%holes)
  END SUBROUTINE deallocate_geometry
END MODULE types
