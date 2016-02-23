MODULE output
  USE filehandling
  USE types
  USE kindprecision
  IMPLICIT NONE

CONTAINS

  SUBROUTINE print_to_vtk(geom,fd)
    IMPLICIT NONE
    TYPE(geometry),INTENT(IN)            :: geom
    INTEGER,INTENT(IN)                   :: fd           ! file descriptor/unit number for outputfile
    INTEGER                              :: i, j, num_attributes

    ! Write Header Information
    WRITE(fd,'(A26)') '# vtk DataFile Version 3.0'
    WRITE(fd,'(A31)') 'Translation of PSLG format file'
    WRITE(fd,'(A5)') 'ASCII'
    WRITE(fd,'(A16)') 'DATASET POLYDATA'
    WRITE(fd,'(/A7,i10,A6)') 'POINTS ' ,SIZE(geom%vertices)+SIZE(geom%holes),' DOUBLE'
    ! write out the vertices
    DO i=1,SIZE(geom%vertices)
      WRITE(fd,'(F18.12,1X,F18.12,1X,F18.12)') geom%vertices(i)%x, geom%vertices(i)%y, 0.
    END DO
    ! write out the hole seeds
    DO i=1,SIZE(geom%holes)
      WRITE(fd,'(F18.12,1X,F18.12,1X,F18.12)') geom%holes(i)%x, geom%holes(i)%y, 0.
    END DO
    ! write out the segment connectivity. Every segment only consists of two points
    WRITE(fd,'(/A,1X,I8,1X,I8)') 'LINES ',SIZE(geom%segments),3*SIZE(geom%segments)
    DO i=1,SIZE(geom%segments)
      WRITE(fd,'(I1,1X,I8,1X,I8)') 2, geom%segments(i)%ep1, geom%segments(i)%ep2
    END DO
    ! write out attributes
    ! the number of attributes should be the same for each point
    ! each vertex (hole seeds included) will all have to the same number of scalars defined
    ! define one scalar at least to be the boundary flag
    num_attributes = 0
    IF(ALLOCATED(geom%vertices(1)%attributes)) num_attributes = SIZE(geom%vertices(1)%attributes)
    WRITE(fd,'(/A11,I10)') 'POINT_DATA ',SIZE(geom%vertices)+SIZE(geom%holes)
    WRITE(fd,'(/A,/A)') 'SCALARS int vertex_boundary','LOOKUP_TABLE default'
    DO i=1,SIZE(geom%vertices)
      IF(geom%vertices(i)%isbound) THEN
        WRITE(fd,'(I1)') 1
      ELSE
        WRITE(fd,'(I1)') 0
      END IF
    END DO
    DO i=1,SIZE(geom%holes)
      WRITE(fd,'(I1)') 0
    END DO
    ! mark off the hole seeds in the point set
    WRITE(fd,'(/A,/A)') 'SCALARS int hole_seeds','LOOKUP_TABLE default'
    DO i=1,SIZE(geom%vertices)
      WRITE(fd,'(I1)') 0
    END DO
    DO i=1,SIZE(geom%holes)
      WRITE(fd,'(I1)') 1
    END DO
    IF(num_attributes .GT. 0) THEN
      DO j=1,num_attributes
        WRITE(fd,'(/A,I0.2,/A)') 'SCALARS DOUBLE attribute_',j,'LOOKUP_TABLE default'
        DO i=1,SIZE(geom%vertices)
          WRITE(fd,'(F18.12)') geom%vertices(i)%attributes(j)
        END DO
        DO i=1,SIZE(geom%holes)
          WRITE(fd,'(F18.12)') 0.
        END DO
      END DO
    END IF
    ! segments may also be bounds, so use cell_data
    WRITE(fd,'(/A10,I10)') 'CELL_DATA ',SIZE(geom%segments)
    WRITE(fd,'(/A,/A)') 'SCALARS int segment_boundary','LOOKUP_TABLE default'
    DO i=1,SIZE(geom%segments)
      IF(geom%segments(i)%isbound) THEN
        WRITE(fd,'(I1)') 1
      ELSE
        WRITE(fd,'(I1)') 0
      END IF
    END DO

  END SUBROUTINE print_to_vtk

END MODULE output
