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
    WRITE(fd,'(A)') 'DATASET UNSTRUCTURED_GRID'
    WRITE(fd,'(/A7,i10,A7)') 'POINTS ' ,SIZE(geom%vertices)+SIZE(geom%holes),' DOUBLE'
    ! write out the vertices
    DO i=1,SIZE(geom%vertices)
      WRITE(fd,'(F18.12,1X,F18.12,1X,F18.12)') geom%vertices(i)%x, geom%vertices(i)%y, 0.
    END DO
    ! write out the hole seeds
    DO i=1,SIZE(geom%holes)
      WRITE(fd,'(F18.12,1X,F18.12,1X,F18.12)') geom%holes(i)%x, geom%holes(i)%y, 0.
    END DO
    ! write out the connectivity.
    IF(ALLOCATED(geom%triangles)) THEN
      IF(geom%triangles(1)%is_subquad) THEN
        WRITE(fd,'(/A,1X,I8,1X,I8)') 'CELLS ',SIZE(geom%segments)+SIZE(geom%triangles), &
                                      3*SIZE(geom%segments)+7*SIZE(geom%triangles)
      ELSE
        WRITE(fd,'(/A,1X,I8,1X,I8)') 'CELLS ',SIZE(geom%segments)+SIZE(geom%triangles), &
                                      3*SIZE(geom%segments)+4*SIZE(geom%triangles)
      END IF
    ELSE
      WRITE(fd,'(/A,1X,I8,1X,I8)') 'CELLS ',SIZE(geom%segments), 3*SIZE(geom%segments)
    END IF
    ! Write out segment connectivity. Remember, vtk point indices start with 0
    DO i=1,SIZE(geom%segments)
      WRITE(fd,'(I2,1X,I8,1X,I8)') 2, geom%segments(i)%ep1-1, geom%segments(i)%ep2-1
    END DO
    ! Write out triangle connectivity. Remember, vtk point indices start with 0
    ! remember vtk likes its points ordered on a quad triangle so that the first edge is opposite the third corner, the second edge is opposite the first corner, and the third edge is opposite the second corner
    IF(ALLOCATED(geom%triangles)) THEN
      DO i=1,SIZE(geom%triangles)
        IF(geom%triangles(i)%is_subquad) THEN
          WRITE(fd,'(I2,1X,I8,1X,I8,1X,I8,1X,I8,1X,I8,1X,I8)') 6, geom%triangles(i)%c1-1, geom%triangles(i)%c2-1, &
                         geom%triangles(i)%c3-1, geom%triangles(i)%e3-1, geom%triangles(i)%e1-1, geom%triangles(i)%e2-1
        ELSE
          WRITE(fd,'(I2,1X,I8,1X,I8,1X,I8)') 3, geom%triangles(i)%c1-1, geom%triangles(i)%c2-1, geom%triangles(i)%c3-1
        END IF
      END DO
    END IF
    ! Write out the cell types: 3 for straight line segments, 5 for simple triangles, and 22 for quadratic triangles
    WRITE(fd,'(/A,1X,I8)') 'CELL_TYPES',SIZE(geom%segments)+SIZE(geom%triangles)
    DO i=1,SIZE(geom%segments)
      WRITE(fd,'(I2)') 3
    END DO
    IF(ALLOCATED(geom%triangles)) THEN
      DO i=1,SIZE(geom%triangles)
        IF(geom%triangles(i)%is_subquad) THEN
          WRITE(fd,'(I2)') 22
        ELSE
          WRITE(fd,'(I2)') 5
        END IF
      END DO
    END IF
    ! write out attributes
    ! the number of attributes should be the same for each point
    ! each vertex (hole seeds included) will all have to the same number of scalars defined
    ! define one scalar at least to be the boundary flag
    num_attributes = 0
    IF(ALLOCATED(geom%vertices(1)%attributes)) num_attributes = SIZE(geom%vertices(1)%attributes)
    WRITE(fd,'(/A11,I10)') 'POINT_DATA ',SIZE(geom%vertices)+SIZE(geom%holes)
    WRITE(fd,'(/A,/A)') 'SCALARS vertex_boundary int','LOOKUP_TABLE default'
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
    WRITE(fd,'(/A,/A)') 'SCALARS hole_seeds int','LOOKUP_TABLE default'
    DO i=1,SIZE(geom%vertices)
      WRITE(fd,'(I1)') 0
    END DO
    DO i=1,SIZE(geom%holes)
      WRITE(fd,'(I1)') 1
    END DO
    IF(num_attributes .GT. 0) THEN
      DO j=1,num_attributes
        WRITE(fd,'(/A,I0.2,A,/A)') 'SCALARS attribute_',j,' double','LOOKUP_TABLE default'
        DO i=1,SIZE(geom%vertices)
          WRITE(fd,'(F18.12)') geom%vertices(i)%attributes(j)
        END DO
        DO i=1,SIZE(geom%holes)
          WRITE(fd,'(F18.12)') 0.
        END DO
      END DO
    END IF
    ! segments may also be bounds, so use cell_data
    num_attributes = 0
    IF(ALLOCATED(geom%triangles)) THEN
      IF(ALLOCATED(geom%triangles(1)%attributes)) num_attributes = SIZE(geom%triangles(1)%attributes)
      WRITE(fd,'(/A10,I10)') 'CELL_DATA ',SIZE(geom%segments)+SIZE(geom%triangles)
    ELSE
      WRITE(fd,'(/A10,I10)') 'CELL_DATA ',SIZE(geom%segments)
    END IF
        WRITE(fd,'(/A,/A)') 'SCALARS segment_boundary int','LOOKUP_TABLE default'
    DO i=1,SIZE(geom%segments)
      IF(geom%segments(i)%isbound) THEN
        WRITE(fd,'(I1)') 1
      ELSE
        WRITE(fd,'(I1)') 0
      END IF
    END DO
    IF(ALLOCATED(geom%triangles)) THEN
      DO i=1,SIZE(geom%triangles)
        WRITE(fd,'(I1)') 0
      END DO
    END IF
    ! write out triangle element attributes
    IF(num_attributes .GT. 0) THEN
      DO j=1,num_attributes
        WRITE(fd,'(/A,I0.2,A,/A)') 'SCALARS cell_attribute_',j,' double','LOOKUP_TABLE default'
        DO i=1,SIZE(geom%segments)
          WRITE(fd,'(F18.12)') 0.
        END DO
        DO i=1,SIZE(geom%triangles)
          WRITE(fd,'(F18.12)') geom%triangles(i)%attributes(j)
        END DO
      END DO
    END IF

  END SUBROUTINE print_to_vtk

  SUBROUTINE print_to_pslg(geom,fd)
    integer geom
    integer fd
  END SUBROUTINE
END MODULE output
