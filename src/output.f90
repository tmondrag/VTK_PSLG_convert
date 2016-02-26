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

  SUBROUTINE print_to_pslg(geom,filename_poly)
    TYPE(geometry),INTENT(IN)                         :: geom
    CHARACTER(len=StrBuffLen),INTENT(IN)              :: filename_poly
    CHARACTER(len=StrBuffLen)                         :: fileroot, filename_node, filename_ele
    CHARACTER(len=StrBuffLen)                         :: buffer
    INTEGER                                           :: pfd,nfd,efd
    INTEGER                                           :: Cind, i, j, num_attributes, num_elements

    pfd = safeopen_writereplace(filename_poly)
    Cind = INDEX(filename_poly,".poly",back=.TRUE.)
    IF(Cind .EQ. 0) THEN
      fileroot = TRIM(filename_poly)
    ELSE
      fileroot = filename_poly(1:Cind-1)
    END IF
    filename_node = TRIM(fileroot) // ".node"
    filename_ele = TRIM(fileroot) // ".ele"
    nfd = safeopen_writereplace(filename_node)
    efd = safeopen_writereplace(filename_ele)

    ! write the first lines of the poly and node files
    IF(ALLOCATED(geom%vertices)) THEN
      num_elements = SIZE(geom%vertices)
      IF(ALLOCATED(geom%vertices(1)%attributes)) THEN
        num_attributes = SIZE(geom%vertices(1)%attributes)
      ELSE
        num_attributes = 0
      END IF
    ELSE
      WRITE(stderr,'(A)') "ERROR: Empty geometry, nothing to print out to PSLG" ! If you are here, you probably called this subroutine too soon
      STOP
    END IF
    WRITE(pfd,'(I8,1X,I1,1X,I4,1X,I1)') 0,2,num_attributes,1
    WRITE(nfd,'(I8,1X,I1,1X,I4,1X,I1)') num_elements,2,num_attributes,1
    ! write out node information
    DO i=1,num_elements
      buffer = ""
      WRITE(buffer,'(I8,1X,F18.12,1X,F18.12)') i,geom%vertices(i)%x,geom%vertices(i)%y
      IF(num_attributes .GT. 0) THEN
        DO j = 1,num_attributes
         WRITE(buffer,'(A,1X,F18.12)') TRIM(buffer),geom%vertices(i)%attributes(j)
        END DO
      END IF
      IF(geom%vertices(i)%isbound) THEN
        WRITE(buffer,'(A,1X,I1)') TRIM(buffer),1
      ELSE
        WRITE(buffer,'(A,1X,I1)') TRIM(buffer),0
      END IF
      WRITE(nfd,'(A)') TRIM(buffer)
    END DO
    CLOSE(nfd)
    ! Write out segment information intro
    IF(ALLOCATED(geom%segments)) THEN
      num_elements = SIZE(geom%segments)
    ELSE
      num_elements = 0
    END IF
    WRITE(pfd,'(I8,1X,I1)') num_elements,1
    ! Write out segment information
    IF(num_elements .GT. 0) THEN
      DO i=1,num_elements
        IF(geom%segments(i)%isbound) THEN
          WRITE(pfd,'(I8,1X,I8,1X,I8,1X,I1)') i, geom%segments(i)%ep1, geom%segments(i)%ep2, 1
        ELSE
          WRITE(pfd,'(I8,1X,I8,1X,I8,1X,I1)') i, geom%segments(i)%ep1, geom%segments(i)%ep2, 0
        END IF
      END DO
    END IF
    ! Write out hole information intro
    IF(ALLOCATED(geom%segments)) THEN
      num_elements = SIZE(geom%holes)
    ELSE
      num_elements = 0
    END IF
    WRITE(pfd,'(I8)') num_elements
    ! Write out hole information
    IF(num_elements .GT. 0) THEN
      DO i=1,num_elements
        WRITE(pfd,'(I8,1X,F18.12,1X,F18.12)') i,geom%holes(i)%x,geom%holes(i)%y
      END DO
    END IF
    CLOSE(pfd)
    ! Write out triangle information intro
    IF(ALLOCATED(geom%triangles)) THEN
      num_elements = SIZE(geom%triangles)
      IF(ALLOCATED(geom%triangles(1)%attributes)) THEN
        num_attributes = SIZE(geom%triangles(1)%attributes)
      ELSE
        num_attributes = 0
      END IF
      IF(geom%triangles(1)%is_subquad) THEN
        Cind = 6
      ELSE
        Cind = 3
      END IF
    ELSE
      num_elements = 0
      num_attributes = 0
      Cind = 3
    END IF
    WRITE(efd,'(I8,1X,I1,1X,I4)') num_elements,Cind,num_attributes
    IF(num_elements .GT. 0) THEN
      DO i=1,num_elements
        buffer = ""
        WRITE(buffer,'(I8,1X,I8,1X,I8,1X,I8)') i,geom%triangles(i)%c1,geom%triangles(i)%c2,geom%triangles(i)%c3
        IF(geom%triangles(i)%is_subquad) THEN
          WRITE(buffer,'(A,1X,I8,1X,I8,1X,I8)') TRIM(buffer),geom%triangles(i)%e1,geom%triangles(i)%e2,geom%triangles(i)%e3
        END IF
        IF(num_attributes .GT. 0) THEN
          DO j = 1,num_attributes
           WRITE(buffer,'(A,1X,F18.12)') TRIM(buffer),geom%triangles(i)%attributes(j)
          END DO
        END IF
        WRITE(efd,'(A)') TRIM(buffer)
      END DO
    END IF
    CLOSE(efd)
  END SUBROUTINE
END MODULE output
