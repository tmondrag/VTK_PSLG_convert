MODULE input
  USE filehandling
  USE types
  USE kindprecision
  IMPLICIT NONE

CONTAINS

  ! read in geometry data from a .poly(PSLG) file
  FUNCTION read_from_PSLG(fd) RESULT (geom)
    IMPLICIT NONE
    TYPE(geometry)            :: geom
    INTEGER                   :: fd           ! file descriptor/unit number for inputfile
    CHARACTER(LEN=StrBuffLen) :: filename_poly
    CHARACTER(LEN=StrBuffLen) :: fileroot
    CHARACTER(LEN=StrBuffLen) :: filename_node,filename_ele
    INTEGER                   :: num_vertices, num_segments, num_holes, num_tri
    INTEGER                   :: num_attributes, num_dimensions, num_bmarkers, nodes_per_tri
    INTEGER                   :: Cind,i,dummi
    INTEGER                   :: nfd          ! file descriptor/unit number for nodefile
    INTEGER                   :: efd          ! file descriptor/unit number for elementfile
    LOGICAL                   :: has_bound_flag, file_exists

    INQUIRE(UNIT=fd, NAME=filename_poly)
    Cind = INDEX(filename_poly,".poly",back=.TRUE.)
    fileroot = filename_poly(1:Cind-1)
    filename_node = TRIM(fileroot) // ".node"
    filename_ele = TRIM(fileroot) // ".ele"
    CALL skip_comments(fd)
    ! Read in vertices
    READ(fd,*,ERR=100) num_vertices, num_dimensions, num_attributes, num_bmarkers
    IF(num_vertices .EQ. 0) THEN
      INQUIRE(FILE=filename_node, EXIST=file_exists)
      IF(.NOT. file_exists) GOTO 125
      nfd = safeopen_readonly(filename_node)
      CALL skip_comments(nfd)
      READ(nfd,*,ERR=150) num_vertices, num_dimensions, num_attributes, num_bmarkers
      has_bound_flag = (num_bmarkers .NE. 0)
      ALLOCATE(geom%vertices(1:num_vertices))
      DO i=1,num_vertices
        CALL skip_comments(nfd)
        CALL read_vertex(nfd,geom%vertices(i),num_attributes,has_bound_flag)
      END DO
      CLOSE(nfd)
    ELSE
      has_bound_flag = (num_bmarkers .NE. 0)
      ALLOCATE(geom%vertices(1:num_vertices))
      DO i=1,num_vertices
        CALL skip_comments(fd)
        CALL read_vertex(fd,geom%vertices(i),num_attributes,has_bound_flag)
      END DO
    END IF
    CALL skip_comments(fd)
    ! Read in segments from .poly file
    READ(fd,*,ERR=100) num_segments, num_bmarkers
    has_bound_flag = (num_bmarkers .NE. 0)
    ALLOCATE(geom%segments(1:num_segments))
    DO i=1,num_segments
      CALL skip_comments(fd)
      CALL read_segment(fd,geom%segments(i),has_bound_flag)
    END DO
    ! check for a related .ele file and read in its information (connectivity for triangles)
    INQUIRE(FILE=filename_ele, EXIST=file_exists)
    IF(.NOT. file_exists) THEN
      WRITE(stderr,'(A,A,A)') "Warning: ",TRIM(filename_ele)," not found for reading triangulation information."
      WRITE(stderr,'(A)') "Please use a .poly PSLG file as input if triangle information is in a .ele file."
    ELSE
      efd = safeopen_readonly(filename_ele)
      CALL skip_comments(efd)
      READ(efd,*,ERR=175) num_tri, nodes_per_tri, num_attributes
      !write(stderr,*)num_tri, nodes_per_tri, num_attributes
      ALLOCATE(geom%triangles(1:num_tri))
      DO i=1,num_tri
        CALL skip_comments(efd)
        CALL read_triangle(fd,geom%triangles(i),nodes_per_tri,num_attributes)
      END DO
      CLOSE(efd)
    END IF
    CALL skip_comments(fd)
    ! Read in hole seeds
    READ(fd,*,ERR=100) num_holes
    ALLOCATE(geom%holes(1:num_holes))
    DO i=1,num_holes
      CALL skip_comments(fd)
      READ(fd,*,ERR=200) dummi,geom%holes(i)%x,geom%holes(i)%y
    END DO
    RETURN

100 WRITE(stderr,'(A,A,A)') "ERROR: Mistake reading from .poly file ",filename_poly,"."
    STOP
125 WRITE(stderr,'(A)') "ERROR: Could not determine .node filename for reading vertex info."
    WRITE(stderr,'(A)') "Please use a .poly PSLG file as input if vertex information is in a .node file."
    STOP
150 WRITE(stderr,'(A,A,A)') "ERROR: Mistake reading from .node file ",filename_node,"."
    STOP
175 WRITE(stderr,'(A,A,A)') "ERROR: Mistake reading from .ele file ",filename_ele,"."
    STOP
200 WRITE(stderr,'(A)') "ERROR: hole information misread."
    STOP
  END FUNCTION

  SUBROUTINE read_vertex(fd,th_vertex,num_attributes,has_bound_flag)
    IMPLICIT NONE
    TYPE(vertex),INTENT(INOUT)                        :: th_vertex
    INTEGER,INTENT(IN)                                :: fd   ! File descriptor/Unit number
    INTEGER,INTENT(IN)                                :: num_attributes
    LOGICAL,INTENT(IN)                                :: has_bound_flag
    CHARACTER(LEN=StrBuffLen)                         :: buffer, field
    INTEGER                                           :: i, sepindex, bound_int

    READ(fd,'(A)',ERR=100) buffer
    sepindex = INDEX(buffer,' ')
    DO WHILE(sepindex .EQ. 1)
      buffer = buffer(sepindex+1:StrBuffLen)
      sepindex = INDEX(buffer,' ')
    END DO
    field = buffer(1:sepindex-1)
    buffer = buffer(sepindex+1:StrBuffLen)
    ! the first field should be the index of this vertex. Not useful here, but it might be useful to check during debugs
    sepindex = INDEX(buffer,' ')
    DO WHILE(sepindex .EQ. 1)
      buffer = buffer(sepindex+1:StrBuffLen)
      sepindex = INDEX(buffer,' ')
    END DO
    field = buffer(1:sepindex-1)
    buffer = buffer(sepindex+1:StrBuffLen)
    ! the second field is the vertex x coordinate
    READ(field,*,ERR=100) th_vertex%x
    sepindex = INDEX(buffer,' ')
    DO WHILE(sepindex .EQ. 1)
      buffer = buffer(sepindex+1:StrBuffLen)
      sepindex = INDEX(buffer,' ')
    END DO
    field = buffer(1:sepindex-1)
    buffer = buffer(sepindex+1:StrBuffLen)
    ! the third field is the vertex y coordinate
    READ(field,*,ERR=100) th_vertex%y
    ! what follows is a loop for the attributes
    IF(num_attributes .GT. 0) THEN
      ALLOCATE(th_vertex%attributes(1:num_attributes))
      DO i=1,num_attributes
        sepindex = INDEX(buffer,' ')
        DO WHILE(sepindex .EQ. 1)
          buffer = buffer(sepindex+1:StrBuffLen)
          sepindex = INDEX(buffer,' ')
        END DO
        field = buffer(1:sepindex-1)
        buffer = buffer(sepindex+1:StrBuffLen)
        ! read in attribute
        READ(field,*,ERR=100) th_vertex%attributes(i)
      END DO
    END IF
    IF(has_bound_flag) THEN
      sepindex = INDEX(buffer,' ')
      DO WHILE(sepindex .EQ. 1)
        buffer = buffer(sepindex+1:StrBuffLen)
        sepindex = INDEX(buffer,' ')
      END DO
      field = buffer(1:sepindex-1)
      buffer = buffer(sepindex+1:StrBuffLen)
      ! read in attribute
      READ(field,*,ERR=100) bound_int
      IF(bound_int .EQ. 0) THEN
        th_vertex%isbound = .FALSE.
      ELSE IF(bound_int .EQ. 1) THEN
        th_vertex%isbound = .TRUE.
      ELSE
        GOTO 100
      END IF
    END IF
    RETURN
100 WRITE(stderr,'(A)') "ERROR: vertex information misread."
    STOP
  END SUBROUTINE read_vertex

  SUBROUTINE read_triangle(fd,th_tri,nodes_per_tri,num_attributes)
    IMPLICIT NONE
    TYPE(triangle),INTENT(INOUT)                      :: th_tri
    INTEGER,INTENT(IN)                                :: fd   ! File descriptor/Unit number
    INTEGER,INTENT(IN)                                :: nodes_per_tri
    INTEGER,INTENT(IN)                                :: num_attributes
    CHARACTER(LEN=StrBuffLen)                         :: buffer, field
    INTEGER                                           :: i, sepindex

    READ(fd,'(A)',ERR=100) buffer
    write(stderr,*) TRIM(buffer)
    sepindex = INDEX(buffer,' ')
    DO WHILE(sepindex .EQ. 1)
      buffer = buffer(sepindex+1:StrBuffLen)
      sepindex = INDEX(buffer,' ')
    END DO
    field = buffer(1:sepindex-1)
    buffer = buffer(sepindex+1:StrBuffLen)
    ! the first field should be the index of this triangle. Not useful here, but it might be useful to check during debugs
    !WRITE(stderr,*) "reading triangle ",trim(field),'.'
    IF((nodes_per_tri .EQ. 3) .OR. (nodes_per_tri .EQ. 6)) THEN
      sepindex = INDEX(buffer,' ')
      DO WHILE(sepindex .EQ. 1)
        buffer = buffer(sepindex+1:StrBuffLen)
        sepindex = INDEX(buffer,' ')
      END DO
      field = buffer(1:sepindex-1)
      buffer = buffer(sepindex+1:StrBuffLen)
      ! the second field is the index of the first corner
      READ(field,*,ERR=100) th_tri%c1
      sepindex = INDEX(buffer,' ')
      DO WHILE(sepindex .EQ. 1)
        buffer = buffer(sepindex+1:StrBuffLen)
        sepindex = INDEX(buffer,' ')
      END DO
      field = buffer(1:sepindex-1)
      buffer = buffer(sepindex+1:StrBuffLen)
      ! the third field is the index of the second corner
      READ(field,*,ERR=100) th_tri%c2
      sepindex = INDEX(buffer,' ')
      DO WHILE(sepindex .EQ. 1)
        buffer = buffer(sepindex+1:StrBuffLen)
        sepindex = INDEX(buffer,' ')
      END DO
      field = buffer(1:sepindex-1)
      buffer = buffer(sepindex+1:StrBuffLen)
      ! the fourth field is the index of the third corner
      READ(field,*,ERR=100) th_tri%c3
      !WRITE(stderr, *) "Corners at: ",th_tri%c1,th_tri%c2,th_tri%c3
    ELSE
      GOTO 100
    END IF
    ! if this is a subparametric triangle it will have six vertices
    IF(nodes_per_tri .EQ. 6) THEN
      th_tri%is_subquad = .TRUE.
      sepindex = INDEX(buffer,' ')
      DO WHILE(sepindex .EQ. 1)
        buffer = buffer(sepindex+1:StrBuffLen)
        sepindex = INDEX(buffer,' ')
      END DO
      field = buffer(1:sepindex-1)
      buffer = buffer(sepindex+1:StrBuffLen)
      ! the fifth field is the index of the edge vertex opposite the first corner
      READ(field,*,ERR=100) th_tri%e1
      sepindex = INDEX(buffer,' ')
      DO WHILE(sepindex .EQ. 1)
        buffer = buffer(sepindex+1:StrBuffLen)
        sepindex = INDEX(buffer,' ')
      END DO
      field = buffer(1:sepindex-1)
      buffer = buffer(sepindex+1:StrBuffLen)
      ! the sixth field is the index of the edge vertex opposite the second corner
      READ(field,*,ERR=100) th_tri%e2
      sepindex = INDEX(buffer,' ')
      DO WHILE(sepindex .EQ. 1)
        buffer = buffer(sepindex+1:StrBuffLen)
        sepindex = INDEX(buffer,' ')
      END DO
      field = buffer(1:sepindex-1)
      buffer = buffer(sepindex+1:StrBuffLen)
      ! the seventh field is the index of the edge vertex opposite the third corner
      READ(field,*,ERR=100) th_tri%e3
    ELSE
      th_tri%is_subquad = .FALSE.
    END IF
    ! what follows is a loop for the attributes
    IF(num_attributes .GT. 0) THEN
      ALLOCATE(th_tri%attributes(1:num_attributes))
      DO i=1,num_attributes
        sepindex = INDEX(buffer,' ')
        DO WHILE(sepindex .EQ. 1)
          buffer = buffer(sepindex+1:StrBuffLen)
          sepindex = INDEX(buffer,' ')
        END DO
        field = buffer(1:sepindex-1)
        buffer = buffer(sepindex+1:StrBuffLen)
        ! read in attribute
        READ(field,*,ERR=100) th_tri%attributes(i)
      END DO
    END IF
    RETURN
100 WRITE(stderr,'(A)') "ERROR: triangle information misread."
    STOP
  END SUBROUTINE read_triangle

  SUBROUTINE read_segment(fd,th_segment,has_bound_flag)
    IMPLICIT NONE
    TYPE(segment),INTENT(INOUT)                       :: th_segment
    INTEGER,INTENT(IN)                                :: fd   ! File descriptor/Unit number
    LOGICAL,INTENT(IN)                                :: has_bound_flag
    CHARACTER(LEN=StrBuffLen)                         :: buffer, field
    INTEGER                                           :: sepindex, bound_int

    READ(fd,'(A)',ERR=100) buffer
    sepindex = INDEX(buffer,' ')
    DO WHILE(sepindex .EQ. 1)
      buffer = buffer(sepindex+1:StrBuffLen)
      sepindex = INDEX(buffer,' ')
    END DO
    field = buffer(1:sepindex-1)
    buffer = buffer(sepindex+1:StrBuffLen)
    ! the first field should be the index of this segment. Not useful here, but it might be useful to check during debugs
    sepindex = INDEX(buffer,' ')
    DO WHILE(sepindex .EQ. 1)
      buffer = buffer(sepindex+1:StrBuffLen)
      sepindex = INDEX(buffer,' ')
    END DO
    field = buffer(1:sepindex-1)
    buffer = buffer(sepindex+1:StrBuffLen)
    ! the second field is the vertex index of the first endpoint
    READ(field,*,ERR=100) th_segment%ep1
    sepindex = INDEX(buffer,' ')
    DO WHILE(sepindex .EQ. 1)
      buffer = buffer(sepindex+1:StrBuffLen)
      sepindex = INDEX(buffer,' ')
    END DO
    field = buffer(1:sepindex-1)
    buffer = buffer(sepindex+1:StrBuffLen)
    ! the third field is the vertex index of the second endpoint
    READ(field,*,ERR=100) th_segment%ep2
    IF(has_bound_flag) THEN
      sepindex = INDEX(buffer,' ')
      DO WHILE(sepindex .EQ. 1)
        buffer = buffer(sepindex+1:StrBuffLen)
        sepindex = INDEX(buffer,' ')
      END DO
      field = buffer(1:sepindex-1)
      buffer = buffer(sepindex+1:StrBuffLen)
      ! read in boundary flag
      READ(field,*,ERR=100) bound_int
      IF(bound_int .EQ. 0) THEN
        th_segment%isbound = .FALSE.
      ELSE IF(bound_int .EQ. 1) THEN
        th_segment%isbound = .TRUE.
      ELSE
        GOTO 100
      END IF
    END IF
    RETURN
100 WRITE(stderr,'(A)') "ERROR: segment information misread."
    STOP
  END SUBROUTINE read_segment

  SUBROUTINE skip_comments(fd)
    IMPLICIT NONE
    INTEGER, INTENT(IN)                               :: fd   ! FILE DESCRIPTOR NUMBER
    CHARACTER(LEN=StrBuffLen)                         :: buffer

    ReadComments: DO
      READ(fd, '(A)') buffer
      write(stderr,*) TRIM(buffer)
      IF((buffer(1:1) /= "#") .AND. (TRIM(buffer) /= "")) exit ReadComments
    END DO ReadComments
    BACKSPACE(fd)
  END SUBROUTINE skip_comments
END MODULE input
