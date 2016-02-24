PROGRAM convert_PSLG_to_VTK
  USE types
  USE filehandling
  USE kindprecision
  USE input
  USE output
  IMPLICIT NONE
  TYPE(geometry)            :: my_geometry
  CHARACTER(len=StrBuffLen) :: arg_buffer
  INTEGER                   :: num_args, iunit, ounit

  num_args = COMMAND_ARGUMENT_COUNT()
  IF(num_args .EQ. 0) THEN
    WRITE(stderr,'(A)') "Using standard input as the input file and standard output as the output file."
    WRITE(stderr,'(A)') "If this wasn't your intent then"
    WRITE(stderr,'(A)') "USAGE: convert_PSLG_to_VTK inputfilename outputfilename"
    iunit = stdin
    ounit = stdout
  ELSE IF(num_args .EQ. 2) THEN
    CALL GET_COMMAND_ARGUMENT(1,arg_buffer)
    iunit = safeopen_readonly(arg_buffer)
    CALL GET_COMMAND_ARGUMENT(2,arg_buffer)
    ounit = safeopen_writereplace(arg_buffer)
  ELSE
    WRITE(stderr,'(A)') "USAGE: convert_PSLG_to_VTK inputfilename outputfilename"
    STOP 10
  END IF

  CALL find_IU_info(iunit)
  my_geometry = read_from_PSLG(iunit)
  CALL print_to_vtk(my_geometry,ounit)
  CALL deallocate_geometry(my_geometry)
  CALL find_IU_info(ounit)

  IF(iunit .NE. stdin) CLOSE(iunit)
  IF(ounit .NE. stdout) CLOSE(ounit)
END PROGRAM convert_PSLG_to_VTK
