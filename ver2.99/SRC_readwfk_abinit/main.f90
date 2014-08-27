PROGRAM READWFK
  USE debugMod, ONLY : debug
  USE CommandLineArgumentsMod, ONLY : parseCommandLineArguments
  USE abinitReaderMod, ONLY : engine
  
  IMPLICIT NONE
  
  CALL parseCommandLineArguments()
  
  CALL engine()
  
END PROGRAM READWFK
