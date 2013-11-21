MODULE UtilitiesMod
  ! Just a collection of utility subroutines and functions that are used.
  
  ! Contains:
  !   CharToInt
  !   IntToChar
  !   convertToLowerCase
  !   
  
CONTAINS
  
  SUBROUTINE CharToInt(inChar,outInt)
    ! Turns the character 0, 1, 2, etc. to its respective digit integer 0, 1, 2, etc.
    
    IMPLICIT NONE
    CHARACTER(LEN=1), INTENT(IN) :: inChar
    INTEGER, INTENT(OUT) :: outInt

    SELECT CASE (inChar)
    CASE ("0")
       outInt = 0
    CASE ("1")
       outInt = 1
    CASE ("2")
       outInt = 2
    CASE ("3")
       outInt = 3
    CASE ("4")
       outInt = 4
    CASE ("5")
       outInt = 5
    CASE ("6")
       outInt = 6
    CASE ("7")
       outInt = 7
    CASE ("8")
       outInt = 8
    CASE ("9")
       outInt = 9
    CASE DEFAULT
       WRITE(*,*) ""
       WRITE(*,*) "    ERROR: Passed illegal value to CharToInt subroutine"
       WRITE(*,*) "    value passed is:", inChar
       WRITE(*,*) "    STOPPING"
       WRITE(*,*) ""
       STOP "Illegal value passed to CharToInt subroutine"
    END SELECT
  END SUBROUTINE CharToInt
  
  SUBROUTINE IntToChar(inInt, outChar)
    ! Turns the integer 1, 2, 3, etc. into characters.
    IMPLICIT NONE
    CHARACTER(LEN=1), INTENT(OUT) :: outChar
    INTEGER, INTENT(IN) :: inInt
    
    SELECT CASE (inInt)
    CASE (0)
       outChar = "0"
    CASE (1)
       outChar = "1"
    CASE (2)
       outChar = "2"
    CASE (3)
       outChar = "3"
    CASE (4)
       outChar = "4"
    CASE (5)
       outChar = "5"
    CASE (6)
       outChar = "6"
    CASE (7)
       outChar = "7"
    CASE (8)
       outChar = "8"
    CASE (9)
       outChar = "9"
    CASE DEFAULT
       WRITE(*,*) ""
       WRITE(*,*) "    ERROR: Passed illegal value to IntToChar subroutine"
       WRITE(*,*) "    STOPPING"
       WRITE(*,*) ""
       STOP "Illegal value passed to IntToChar subroutine"
    END SELECT
    
  END SUBROUTINE IntToChar
  
  SUBROUTINE convertToLowerCase( string )
    ! with help from http://www.jcameron.com/vms/source/UPCASE.FOR
    CHARACTER*(*) string
    INTEGER*4 string_length, i
    
    string_length = LEN( string )
    
    DO i = 1, string_length
       
       IF (LGE( string(i:i),'A') .AND. LLE( string(i:i),'Z')) THEN
          string (i:i) = CHAR(ICHAR( string (i:i) ) + 32)
       ELSE
       ENDIF
       
    END DO
    RETURN
    
  END SUBROUTINE convertToLowerCase
  
END MODULE UtilitiesMod
