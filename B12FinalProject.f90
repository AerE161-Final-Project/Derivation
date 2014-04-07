 PROGRAM B12FinalProject
      IMPLICIT NONE
      INTEGER :: n=0 !number of points in the file
      INTEGER :: ierror=0 !Staus 0 for success
      INTEGER :: i,low
      REAL, ALLOCATABLE, DIMENSION(:) :: veolcity, time
      REAL :: temp1, temp2 !Temporary variable for swap
      
      !In the file section of OPEN put in the file you
      !want your data to be read from
      OPEN(Unit=1, File='data.txt', Status='OLD', Action='READ', &
      Iostat=ierror)

      IF(ierror==0) THEN
      DO
        READ(1,*,Iostat=ierror)temp1, temp2
        IF(ierror.NE.0)EXIT !Exits loop if there is an error in file
        n = n + 1
      END DO

      ALLOCATE(veolcity(low:n),time(low:n),STAT=ierror)!Allocate memory
         IF(ierror==0) THEN ! Successful allocation
           REWIND(Unit=1)
         !output data points
           DO i=1,n
            READ(1,*) time(i-1),veolcity(i-1)
           END DO
           IF(MOD(n-1,2)==0)THEN !Mod gets the remainder
           WRITE(*,*)"Simpson13"
           ELSE IF(MOD(n-1,2)>=0)THEN
           WRITE(*,*)"Simpson38"
           END IF !End of if statment for function type
         WRITE(*,55) "Time","Veolcity" !Prints header to screen
         55 FORMAT(3X,A,12X,A)
        DO i=0,n-1 !Print input values to the screen
         WRITE(*,*) time(i),veolcity(i)
        END DO !End of loop to print data to screen
      DEALLOCATE(veolcity,time,STAT=ierror)
        END IF !End of allocate if statement
       ELSE
        WRITE(*,*)"File open failed -- status=",ierror
       END IF  !End of file check if statement
      CLOSE(1)
      END PROGRAM B12FinalProject

