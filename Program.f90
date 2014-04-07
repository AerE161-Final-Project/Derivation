      PROGRAM B12FinalProject
      IMPLICIT NONE
      INTEGER :: n=0 !number of points in the fial
      INTEGER :: ierror=0 !Staus 0 for success
      REAL, ALLOCATABLE, DIMENSION(:) :: veolcity, time
      REAL :: temp1, temp2 !Temporay variable for swap
    
      OPEN(Unit=1, File='data38.txt', Status='OLD', Action='READ', &
      Iostat=ierror)

      IF(ierror==0) THEN
      DO
        READ(1,*,Iostat=ierror)temp1, temp2
        IF(ierror.NE.0)EXIT
        n = n + 1
      END DO
      END IF

      ALLOCATE(veolcity(n),time(n),STAT=ierror)!Allocate memory
         IF(ierror==0) THEN ! Successful allocation
           REWIND(Unit=1)
      END PROGRAM
