PROGRAM B10FinalProject
      IMPLICIT NONE
      INTEGER :: n=0 !number of points in the file
      INTEGER :: ierror=0 !Staus 0 for success
      INTEGER :: i,low=0,subinter !subinter is n for functions
      REAL, ALLOCATABLE, DIMENSION(:) :: V, t, acclr !veolcity(m/s) and time(sec)
      REAL :: temp1, temp2 !Temporary variable for swap
      REAL :: Simpson13, Simpson38,Distance,inc !inc is h for functions
      CHARACTER(10) :: filename
      CHARACTER(3) :: unit1="sec"   !set unit display
      CHARACTER(5) :: unit2="m/s^2" !set unit display
      CHARACTER(8) :: timedisp="at time="      !set string time display
      CHARACTER(13) :: accdisp="acceleration=" ! set string acceleration display
      WRITE(*,*)"Enter in file name with declaraction"
      READ(*,*)filename
      !want your data to be read from
      OPEN(Unit=1, File=filename, Status='OLD', Action='READ', &
      Iostat=ierror)

      IF(ierror==0) THEN
       DO
        READ(1,*,Iostat=ierror)temp1, temp2
        IF(ierror.NE.0)EXIT !Exits loop if there is an error in file
        n = n + 1
       END DO
        subinter = n-1 !number of subintervals
       ALLOCATE(t(low:n),V(low:n),acclr(low:n),STAT=ierror)!Allocate memory
         IF(ierror==0) THEN ! Successful allocation
           REWIND(Unit=1)
         !output data points
           DO i=1,n
            READ(1,*) t(i-1),V(i-1)
           END DO
         inc= (t(n-1)-t(0))/(n-1) !Calculates the increment of time
         WRITE(*,*)"Data points from file ",filename
         WRITE(*,*)"_________________________________"
         WRITE(*,55) "Time","Veolcity" !Prints header to screen
         55 FORMAT(3X,A,12X,A)
         DO i=0,n-1 !Print input values to the screen
         WRITE(*,*) t(i),"sec",V(i),"m/s"
         21 FORMAT(F8.2,A,X,F8.2,A)
        END DO !End of loop to print data to screen
           IF(MOD(n-1,2)==0)THEN !Mod gets the remainder
           Distance = Simpson13(V,inc,subinter)
           WRITE(*,*)"Distance covered in",t(n-1), "sec is ",Distance,&
           "m"
           54 FORMAT(A,X,F4.1,X,A,X,F8.3,X,A)
           CALL Acceleration(V,inc,subinter,acclr) ! subroutine
           !Will display the acceleration at each time interval
           DO i=0,n-1
           WRITE(*,20)timedisp,t(i),unit1,accdisp,acclr(i),unit2
           END DO!End of time vs acceleration loop
           ELSE IF(MOD(n-1,2)>=0)THEN
           Distance = Simpson38(V,inc,subinter)
           WRITE(*,*)"Distance covered in",t(n-1),"sec is ",Distance,&
           "m"
           CALL Acceleration(V,inc,subinter,acclr) !subroutine
           !Will display the accerleration at each time interval
           DO i=0,n-1
           WRITE(*,20)timedisp,t(i),unit1,accdisp,acclr(i),unit2
           20 FORMAT(A,X,F8.2,A,X,A,X,F8.2,A)
           END DO !End of time vs acceleration loop
           END IF !End of if statment for function type
       DEALLOCATE(t,V,acclr,STAT=ierror)
        END IF !End of allocate if statement
      ELSE
        WRITE(*,*)"File open failed -- status=",ierror
      END IF  !End of file check if statement
      CLOSE(1)!closes file opened by program
      END PROGRAM B10FinalProject

      REAL FUNCTION Simpson13(V,h,n)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: n
      REAL, INTENT(IN) :: h
      INTEGER :: i,j
      REAL, INTENT(IN), DIMENSION(0:n) :: V
      REAL :: S1=0, S2=0
      DO i=1,n-1,2
      S1= S1 + V(i)
      END DO
      DO j=2,n-2,2
      S2= S2 +V(j)
      END DO
      Simpson13 = (h/3)*(V(0)+4*S1+2*S2+V(n))
      END FUNCTION

      REAL FUNCTION Simpson38(V,h,n)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: n
      REAL, INTENT(IN) :: h
      INTEGER :: i,j,k
      REAL, INTENT(IN), DIMENSION(0:n) :: V
      REAL :: S1=0, S2=0, S3=0
      DO i=1,n-2,3
      S1= S1 + V(i)
      END DO
      DO j=2,n-1,3
      S2= S2 + V(j)
      END DO
      DO k=3,n-3,3
      S3= S3 + V(k)
      END DO
      Simpson38 =((3./8)*h)*(V(0)+3*S1+3*S2+2*S3+V(n))
      END FUNCTION

      SUBROUTINE Acceleration(V,h,n,acclr)
      IMPLICIT NONE
      REAL, INTENT(IN) :: h
      INTEGER, INTENT(IN) :: n
      INTEGER :: i
      REAL, DIMENSION(0:n), INTENT(IN) :: V
      REAL, DIMENSION(0:n), INTENT(OUT) :: acclr
      acclr(0)=(-3*V(0)+4*V(1)-V(2))/(2*h)
      DO i=1,n-1
      acclr(i)= (V(i+1)-V(i-1))/(2*h)
      END DO
      acclr(n)= (V(n-2)-4*V(n-1)+3*V(n))/(2*h)
      END SUBROUTINE Acceleration
