PROGRAM Simpson13 
IMPLICIT NONE 
INTEGER :: n=0 
INTEGER :: ierror=0 
INTEGER :: i 
REAL, ALLOCATABLE, 
DIMENSION(:) :: V, t 
REAL :: temp1, temp2,S13,S3,h 
OPEN(unit=1, File='data.txt', Status='OLD', Action='READ', Iostat=ierror) 
IF(ierror==0) THEN 
DO READ(1,*,Iostat=ierror) temp1, temp2 
IF (ierror.NE.0) EXIT n=n+1 
END DO 
WRITE(*,*) 'Allocating size=', n ALLOCATE(t(n), V(n), STAT=ierror) 
IF(ierror==0) THEN 
REWIND (Unit=1) 
DO i=1,n 
READ(1,*) t(i), V(i) 
END DO 
h=t(2)-t(1) 
S3 = V(1)+4*(V(2)+V(4)+V(6)+V(8))+2*(V(3)+V(5)+V(7))+V(9) S13=(S3*h)/3 
WRITE(*,*) 'Distance covered in 18.0 sec is',S13,'m'
