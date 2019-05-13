


SUBROUTINE FLOW2ECO(PHI1,PHI2)
    USE GLOBAL
    IMPLICIT NONE
    REAL(SP),DIMENSION(Mloc,Nloc,SubMainGridRatio,SubMainGridRatio),INTENT(IN):: PHI1
    REAL(SP),DIMENSION(MMloc,NNloc),INTENT(OUT)::PHI2
    INTEGER :: KI,KJ,KII,KJJ





    ! convert 4- to 2-dimensions
    DO J = 1,NNloc
    DO I = 1,MMloc
      KI = INT((I-1)/SubMainGridRatio)+1
      KJ = INT((J-1)/SubMainGridRatio)+1
      KII = I-(KI-1)*SubMainGridRatio
      KJJ = J-(KJ-1)*SubMainGridRatio
      PHI2(I,J)=PHI1(KI,KJ,KII,KJJ)
    ENDDO
    ENDDO

! check
!open(2,file='tmp1.txt')
!     do j=1,(NGlob+Nghost*2)*SubMainGridRatio
!      write(2,192) (PHI2(i,j),i=1,(MGlob+2*Nghost)*SubMainGridRatio)
!     enddo
!close(2)
!192   format(3000f12.6)

END SUBROUTINE FLOW2ECO



SUBROUTINE ECO2FLOW(PHI1,PHI2)
    USE GLOBAL
    IMPLICIT NONE
    REAL(SP),DIMENSION(MMloc,NNloc),INTENT(IN)::PHI1
    REAL(SP),DIMENSION(Mloc,Nloc,SubMainGridRatio,SubMainGridRatio),INTENT(OUT):: PHI2
    INTEGER :: KI,KJ

    ! convert 2-dimension to 4-
    DO J=1,Nloc
    DO I=1,Mloc
      DO JJ=1,SubMainGridRatio
      DO II=1,SubMainGridRatio
        KI = (I-1)*SubMainGridRatio+II
        KJ = (J-1)*SubMainGridRatio+JJ
        PHI2(I,J,II,JJ)=PHI1(KI,KJ)
      ENDDO
      ENDDO
    ENDDO
    ENDDO





END SUBROUTINE ECO2FLOW








