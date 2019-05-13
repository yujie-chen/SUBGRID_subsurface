














SUBROUTINE UPDATE_BND_GHOST(PHI)
     USE GLOBAL
     IMPLICIT NONE
     REAL(SP),INTENT(INOUT) :: PHI(Mloc,Nloc,SubMainGridRatio,SubMainGridRatio)

    ! update bed at real boundary ghost cells





      DO I=1,Nghost
      DO J=Jbeg,Jend
        DO II=1,SubMainGridRatio
        DO JJ=1,SubMainGridRatio
          PHI(I,J,II,JJ)=PHI(Ibeg,J,SubMainGridRatio+1-II,JJ)
        ENDDO
        ENDDO
      ENDDO
      ENDDO







      DO I=Iend+1,Mloc
      DO J=Jbeg,Jend
        DO II=1,SubMainGridRatio
        DO JJ=1,SubMainGridRatio
          PHI(I,J,II,JJ)=PHI(Iend,J,SubMainGridRatio+1-II,JJ)
        ENDDO
        ENDDO
      ENDDO
      ENDDO







      DO I=Ibeg,Iend
      DO J=1,Nghost
        DO II=1,SubMainGridRatio
        DO JJ=1,SubMainGridRatio
          PHI(I,J,II,JJ)=PHI(I,Jbeg,II,SubMainGridRatio+1-JJ)
        ENDDO
        ENDDO
      ENDDO
      ENDDO







      DO I=Ibeg,Iend
      DO J=Jend+1,Nloc
        DO II=1,SubMainGridRatio
        DO JJ=1,SubMainGridRatio
          PHI(I,J,II,JJ)=PHI(I,Jend,II,SubMainGridRatio+1-JJ)
        ENDDO
        ENDDO
      ENDDO
      ENDDO




END SUBROUTINE UPDATE_BND_GHOST




