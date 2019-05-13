









SUBROUTINE PHI_EXCH_SUBGRID (PHI)
  USE PARAM
  USE GLOBAL
  IMPLICIT NONE
  REAL(SP),INTENT(INOUT) :: PHI(MMloc,NNloc,SubMainGridRatio,SubMainGridRatio)
  ! THIS SUBROUTINE IS NOT TESTED!!!!

  ! for serial run, need exch when bed are updated
  ! shift, not mirror!

  ! west
  DO J = Jbeg,Jend
  DO I = 1, Nghost
   DO JJ = 1, SubMainGridRatio
   DO II = 1, SubMainGridRatio
    PHI(I,J,II,JJ) = PHI(Ibeg+I-1,J,II,JJ)
   ENDDO
   ENDDO
  ENDDO
  ENDDO

  ! east
  DO J = Jbeg,Jend
  DO I = Iend+1,Mloc
   DO JJ = 1, SubMainGridRatio
   DO II = 1, SubMainGridRatio
    PHI(I,J,II,JJ) = PHI(I-Nghost,J,II,JJ)
   ENDDO
   ENDDO
  ENDDO
  ENDDO

  ! south
  DO J = 1, Nghost
  DO I = Ibeg, Iend
   DO JJ = 1, SubMainGridRatio
   DO II = 1, SubMainGridRatio
    PHI(I,J,II,JJ) = PHI(I,Jbeg+J-1,II,JJ)
   ENDDO
   ENDDO
  ENDDO
  ENDDO

  ! north
  DO J = Jend+1, Nloc
  DO I = Ibeg, Iend
   DO JJ = 1, SubMainGridRatio
   DO II = 1, SubMainGridRatio
    PHI(I,J,II,JJ) = PHI(I,J-Nghost,II,JJ)
   ENDDO
   ENDDO
  ENDDO
  ENDDO
END SUBROUTINE PHI_EXCH_SUBGRID















