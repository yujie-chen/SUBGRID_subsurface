!------------------------------------
! Subsurface module
!------------------------------------



MODULE SUBSURFACE_MODULE
  USE PARAM
  USE GLOBAL,ONLY : Mloc,Nloc,Mloc1,Nloc1,Nghost,  &
                    Mglob,Nglob, &
                    Ibeg,Iend,Jbeg,Jend,Iend1,Jend1,  &
                    DX,DY, DX2,DY2,DXDY, &
                    H,H0,H_u,H_v,ETA,U,V,P,Q,PQ,  &
                    MASK,MASKu,MASKv,MaxDepth, &
                    MinDepth,MASK,DT,tmp4preview, &
                    SubMainGridRatio,&     !Added by YUJIE CHEN
                    dt_over_dx,dt_over_dy,TIME
                 
  USE INPUT_Util




  IMPLICIT NONE
  SAVE

  CHARACTER(LEN=80) IMPER_TYPE,IMPER_FILE    !Added by YUJIE CHEN

  LOGICAL :: SATUATION =.TRUE.
  LOGICAL :: OUT_ETA_GROUND = .FALSE.
  LOGICAL :: OUT_P_GROUND = .FALSE.
  LOGICAL :: OUT_Q_GROUND = .FALSE.
  LOGICAL :: OUT_MASK_GROUND = .FALSE.
  LOGICAL :: OUT_MASKu_GROUND = .FALSE.
  LOGICAL :: OUT_MASKv_GROUND = .FALSE.
!Added by YUJIE CHEN
  LOGICAL :: OUT_D_GROUND = .FALSE.
  LOGICAL :: OUT_Du_GROUND = .FALSE.
  LOGICAL :: OUT_Dv_GROUND = .FALSE.
  LOGICAL :: OUT_Su_GROUND = .FALSE.
  LOGICAL :: OUT_Sv_GROUND = .FALSE.

  REAL(SP):: PLOT_INTV_ground,PLOT_COUNT_ground
  REAL(SP),DIMENSION(:,:),ALLOCATABLE::POROSITY_ground,ETA_ground,P_ground,Q_ground, &
                         Du_ground,Su_ground,Dv_ground,Sv_ground,  &
                         D_ground,  &  !Added by YUJIE CHEN
                         KAu_ground,KAv_ground,DepthX,DepthY, &
                         ETAu_ground,ETAv_ground,POROgrn
  REAL(SP) :: S_constant,KA_constant,POR_constant

  REAL(SP),DIMENSION(:,:,:,:),ALLOCATABLE:: ImpSubGrid      !Added by YUJIE CHEN
  INTEGER,DIMENSION(:,:),ALLOCATABLE :: MASK_GROUND, MASKu_GROUND, MASKv_ground





CONTAINS

! ------------------------------  
SUBROUTINE SUBSURFACE_INITIAL
  USE GLOBAL,ONLY : itmp1,itmp2,itmp3,itmp4,itmp5
                    
  USE Input_Util
  IMPLICIT NONE

  CHARACTER(LEN=80) FILE_NAME
  INTEGER::LINE
  INTEGER :: ierr
  ALLOCATE(ImpSubGrid(Mloc,Nloc,SubMainGridRatio,SubMainGridRatio))
! read parameters from input.txt
      FILE_NAME='input.txt'

      CALL GET_LOGICAL_VAL(SATUATION,FILE_NAME,'SATUATION',line,ierr)

      IF(ierr==1)THEN
        SATUATION = .TRUE.






         WRITE(*,'(A80)')'No SATUATION defined, USE defalt: TRUE'
         WRITE(3,'(A80)')'No SATUATION defined, USE defalt: TRUE'

       ENDIF

      CALL GET_LOGICAL_VAL(OUT_P_GROUND,FILE_NAME,'P_GROUND',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_Q_GROUND,FILE_NAME,'Q_GROUND',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_ETA_GROUND,FILE_NAME,'ETA_GROUND',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_MASK_GROUND,FILE_NAME,'MASK_GROUND',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_MASKu_GROUND,FILE_NAME,'MASKu_GROUND',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_MASKv_GROUND,FILE_NAME,'MASKv_GROUND',line,ierr)

! ADDED BY YUJIE CHEN 03-04-2019

      CALL GET_LOGICAL_VAL(OUT_D_GROUND,FILE_NAME,'D_GROUND',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_Du_GROUND,FILE_NAME,'Du_GROUND',line,ierr)
      CALL GET_LOGICAL_VAL(OUT_Dv_GROUND,FILE_NAME,'Dv_GROUND',line,ierr)

      CALL GET_Float_VAL(KA_constant,FILE_NAME,'Kappa',line,ierr)
      IF(ierr==1)THEN
        KA_constant = 0.01






         WRITE(*,'(A80)')'Kappa: Use default,', '0.01'
         WRITE(3,'(A80)')'Kappa: Use default,', '0.01'

      ENDIF

! constant porosity
! YUJIE CHEN, change POROSITY_GROUND to POROSITY_SOIL
      CALL GET_Float_VAL(POR_constant,FILE_NAME,'POROSITY_SOIL',line,ierr)
      IF(ierr==1)THEN
        POR_constant = 0.3






         WRITE(*,'(A80)')'POROgrn: Use default,', '0.3'
         WRITE(3,'(A80)')'POROgrn: Use default,', '0.3'

      ENDIF

!!!!!!!!!!!Added by YUJIECHEN!!!!!!
      CALL GET_STRING_VAL(IMPER_TYPE,FILE_NAME,'IMPER_TYPE',line,ierr)
!     IMPER_TYPE: UNIFORM/VARY/1
      IF(ierr==1)THEN
            IMPER_TYPE = 'UNIFORM'






            WRITE(*,'(A40,A40)')'IMPER_TYPE: Use default,', 'UNIFORM'
            WRITE(3,'(A40,A40)')'IMPER_TYPE: Use default,', 'UNIFORM'

      ENDIF

    IF(IMPER_TYPE(1:4)=='UNIF')THEN
      CALL GET_Float_VAL(S_constant,FILE_NAME,'HardBottom',line,ierr)
      IF(ierr==1)THEN
        S_constant = MaxDepth






         WRITE(*,'(A80)')'S_constant: Use default,', 'MaxDepth'
         WRITE(3,'(A80)')'S_constant: Use default,', 'MaxDepth'

      ENDIF
         ImpSubGrid=S_constant
    ENDIF

    IF(IMPER_TYPE(1:4)=='SUBG')THEN
        CALL GET_STRING_VAL(IMPER_FILE,FILE_NAME,'IMPER_FILE',line,ierr)
        IF(ierr==1)THEN







    WRITE(*,'(A40,A40)')'IMPER_FILE:', 'NOT FOUND, STOP'
    WRITE(3,'(A40,A40)')'IMPER_FILE:', 'NOT FOUND, STOP'

    STOP
    ELSE



    WRITE(3,'(A12,A50)')'IMPER_FILE:', IMPER_FILE

    ENDIF
        CALL GetFile_Subgrid (IMPER_FILE,ImpSubGrid)
    ENDIF ! subgrid



! ALLOCATION
        ALLOCATE(ETA_ground(Mloc,Nloc),P_ground(Mloc1,Nloc),Q_ground(Mloc,Nloc1), &
                 D_ground(Mloc,Nloc), POROSITY_GROUND(Mloc,Nloc), &   ! Added by YUJIE CHEN 03-04-2019
                 Du_ground(Mloc1,Nloc),Dv_ground(Mloc,Nloc1), &
                 Su_ground(Mloc1,Nloc),Sv_ground(Mloc,Nloc1), &
                 KAu_ground(Mloc1,Nloc),KAv_ground(Mloc,Nloc1), &
                 MASK_ground(Mloc,Nloc),MASKu_ground(Mloc1,Nloc), &
                 POROgrn(Mloc,Nloc), &
                 MASKv_ground(Mloc,Nloc1), &
                 DepthX(Mloc1,Nloc),DepthY(Mloc,Nloc1), &
                 ETAu_ground(Mloc1,Nloc),ETAv_ground(Mloc,Nloc1))

! assign variables

    P_ground = ZERO
    Q_ground = ZERO
    Du_ground = ZERO
    Dv_ground = ZERO
    KAu_ground = KA_constant 
    KAv_ground = KA_constant
    POROgrn = POR_constant

!    ETA_ground = ZERO
!    ETAu_ground = ZERO
!    ETAv_ground = ZERO

! Added by YUJIE CHEN 03-04-2019

    ETA_ground = ETA
    D_ground = ZERO
    MASKu_ground = ZERO
    MASKv_ground = ZERO
    MASK_ground = ZERO
    POROSITY_GROUND = 0.0_SP

    IF(IMPER_TYPE(1:4)=='UNIF')THEN
        Su_ground = S_constant
        Sv_ground = S_constant
    ENDIF

    IF(IMPER_TYPE(1:4)=='SUBG')THEN
        CALL SETUP_S_UV
    ENDIF

  !  CALL UPDATE_GROUND_MASK_ETA       ! ORIGINAL

END SUBROUTINE SUBSURFACE_INITIAL

!------------------------

SUBROUTINE UPDATE_GROUND_MASK_ETA

   INTEGER::tmpd
! depthx and depthy
! update with time
     DepthX = H_u
     DepthY = H_v

! eta at u v points
   DO J=1,Nloc
   DO I=Ibeg,Iend1
       ETAu_ground(I,J)=0.5_SP*(ETA_ground(I-1,J)+ETA_ground(I,J))
   ENDDO
   ENDDO

   DO J=Jbeg,Jend1
   DO I=1,Mloc
       ETAv_ground(I,J)=0.5_SP*(ETA_ground(I,J-1)+ETA_ground(I,J))
   ENDDO
   ENDDO






! thickness

!    DO J=1,Nloc
!    DO I=1,Mloc1
!      tmpd = ZERO
!      IF(MASKu(I,J) == ZERO)THEN
!        tmpd = Su_ground(I,J) +ETAu_ground(I,J)
!        IF(tmpd.GT.ZERO) THEN
!           Du_ground(I,J)=tmpd
!           MASKu_GROUND(I,J)=1
!        ELSE
!           Du_ground(I,J)=ZERO
!           MASKu_GROUND(I,J)=ZERO
!        ENDIF
!      ELSE
!        Du_ground(I,J) = Su_ground(I,J)-DepthX(I,J)
!        MASKu_GROUND(I,J)=1
!      ENDIF
!    ENDDO
!    ENDDO

!    DO J=1,Nloc1
!    DO I=1,Mloc
!      tmpd = ZERO
!      IF(MASKv(I,J) == ZERO)THEN
!        tmpd = Sv_ground(I,J) +ETAv_ground(I,J)
!        IF(tmpd.GT.ZERO)THEN
!           Dv_ground(I,J)=tmpd
!           MASKv_GROUND(I,J)=1
!        ELSE
!           Dv_ground(I,J)=ZERO
!           MASKv_GROUND(I,J)=ZERO
!        ENDIF
!      ELSE
!        Dv_ground(I,J) = Sv_ground(I,J)-DepthY(I,J)
!      ENDIF
!    ENDDO
!    ENDDO

END SUBROUTINE UPDATE_GROUND_MASK_ETA


SUBROUTINE UPDATE_SUBSURFACE
  USE GLOBAL,ONLY : itmp1,itmp2,itmp3,itmp4,itmp5,SMALL

  IMPLICIT NONE
  INTEGER::ISTEP,ISTAGE,IVAR
  REAL(SP):: Pgrad

     CALL UPDATE_GROUND_MASK_ETA

     DO J=Jbeg,Jend
     DO I=Ibeg,Iend1
!!!!Modifeid by YUJIE CHEN
        IF(MASK_GROUND(I,J)==1)THEN
            pgrad=ETA(I,J)-ETA(I-1,J)
            IF(MASKu_GROUND(I,J)==1)THEN
              P_ground(I,J) = - KAu_ground(I,J)*Du_ground(I,J)*Pgrad/DX
!                   *(ETA_ground(I,J)-ETA_ground(I-1,J))/DX
            ENDIF
        ELSE
            P_ground(I,J) = ZERO
        ENDIF
     ENDDO
     ENDDO

     DO J=Jbeg,Jend1
     DO I=Ibeg,Iend
        IF(MASK_GROUND(I,J)==1)THEN
            pgrad=ETA(I,J)-ETA(I,J-1)
            IF(MASKv_GROUND(I,J)==1)THEN
              Q_ground(I,J) = - KAv_ground(I,J)*Dv_ground(I,J)*Pgrad/DY
!                    *(ETA_ground(I,J)-ETA_ground(I,J-1))/DY
            ENDIF
        ELSE
            Q_ground(I,J) = ZERO
        ENDIF
     ENDDO
     ENDDO







     DO J=Jbeg,Jend
     DO I=Ibeg,Iend
!       IF(MASK(I,J)==0)THEN
!          IF(MASK_GROUND(I,J)==1)THEN       ! Added By YUJIE CHEN   03-12-2019
!              ETA_ground(I,J) = ETA_ground(I,J)  &
!                  -1.0_SP/POROgrn(I,J)*((P_ground(I+1,J)-P_ground(I,J))*dt_over_dx  &
!                   +(Q_ground(I,J+1)-Q_ground(I,J))*dt_over_dy)
!          ELSE                              ! Added By YUJIE CHEN  03-12-2019
!              ETA_ground(I,J) = 0
!          ENDIF
!       ELSE
!         ETA_ground(I,J) = ETA(I,J)
!       ENDIF

!       IF(MASK(I,J)==0 .AND. MASK_GROUND(I,J)==1)THEN
        IF(MASK(I,J)==0)THEN
              ETA(I,J) = ETA(I,J)  &
                  -1.0_SP/POROgrn(I,J)*((P_ground(I+1,J)-P_ground(I,J))*dt_over_dx  &
                   +(Q_ground(I,J+1)-Q_ground(I,J))*dt_over_dy)
       ENDIF

     ENDDO
     ENDDO






    CALL FILLIN_GRN_GHOST

END SUBROUTINE UPDATE_SUBSURFACE


SUBROUTINE FILLIN_GRN_GHOST

!	west




		DO J=1,Nloc
		DO I=1,Nghost
!            ETA_ground(I,J) = ETA_ground(Ibeg,J)
            ETA(I,J) = ETA(Ibeg,J)
			P_ground(I,J)=P_ground(Ibeg,J)
			Du_ground(I,J)=Du_ground(Ibeg,J)
		ENDDO
		ENDDO

		DO J=Jbeg,Jend1
		DO I=1,Nghost
			Q_ground(I,J)=Q_ground(Ibeg,J)
			Dv_ground(I,J)=Dv_ground(Ibeg,J)
		ENDDO
		ENDDO





!	east




		DO J=1,Nloc
		DO I=Iend1+1,Mloc1
!            ETA_ground(I-1,J) = ETA_ground(Iend,J)
            ETA(I-1,J) = ETA(Iend,J)
			P_ground(I,J)=P_ground(Iend1,J)
			Du_ground(I,J)=Du_ground(Iend1,J)
		ENDDO
		ENDDO

		DO J=Jbeg,Jend1
		DO I=Iend1,Mloc
			Q_ground(I,J)=Q_ground(Iend,J)
			Dv_ground(I,J)=Dv_ground(Iend,J)
		ENDDO
		ENDDO





!	south




		DO J=1,Nghost
		DO I=1,Mloc
!            ETA_ground(I,J) = ETA_ground(I,Jbeg)
            ETA(I,J) = ETA(I,Jbeg)
			Q_ground(I,J) = Q_ground(I,Jbeg)
			Dv_ground(I,J) = Dv_ground(I,Jbeg)
		ENDDO
		ENDDO

		DO J=1,Nghost
		DO I=Ibeg,Iend1
			P_ground(I,J) = P_ground(I,Jbeg)
			Du_ground(I,J) = Du_ground(I,Jbeg)
		ENDDO
		ENDDO





!	north




		DO J=Jend1+1,Nloc1
		DO I=1,Mloc
!            ETA_ground(I,J-1) = ETA_ground(I,Jend)
            ETA(I,J-1) = ETA(I,Jend)
			Q_ground(I,J)=Q_ground(I,Jend1)
			Dv_ground(I,J)=Dv_ground(I,Jend1)
		ENDDO
		ENDDO

		DO J=Jend1,Nloc
		DO I=Ibeg,Iend1
			P_ground(I,J)=P_ground(I,Jend)
			Du_ground(I,J)=Du_ground(I,Jend)
		ENDDO
		ENDDO





END SUBROUTINE FILLIN_GRN_GHOST


!Added by YUJIE CHEN
!WHEN hardbottom is varied
SUBROUTINE SETUP_S_UV
   INTEGER::tmpv1,tmpv2
! get Su_ground, Sv_ground

! west and east
    DO J=Jbeg,Jend
    DO I=Ibeg,Iend1

        tmpv1=ZERO
        tmpv2=ZERO

        DO JJ=1,SubMainGridRatio
!         west side
        tmpv1=tmpv1+ImpSubGrid(I-1,J,SubMainGridRatio,JJ)
!         east side
        tmpv2=tmpv2+ImpSubGrid(I,J,1,JJ)
        ENDDO  ! end submaingridratio
        Su_ground(I,J)=0.5*(tmpv1/real(SubMainGridRatio)+tmpv2/real(SubMainGridRatio))

    ENDDO
    ENDDO

! south and north
    DO J=Jbeg,Jend1
    DO I=Ibeg,Iend

        tmpv1=ZERO
        tmpv2=ZERO

    DO II=1,SubMainGridRatio
!         south side
        tmpv1=tmpv1+ImpSubGrid(I,J-1,II,SubMainGridRatio)
!         north side
        tmpv2=tmpv2+ImpSubGrid(I,J,II,1)
    ENDDO  ! end submaingridratio
      Sv_ground(I,J)=0.5*(tmpv1/real(SubMainGridRatio)+tmpv2/real(SubMainGridRatio))
    ENDDO
    ENDDO






END SUBROUTINE SETUP_S_UV


!Added by YUJIE CHEN 03-04-2019
SUBROUTINE UPDATE_SUBGRID_SUBSURFACE

     USE GLOBAL
     IMPLICIT NONE
     REAL(SP) :: AvgEta,TmpEta,tmpv1,tmpv2,tmpv3,tmpv4,&
                            tmpvg1,tmpvg2,tmpvg3,tmpvg4
     INTEGER  :: Ktmp,tmpk1,tmpk2,pcount,pcount1,pcount2,&
                   tmpkg1,tmpkg2,pcountg,pcountg1,pcountg2

    !     H0 = H
     DO J=1,Nloc
     DO I=1,Mloc
        tmpv2=ZERO
        pcount=ZERO
        Porosity(I,J)=ZERO
        tmpvg2=ZERO
        pcountg=ZERO
        POROSITY_GROUND(I,J)=ZERO

        DO JJ=1,SubMainGridRatio
        DO II=1,SubMainGridRatio

          tmpv1=Eta(I,J)+min(DepSubGrid(I,J,II,JJ),ImpSubGrid(I,J,II,JJ))
          tmpvg1=Eta(I,J)+ImpSubGrid(I,J,II,JJ)

           IF(tmpv1 .GT.ZERO)THEN
              tmpv2=tmpv2+tmpv1
              pcount=pcount+1
           ENDIF
!$$$
           IF(tmpvg1 .GT. ZERO .AND. ImpSubGrid(I,J,II,JJ).GT.DepSubGrid(I,J,II,JJ))THEN
              IF(tmpv1 .GT. ZERO) THEN
                 tmpvg1=ImpSubGrid(I,J,II,JJ)-DepSubGrid(I,J,II,JJ)
              ENDIF
              tmpvg2=tmpvg2+tmpvg1
              pcountg=pcountg+1
           ENDIF
        ENDDO
        ENDDO

        IF(I==200 .AND. J==100)THEN
           print*,'ETA=',Eta(I,J)
           print*,'tmpv2=',tmpv2
           print*,'tmpv1=',tmpv1
!           print*,'ETAgrn=',Eta_ground(I,J)
           print*,'tmpvg2=',tmpvg2
           print*,'tmpvg1=',tmpvg1
           print*,'ImpSubGrid:',ImpSubGrid(I,J,:,:)
           print*,'DepSubGrid:',DepSubGrid(I,J,:,:)
        ENDIF

        IF(pcount == ZERO)THEN
           H(I,J) = ZERO
           MASK(I,J)= ZERO
        ELSE
           H(I,J)=tmpv2/REAL(NumPixel)
           Porosity(I,J)=REAL(pcount)/REAL(NumPixel)
           MASK(I,J)=1
        ENDIF

        IF(pcountg == ZERO)THEN
           D_ground(I,J) = ZERO
           MASK_GROUND(I,J) = ZERO
        ELSE
           D_ground(I,J) = tmpvg2/REAL(NumPixel)
           POROSITY_GROUND(I,J) = REAL(pcountg)/REAL(NumPixel)
           MASK_GROUND(I,J) = 1
        ENDIF

    ENDDO
    ENDDO

! because mask is used when calculate h_u h_v 2016-01-17





! get H_u, H_v
! get Du_ground,Dv_ground
! percentage of area of sections respect to depthx,depthy
! west and east
    DO J=Jbeg,Jend
    DO I=Ibeg,Iend1

        tmpv2=ZERO
        tmpv4=ZERO
        pcount1=ZERO
        pcount2=ZERO

        tmpvg2=ZERO
        tmpvg4=ZERO
        pcountg1=ZERO
        pcountg2=ZERO


!!!!!Need to be modified by taking Eta_ground, Mask_ground into consideration
    AvgEta=0.5_SP*(Eta(MAX(1,I-1),J)+Eta(I,J))
    IF(MASK(I-1,J)==0.AND.MASK(I,J)==1)AvgEta=Eta(I,J)
    IF(MASK(I,J)==0.AND.MASK(I-1,J)==1)AvgEta=Eta(I-1,J)
    IF(MASK(I-1,J)==0.AND.MASK(I,J)==0)AvgEta=MIN(Eta(I-1,J),Eta(I,J))

!   AvgEta=0.5_SP*(Eta_ground(MAX(1,I-1),J)+Eta_ground(I,J))
!   IF(MASK(I-1,J)==0.AND.MASK(I,J)==1)AvgEta=Eta(I,J)
!   IF(MASK(I,J)==0.AND.MASK(I-1,J)==1)AvgEta=Eta(I-1,J)
!   IF(MASK(I-1,J)==0.AND.MASK(I,J)==0)AvgEta=MIN(Eta_ground(I-1,J),Eta_ground(I,J))

    DO JJ=1,SubMainGridRatio
!         west side
     tmpv1=AvgEta+min(DepSubGrid(I-1,J,SubMainGridRatio,JJ),&
               ImpSubGrid(I-1,J,SubMainGridRatio,JJ))
        IF (tmpv1 .GT.ZERO) THEN
            tmpv2=tmpv2+tmpv1
            pcount1=pcount1+1
        ENDIF

    tmpvg1=AvgEta+ImpSubGrid(I-1,J,SubMainGridRatio,JJ)
       IF(tmpvg1 .GT.ZERO .AND. ImpSubGrid(I-1,J,SubMainGridRatio,JJ).GT. &
                  DepSubGrid(I-1,J,SubMainGridRatio,JJ)) THEN
          IF(tmpv1.GT.ZERO) THEN
               tmpvg1= ImpSubGrid(I-1,J,SubMainGridRatio,JJ) &
                  -DepSubGrid(I-1,J,SubMainGridRatio,JJ)
          ENDIF
          tmpvg2 = tmpvg2+tmpvg1
          pcountg1 = pcountg1 +1
       ENDIF

!         east side
    tmpv3 = AvgEta+min(DepSubGrid(I,J,1,JJ),ImpSubGrid(I,J,1,JJ))
        IF (tmpv3 .GT.ZERO) THEN
            tmpv4=tmpv4+tmpv3
            pcount2=pcount2+1
        ENDIF

   tmpvg3 = AvgEta+ImpSubGrid(I,J,1,JJ)
       IF(tmpvg3 .GT.ZERO .AND. ImpSubGrid(I,J,1,JJ).GT.DepSubGrid(I,J,1,JJ)) THEN
         IF(tmpv3.GT.ZERO) THEN
             tmpvg3=ImpSubGrid(I,J,1,JJ)-DepSubGrid(I,J,1,JJ)
         ENDIF
        tmpvg4=tmpvg4+tmpvg3
        pcountg2=pcountg2+1
    ENDIF

    ENDDO  ! end submaingridratio

    IF(pcount1 .GT.ZERO.AND.pcount2 .GT.ZERO)THEN
        H_u(I,J)=0.5*(tmpv2/real(SubMainGridRatio)+tmpv4/real(SubMainGridRatio))
    ELSE
        H_u(I,J)=ZERO
    ENDIF

    IF(pcountg1 .GT.ZERO.AND.pcountg2 .GT.ZERO)THEN
        Du_ground(I,J)=0.5*(tmpvg2/real(SubMainGridRatio)+tmpvg4/real(SubMainGridRatio))
    ELSE
        Du_ground(I,J)=ZERO
    ENDIF

    ENDDO
    ENDDO

! south and north
    DO J=Jbeg,Jend1
    DO I=Ibeg,Iend

        tmpv2=ZERO
        tmpv4=ZERO
        pcount1=0
        pcount2=0

        tmpvg2=ZERO
        tmpvg4=ZERO
        pcountg1=0
        pcountg2=0

    AvgEta=0.5_SP*(Eta(I,MAX(1,J-1))+Eta(I,J))
    IF(MASK(I,J-1)==0.AND.MASK(I,J)==1)AvgEta=Eta(I,J)
    IF(MASK(I,J)==0.AND.MASK(I,J-1)==1)AvgEta=Eta(I,J-1)
    IF(MASK(I,J-1)==0.AND.MASK(I,J)==0)AvgEta=MIN(Eta(I,MAX(1,J-1)),Eta(I,J))

!    AvgEta=0.5_SP*(Eta_ground(I,MAX(1,J-1))+Eta_ground(I,J))
!    IF(MASK(I,J-1)==0.AND.MASK(I,J)==1)AvgEta=Eta(I,J)
!    IF(MASK(I,J)==0.AND.MASK(I,J-1)==1)AvgEta=Eta(I,J-1)
!    IF(MASK(I,J-1)==0.AND.MASK(I,J)==0)AvgEta=MIN(Eta_ground(I,MAX(1,J-1)),Eta_ground(I,J))


    DO II=1,SubMainGridRatio
!         south side
        tmpv1=avgeta+min(DepSubGrid(I,J-1,II,SubMainGridRatio),ImpSubGrid(I,J-1,II,SubMainGridRatio))
        IF (tmpv1 .GT.ZERO) THEN
            tmpv2=tmpv2+tmpv1
            pcount1=pcount1+1
        ENDIF

       tmpvg1 = AvgEta+ ImpSubGrid(I,J-1,II,SubMainGridRatio)
       IF(tmpvg1 .GT.ZERO .AND. ImpSubGrid(I,J-1,II,SubMainGridRatio) &
                         .GT. DepSubGrid(I,J-1,II,SubMainGridRatio))THEN
         IF(tmpv1 .GT.ZERO)THEN
                 tmpvg1= ImpSubGrid(I,J-1,II,SubMainGridRatio) - &
                            DepSubGrid(I,J-1,II,SubMainGridRatio)
         ENDIF
            tmpvg2 = tmpvg2 + tmpvg1
            pcountg1 = pcountg1 + 1
       ENDIF

!         north side
        tmpv3=avgeta+min(DepSubGrid(I,J,II,1),ImpSubGrid(I,J,II,1))
        IF (tmpv3 .GT.ZERO) THEN
            tmpv4=tmpv4+tmpv3
            pcount2=pcount2+1
        ENDIF

       tmpvg3=avgeta+ImpSubGrid(I,J,II,1)
       IF(tmpvg3 .GT.ZERO .AND. ImpSubGrid(I,J,II,1).GT.DepSubGrid(I,J,II,1))THEN
         IF(tmpv3 .GT.ZERO) THEN
            tmpvg3=ImpSubGrid(I,J,II,1)-DepSubGrid(I,J,II,1)
         ENDIF
       tmpvg4=tmpvg4+tmpvg3
       pcountg2=pcountg2+1
       ENDIF

    ENDDO  ! end submaingridratio

    IF(pcount1 .GT.ZERO.AND.pcount2 .GT.ZERO)THEN
        H_v(I,J)=0.5*(tmpv2/real(SubMainGridRatio)+tmpv4/real(SubMainGridRatio))
    ELSE
        H_v(I,J)=ZERO
    ENDIF

    IF(pcountg1 .GT.ZERO.AND.pcountg2 .GT.ZERO)THEN
        Dv_ground(I,J)=0.5*(tmpvg2/real(SubMainGridRatio)+tmpvg4/real(SubMainGridRatio))
    ELSE
        Dv_ground(I,J)=ZERO
    ENDIF

    ENDDO
    ENDDO












END SUBROUTINE UPDATE_SUBGRID_SUBSURFACE

END MODULE SUBSURFACE_MODULE

! end the module


