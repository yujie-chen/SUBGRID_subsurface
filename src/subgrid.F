# if defined (SUBGRID)

SUBROUTINE UPDATE_SUBGRID
     USE GLOBAL
# if defined (SUBSURFACE)
     USE SUBSURFACE_MODULE
# endif
     IMPLICIT NONE
     REAL(SP) :: AvgEta,TmpEta,tmpv1,tmpv2,tmpv3,tmpv4
     INTEGER  :: Ktmp,tmpk1,tmpk2,pcount,pcount1,pcount2

! outside if: interpolation
! here I turn off the pre-storage of porosity -gxwu
# if defined (INTERPOLATION_POROSITY)
     DO J=1,Nloc
     DO I=1,Mloc
        TmpEta = Eta(I,J)
        TmpEta = MIN(TmpEta,EtaMaxVal)
        TmpEta = MAX(TmpEta,EtaMinVal)
        tmpk1 = NINT((TmpEta-EtaMinVal)/D_Eta) + 1
        tmpk2 = MIN(tmpk1+1,Neta)

        IF(PorosityTab(I,J,tmpk2)<=ZERO)THEN
          Porosity(I,J)=ZERO
        ELSEIF(PorosityTab(I,J,tmpk1)>=1.0_SP)THEN
          Porosity(I,J)=1.0_SP
        ELSE
          ! linear interpolation
          Porosity(I,J)=PorosityTab(I,J,tmpk1)+(TmpEta-EtaTab(tmpk1)) &
                       *(PorosityTab(I,J,tmpk2)-PorosityTab(I,J,tmpk1))/D_Eta
        ENDIF
        ! porosity should be integer times of 1/NumPixel
        Porosity(I,J)=INT(Porosity(I,J)*NumPixel + 0.5_SP)/real(NumPixel)

        Porosity(I,J)=MAX(Porosity(I,J),ZERO)
        Porosity(I,J)=MIN(Porosity(I,J),1.0_SP)
        IF(Porosity(I,J) > ZERO)THEN
           MASK(I,J)=1
        ELSE
           MASK(I,J)=0
        ENDIF
     ENDDO
     ENDDO

# else

! inside if: polynomial
# if defined(POLY_POROSITY)
     Porosity=1.0_SP

     DO J=1,Nloc
     DO I=1,Mloc
        IF(GrdType(I,J)==0)THEN
          Porosity(I,J)=ZERO
        ELSEIF(GrdType(I,J)==1)THEN
          Porosity(I,J)=1.0_SP
        ELSEIF(GrdType(I,J)==2)THEN
          IF( Eta(I,J) <= EtaTab(Kbeg_por(I,J)) )THEN
            Porosity(I,J)=ZERO
          ELSE
            Porosity(I,J)=1.0_SP
          ENDIF
        ELSEIF(GrdType(I,J)==3)THEN
          ! linear interp if too few points between beg:end
          IF( Eta(I,J) <= EtaTab(Kbeg_por(I,J)) )THEN
            Porosity(I,J)=ZERO
          ELSEIF( Eta(I,J) > EtaTab(Kend_por(I,J)) )THEN
            Porosity(I,J)=1.0_SP
          ELSE
            Porosity(I,J)=( Eta(I,J) - EtaTab(Kbeg_por(I,J)) ) &
                       /( EtaTab(Kend_por(I,J)) - EtaTab(Kbeg_por(I,J)) )
          ENDIF
        ELSEIF(GrdType(I,J)==4)THEN
          ! polyfit
           IF( Eta(I,J) <= EtaTab(Kbeg_por(I,J)) )THEN
              Porosity(I,J)=ZERO
           ELSEIF( Eta(I,J) > EtaTab(Kend_por(I,J)) )THEN
              Porosity(I,J)=1.0_SP
           ELSE
              DO Ktmp=1,PolyOrder+1
                 Porosity(I,J)=Porosity(I,J) + &
                          Coef_Porosity(I,J,Ktmp)*(Eta(I,J)-AvgEta0)**(Ktmp-1)
              ENDDO
           ENDIF
        ELSE
           WRITE(*,*) 'GrdType = 9'
           STOP
        ENDIF

        ! porosity should be integer times of 1/NumPixel
        Porosity(I,J)=INT(Porosity(I,J)*NumPixel + 0.5_SP)/real(NumPixel)
        Porosity(I,J)=MAX(Porosity(I,J),ZERO)
        Porosity(I,J)=MIN(Porosity(I,J),1.0_SP)
        IF(Porosity(I,J) > ZERO)THEN
           MASK(I,J)=1
        ELSE
           MASK(I,J)=0
        ENDIF
     ENDDO
     ENDDO

# else
!    direct calculation
     H0 = H
     DO J=1,Nloc
     DO I=1,Mloc
       tmpv2=ZERO
       pcount=0
       Porosity(I,J)=ZERO
       DO JJ=1,SubMainGridRatio
       DO II=1,SubMainGridRatio
          tmpv1=Eta(I,J)+DepSubGrid(I,J,II,JJ)
          IF(tmpv1.GT.ZERO)THEN
            tmpv2=tmpv2+tmpv1
            pcount=pcount+1
          ENDIF
          ! for idealized test, maximum elevation during DT_ECO
          ! is used for Mean High Tide which affect mortality
# if defined(ECO_MORPHOLOGY)
          IF(tmpv1.GT.MeanHighTide4(I,J,II,JJ))&
            MeanHighTide4(I,J,II,JJ)=tmpv1
# endif
       ENDDO
       ENDDO
       IF(pcount==0)THEN
!      IF(pcount<SubMainGridRatio)THEN   ! at least one row/column get wet
         H(I,J)=ZERO
         MASK(I,J)=0
       ELSE
         H(I,J)=tmpv2/REAL(NumPixel)
         Porosity(I,J)=REAL(pcount)/REAL(NumPixel)
         MASK(I,J)=1
       ENDIF
     ENDDO
     ENDDO

# endif
# endif


! because mask is used when calculate h_u h_v 2016-01-17
# if defined(PARALLEL)
    CALL PHI_INT_EXCH_1(MASK)
# endif

! get H_u, H_v
! percentage of area of sections respect to depthx,depthy
! west and east
     DO J=Jbeg,Jend
     DO I=Ibeg,Iend1

       tmpv2=ZERO
       tmpv4=ZERO
       pcount1=0
       pcount2=0

       AvgEta=0.5_SP*(Eta(MAX(1,I-1),J)+Eta(I,J))
       IF(MASK(I-1,J)==0.AND.MASK(I,J)==1)AvgEta=Eta(I,J)
       IF(MASK(I,J)==0.AND.MASK(I-1,J)==1)AvgEta=Eta(I-1,J)
       IF(MASK(I-1,J)==0.AND.MASK(I,J)==0)AvgEta=MIN(Eta(I-1,J),Eta(I,J))

       DO JJ=1,SubMainGridRatio
!         west side
          tmpv1=AvgEta+DepSubGrid(I-1,J,SubMainGridRatio,JJ)
          IF (tmpv1.GT.ZERO) THEN
            tmpv2=tmpv2+tmpv1
            pcount1=pcount1+1
          ENDIF
!         east side
          tmpv3=AvgEta+DepSubGrid(I,J,1,JJ)
          IF (tmpv3.GT.ZERO) THEN
            tmpv4=tmpv4+tmpv3
            pcount2=pcount2+1
          ENDIF
       ENDDO  ! end submaingridratio

       IF(pcount1.GT.0.AND.pcount2.GT.0)THEN
!          H_u(I,J)=0.5*(tmpv2/pcount1+tmpv4/pcount2)
          H_u(I,J)=0.5*(tmpv2/real(SubMainGridRatio)+tmpv4/real(SubMainGridRatio))
       ELSE
          H_u(I,J)=ZERO
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

       AvgEta=0.5_SP*(Eta(I,MAX(1,J-1))+Eta(I,J))
       IF(MASK(I,J-1)==0.AND.MASK(I,J)==1)AvgEta=Eta(I,J)
       IF(MASK(I,J)==0.AND.MASK(I,J-1)==1)AvgEta=Eta(I,J-1)
       IF(MASK(I,J-1)==0.AND.MASK(I,J)==0)AvgEta=MIN(Eta(I,MAX(1,J-1)),Eta(I,J))

       DO II=1,SubMainGridRatio
!         south side
          tmpv1=avgeta+DepSubGrid(I,J-1,II,SubMainGridRatio)
          IF (tmpv1.GT.ZERO) THEN
            tmpv2=tmpv2+tmpv1
            pcount1=pcount1+1
          ENDIF
!         north side
          tmpv3=avgeta+DepSubGrid(I,J,II,1)
          IF (tmpv3.GT.ZERO) THEN
            tmpv4=tmpv4+tmpv3
            pcount2=pcount2+1
          ENDIF
       ENDDO  ! end submaingridratio

       IF(pcount1.GT.0.AND.pcount2.GT.0)THEN
          H_v(I,J)=0.5*(tmpv2/real(SubMainGridRatio)+tmpv4/real(SubMainGridRatio))
!          H_v(I,J)=0.5*(tmpv2/pcount1+tmpv4/pcount2)
       ELSE
          H_v(I,J)=ZERO
       ENDIF

     ENDDO
     ENDDO

# if defined (PARALLEL)
     CALL PHI_EXCH_2(H_u)
     CALL PHI_EXCH_3(H_v)
# endif


# if defined(DEBUG)
# if defined (PARALLEL)
    IF(myid == 0)THEN
     WRITE(3,*)'Subroutine update_subgrid'
    ENDIF
# else
     WRITE(3,*)'Subroutine update_subgrid'
# endif
# endif

END SUBROUTINE UPDATE_SUBGRID

# endif


SUBROUTINE SETUP_INITIAL_ETA_SUBGRID
     USE GLOBAL
     IMPLICIT NONE
     REAL(SP) :: tmpv1,tmpv2,depmax,hwater,avgeta
     INTEGER :: pcount

! get depth in coarse grid, depth_x and depth_y

     DO J=1,Nloc
     DO I=1,Mloc
       tmpv2=ZERO
       pcount=0
       Porosity(I,J)=ZERO
       depmax=-LARGE
       DO JJ=1,SubMainGridRatio
       DO II=1,SubMainGridRatio
          tmpv1=Eta(I,J)+DepSubGrid(I,J,II,JJ)
          IF(DepSubGrid(I,J,II,JJ).GT.depmax)  &
            depmax=DepSubGrid(I,J,II,JJ)
          IF(tmpv1.GT.ZERO)THEN
            tmpv2=tmpv2+tmpv1
            pcount=pcount+1
          ENDIF
       ENDDO
       ENDDO
       IF(pcount==0)THEN
# if defined (SUBSURFACE)
          ETA(I,J) = -depmax
# else
!          ETA(I,J)=-depmax-MinDepth
          ETA(I,J) = -depmax
# endif
          EtaScreen(I,J) = -depmax
          MASK(I,J) = 0
       ENDIF

     ENDDO
     ENDDO

# if defined (PARALLEL)
    IF(myid == 0)THEN
     WRITE(3,*)'setup initial subgrid eta completed'
    ENDIF
# else
     WRITE(3,*)'setup initial subgrid eta completed'
# endif

END SUBROUTINE SETUP_INITIAL_ETA_SUBGRID

