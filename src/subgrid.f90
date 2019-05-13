

SUBROUTINE UPDATE_SUBGRID
     USE GLOBAL

     IMPLICIT NONE
     REAL(SP) :: AvgEta,TmpEta,tmpv1,tmpv2,tmpv3,tmpv4
     INTEGER  :: Ktmp,tmpk1,tmpk2,pcount,pcount1,pcount2

! outside if: interpolation
! here I turn off the pre-storage of porosity -gxwu


! inside if: polynomial

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





! because mask is used when calculate h_u h_v 2016-01-17




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









END SUBROUTINE UPDATE_SUBGRID


SUBROUTINE SETUP_INITIAL_ETA_SUBGRID
     USE GLOBAL

     USE SUBSURFACE_MODULE

     IMPLICIT NONE
     REAL(SP) :: tmpv1,tmpv2,depmax,avgeta !,hwater
     INTEGER :: pcount
!!! get depth in coarse grid, depth_x and depth_y (where is used?)

     DO J=1,Nloc
     DO I=1,Mloc
       tmpv2=ZERO
       pcount=0
 !     Porosity(I,J)=ZERO
       depmax=-LARGE
       DO JJ=1,SubMainGridRatio
       DO II=1,SubMainGridRatio
         ! tmpv1=Eta(I,J)+DepSubGrid(I,J,II,JJ)
         ! IF(DepSubGrid(I,J,II,JJ).GT.depmax) &
         !   depmax=DepSubGrid(I,J,II,JJ)
          IF(min(DepSubGrid(I,J,II,JJ),ImpSubGrid(I,J,II,JJ)).GT.depmax)  &
            depmax=min(DepSubGrid(I,J,II,JJ),ImpSubGrid(I,J,II,JJ))
             tmpv1=depmax
           ! tmpv1=Eta(I,J)+depmax
          IF(tmpv1.GT.-LARGE)THEN
            tmpv2=tmpv2+tmpv1
            pcount=pcount+1
          ENDIF
       ENDDO
       ENDDO
       IF(pcount==0)THEN
!# if defined (1)
!          ETA(I,J) = -depmax
!# else
!          ETA(I,J)=-depmax-MinDepth
!          ETA(I,J) = -depmax
!# endif
           EtaScreen(I,J) = -LARGE
 !         MASK(I,J) = 0
!Added by YUJIE CHEN  03-04-2019
       ELSE
          EtaScreen(I,J) = tmpv2/REAL(NumPixel)   !$$$
!          MASK(I,J) = 1
       ENDIF

     ENDDO
     ENDDO






     WRITE(3,*)'setup initial subgrid eta completed'


END SUBROUTINE SETUP_INITIAL_ETA_SUBGRID


