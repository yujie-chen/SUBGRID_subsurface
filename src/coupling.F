# if defined (COUPLING)
SUBROUTINE InitOneWayCoupling
   USE GLOBAL
   IMPLICIT NONE
       OPEN(11,FILE=TRIM(COUPLING_FILE))
         READ(11,*)  ! title
         READ(11,*)  ! boundary info
! boundary basic info including point number of coupling, start point, etc
! east
         READ(11,*)  ! east
         READ(11,*) N_COUPLING_EAST,J_START_EAST
! west
         READ(11,*)  ! west
         READ(11,*) N_COUPLING_WEST,J_START_WEST
! south
         READ(11,*)  ! south
         READ(11,*) N_COUPLING_SOUTH,I_START_SOUTH
! north
         READ(11,*)  ! north
         READ(11,*) N_COUPLING_NORTH,I_START_NORTH

! read time and variable at the first level

         READ(11,*) ! time start title
         READ(11,*) TIME_COUPLING_1
! initialize time_2
         TIME_COUPLING_2 = TIME_COUPLING_1

! east
         IF(N_COUPLING_EAST.GT.0)THEN
           ALLOCATE(U_COUPLING_EAST(N_COUPLING_EAST,2),&
               V_COUPLING_EAST(N_COUPLING_EAST,2),&
               Z_COUPLING_EAST(N_COUPLING_EAST,2))
             READ(11,*)   ! east
             READ(11,119)(U_COUPLING_EAST(I,2),I=1,N_COUPLING_EAST)
             READ(11,119)(V_COUPLING_EAST(I,2),I=1,N_COUPLING_EAST)
             READ(11,119)(Z_COUPLING_EAST(I,2),I=1,N_COUPLING_EAST)
!   initialize first step
             U_COUPLING_EAST(:,1)=U_COUPLING_EAST(:,2)
             V_COUPLING_EAST(:,1)=V_COUPLING_EAST(:,2)
             Z_COUPLING_EAST(:,1)=Z_COUPLING_EAST(:,2)
         ELSE
             READ(11,*)

         ENDIF ! n_coupling_east
119      FORMAT(5E16.6)

! west
         IF(N_COUPLING_WEST.GT.0)THEN
           ALLOCATE(U_COUPLING_WEST(N_COUPLING_WEST,2),&
               V_COUPLING_WEST(N_COUPLING_WEST,2),&
               Z_COUPLING_WEST(N_COUPLING_WEST,2))
             READ(11,*)   ! west
             READ(11,119)(U_COUPLING_WEST(I,2),I=1,N_COUPLING_WEST)
             READ(11,119)(V_COUPLING_WEST(I,2),I=1,N_COUPLING_WEST)
             READ(11,119)(Z_COUPLING_WEST(I,2),I=1,N_COUPLING_WEST)
!   initialize first step
             U_COUPLING_WEST(:,1)=U_COUPLING_WEST(:,2)
             V_COUPLING_WEST(:,1)=V_COUPLING_WEST(:,2)
             Z_COUPLING_WEST(:,1)=Z_COUPLING_WEST(:,2)
         ELSE
             READ(11,*)

         ENDIF ! n_coupling_west
! south
         IF(N_COUPLING_SOUTH.GT.0)THEN
           ALLOCATE(U_COUPLING_SOUTH(N_COUPLING_SOUTH,2),&
               V_COUPLING_SOUTH(N_COUPLING_SOUTH,2),&
               Z_COUPLING_SOUTH(N_COUPLING_SOUTH,2))
             READ(11,*)   ! south
             READ(11,119)(U_COUPLING_SOUTH(I,2),I=1,N_COUPLING_SOUTH)
             READ(11,119)(V_COUPLING_SOUTH(I,2),I=1,N_COUPLING_SOUTH)
             READ(11,119)(Z_COUPLING_SOUTH(I,2),I=1,N_COUPLING_SOUTH)
!   initialize first step
             U_COUPLING_SOUTH(:,1)=U_COUPLING_SOUTH(:,2)
             V_COUPLING_SOUTH(:,1)=V_COUPLING_SOUTH(:,2)
             Z_COUPLING_SOUTH(:,1)=Z_COUPLING_SOUTH(:,2)
         ELSE
             READ(11,*)

         ENDIF ! n_coupling_south
! north
         IF(N_COUPLING_NORTH.GT.0)THEN
           ALLOCATE(U_COUPLING_NORTH(N_COUPLING_NORTH,2),&
               V_COUPLING_NORTH(N_COUPLING_NORTH,2),&
               Z_COUPLING_NORTH(N_COUPLING_NORTH,2))
             READ(11,*)   ! north
             READ(11,119)(U_COUPLING_NORTH(I,2),I=1,N_COUPLING_NORTH)
             READ(11,119)(V_COUPLING_NORTH(I,2),I=1,N_COUPLING_NORTH)
             READ(11,119)(Z_COUPLING_NORTH(I,2),I=1,N_COUPLING_NORTH)

!   initialize first step
             U_COUPLING_NORTH(:,1)=U_COUPLING_NORTH(:,2)
             V_COUPLING_NORTH(:,1)=V_COUPLING_NORTH(:,2)
             Z_COUPLING_NORTH(:,1)=Z_COUPLING_NORTH(:,2)
         ELSE
             READ(11,*)

         ENDIF ! n_coupling_north


! specify boundary start points

! west boundary
   IF(N_COUPLING_WEST>0)THEN
# if defined (PARALLEL)
    if ( n_west .eq. MPI_PROC_NULL ) then
      Kstart_WEST=J_START_WEST+Nghost -npy*Nglob/py
      Kend_WEST = J_START_WEST+Nghost+N_COUPLING_WEST-1 -npy*Nglob/py
      IF((Kstart_WEST>Nghost.AND.Kstart_WEST<Nloc-Nghost+1).OR.&
     (Kend_WEST<Nloc-Nghost+1.AND.Kend_WEST>Nghost))THEN
       IF(Kstart_WEST<Nghost+1)THEN
         Kshift_WEST= -Kstart_WEST+Nghost+1
         Kstart_WEST=Nghost+1
       ELSE
         Kshift_WEST=-(Kstart_WEST-Nghost)+1
       ENDIF
       IF(Kend_WEST>Nloc-Nghost)THEN
         Kend_WEST=Nloc-Nghost
       ENDIF
       IN_DOMAIN_WEST=.TRUE.
      ELSE
       IF(Kstart_WEST<=Nghost.AND.Kend_WEST>=Nloc-Nghost+1)THEN
         Kshift_WEST=-Kstart_WEST+Nghost+1
         Kstart_WEST = Nghost+1
         Kend_WEST = Nloc-Nghost
         IN_DOMAIN_WEST=.TRUE.
       ELSE
         IN_DOMAIN_WEST=.FALSE.
       ENDIF
      ENDIF
! check print*,myid,Kshift_WEST,Kstart_WEST,Kend_WEST
     endif
# else
      Kstart_WEST=J_START_WEST+Nghost
      Kend_WEST = J_START_WEST+Nghost+N_COUPLING_WEST-1
      Kshift_WEST = -(Kstart_WEST-Nghost)+1
      IN_DOMAIN_WEST = .TRUE.
# endif

   ENDIF

! east boundary
   IF(N_COUPLING_EAST>0)THEN
# if defined (PARALLEL)
    if ( n_east .eq. MPI_PROC_NULL ) then
      Kstart_EAST=J_START_EAST+Nghost -npy*Nglob/py
      Kend_EAST = J_START_EAST+Nghost+N_COUPLING_EAST-1 -npy*Nglob/py
      IF((Kstart_EAST>Nghost.AND.Kstart_EAST<Nloc-Nghost+1).OR.&
     (Kend_EAST<Nloc-Nghost+1.AND.Kend_EAST>Nghost))THEN
       IF(Kstart_EAST<Nghost+1)THEN
         Kshift_EAST= -Kstart_EAST+Nghost+1
         Kstart_EAST=Nghost+1
       ELSE
         Kshift_EAST=-(Kstart_EAST-Nghost)+1
       ENDIF
       IF(Kend_EAST>Nloc-Nghost)THEN
         Kend_EAST=Nloc-Nghost
       ENDIF
       IN_DOMAIN_EAST=.TRUE.
      ELSE

       IF(Kstart_EAST<=Nghost.AND.Kend_EAST>=Nloc-Nghost+1)THEN
         Kshift_EAST=-Kstart_EAST+Nghost+1
         Kstart_EAST = Nghost+1
         Kend_EAST = Nloc-Nghost
         IN_DOMAIN_EAST=.TRUE.
       ELSE
         IN_DOMAIN_EAST=.FALSE.
       ENDIF
      ENDIF

    endif
# else
      Kstart_EAST=J_START_EAST+Nghost
      Kend_EAST = J_START_EAST+Nghost+N_COUPLING_EAST-1
      Kshift_EAST = -(Kstart_EAST-Nghost)+1
      IN_DOMAIN_EAST = .TRUE.
# endif
    ENDIF

! south boundary
   IF(N_COUPLING_SOUTH>0)THEN
# if defined (PARALLEL)
    if ( n_suth .eq. MPI_PROC_NULL ) then
      Kstart_SOUTH=I_START_SOUTH+Nghost -npx*Mglob/px
      Kend_SOUTH = I_START_SOUTH+Nghost+N_COUPLING_SOUTH-1 -npx*Mglob/px
      IF((Kstart_SOUTH>Nghost.AND.Kstart_SOUTH<Mloc-Nghost+1).OR.&
     (Kend_SOUTH<Mloc-Nghost+1.AND.Kend_SOUTH>Nghost))THEN
       IF(Kstart_SOUTH<Nghost+1)THEN
         Kshift_SOUTH= -Kstart_SOUTH+Nghost+1
         Kstart_SOUTH=Nghost+1
       ELSE
         Kshift_SOUTH=-(Kstart_SOUTH-Nghost)+1
       ENDIF
       IF(Kend_SOUTH>Mloc-Nghost)THEN
         Kend_SOUTH=Mloc-Nghost
       ENDIF
       IN_DOMAIN_SOUTH=.TRUE.
      ELSE

       IF(Kstart_SOUTH<=Nghost.AND.Kend_SOUTH>=Mloc-Nghost+1)THEN
         Kshift_SOUTH=-Kstart_SOUTH+Nghost+1
         Kstart_SOUTH = Nghost+1
         Kend_SOUTH = Mloc-Nghost
         IN_DOMAIN_SOUTH=.TRUE.
       ELSE
         IN_DOMAIN_SOUTH=.FALSE.
       ENDIF
      ENDIF

    endif
# else
      Kstart_SOUTH=I_START_SOUTH+Nghost
      Kend_SOUTH = I_START_SOUTH+Nghost+N_COUPLING_SOUTH-1
      Kshift_SOUTH = -(Kstart_SOUTH-Nghost)+1
      IN_DOMAIN_SOUTH = .TRUE.
# endif
   ENDIF

! north boundary
   IF(N_COUPLING_NORTH>0)THEN
# if defined (PARALLEL)
    if ( n_nrth .eq. MPI_PROC_NULL ) then
      Kstart_NORTH=I_START_NORTH+Nghost -npx*Mglob/px
      Kend_NORTH = I_START_NORTH+Nghost+N_COUPLING_NORTH-1 -npx*Mglob/px
      IF((Kstart_NORTH>Nghost.AND.Kstart_NORTH<Mloc-Nghost+1).OR.&
     (Kend_NORTH<Mloc-Nghost+1.AND.Kend_NORTH>Nghost))THEN
       IF(Kstart_NORTH<Nghost+1)THEN
         Kshift_NORTH= -Kstart_NORTH+Nghost+1
         Kstart_NORTH=Nghost+1
       ELSE
         Kshift_NORTH=-(Kstart_NORTH-Nghost)+1
       ENDIF
       IF(Kend_NORTH>Mloc-Nghost)THEN
         Kend_NORTH=Mloc-Nghost
       ENDIF
       IN_DOMAIN_NORTH=.TRUE.
      ELSE

       IF(Kstart_NORTH<=Nghost.AND.Kend_NORTH>=Mloc-Nghost+1)THEN
         Kshift_NORTH=-Kstart_NORTH+Nghost+1
         Kstart_NORTH = Nghost+1
         Kend_NORTH = Mloc-Nghost
         IN_DOMAIN_NORTH=.TRUE.
       ELSE
         IN_DOMAIN_NORTH=.FALSE.
       ENDIF
      ENDIF

    endif
# else
      Kstart_NORTH=I_START_NORTH+Nghost
      Kend_NORTH = I_START_NORTH+Nghost+N_COUPLING_NORTH-1
      Kshift_NORTH = -(Kstart_NORTH-Nghost)+1
      IN_DOMAIN_NORTH = .TRUE.
# endif
   ENDIF

! open masks for coupling points
     IF(IN_DOMAIN_WEST)THEN
        DO J=Kstart_WEST,Kend_WEST
        DO I=1,Ibeg
          MASKu(I,J)=1
        ENDDO
        ENDDO
     ENDIF
     IF(IN_DOMAIN_EAST)THEN
        DO J=Kstart_EAST,Kend_EAST
        DO I=Iend1,Mloc1
          MASKu(I,J)=1
        ENDDO
        ENDDO
     ENDIF
     IF(IN_DOMAIN_SOUTH)THEN
        DO I=Kstart_SOUTH,Kend_SOUTH
        DO J=1,Jbeg
          MASKv(I,J)=1
        ENDDO
        ENDDO
     ENDIF
     IF(IN_DOMAIN_NORTH)THEN
        DO I=Kstart_NORTH,Kend_NORTH
        DO J=Jend1,Nloc1
          MASKv(I,J)=1
        ENDDO
        ENDDO
     ENDIF

# if defined(DEBUG)
# if defined (PARALLEL)
    IF(myid == 0)THEN
     WRITE(3,*)'Subroutine InitOneWayCoupling'
    ENDIF
# else
     WRITE(3,*)'Subroutine InitOneWayCoupling'
# endif
# endif

END SUBROUTINE InitOneWayCoupling
# endif
! end coupling



# if defined (COUPLING)
SUBROUTINE OneWayCoupling
     USE GLOBAL
     IMPLICIT NONE
119      FORMAT(5E16.6)  ! this is a fixed format for I/O

! determine time slot

    IF(TIME>TIME_COUPLING_1.AND.TIME>TIME_COUPLING_2) THEN
         TIME_COUPLING_1=TIME_COUPLING_2

         READ(11,*,END=120) TIME_COUPLING_2
! east
         IF(N_COUPLING_EAST.GT.0)THEN
             READ(11,*,END=120)   ! east

             U_COUPLING_EAST(:,1)=U_COUPLING_EAST(:,2)
             V_COUPLING_EAST(:,1)=V_COUPLING_EAST(:,2)
             Z_COUPLING_EAST(:,1)=Z_COUPLING_EAST(:,2)

             READ(11,119,END=120)(U_COUPLING_EAST(I,2),I=1,N_COUPLING_EAST)
             READ(11,119,END=120)(V_COUPLING_EAST(I,2),I=1,N_COUPLING_EAST)
             READ(11,119,END=120)(Z_COUPLING_EAST(I,2),I=1,N_COUPLING_EAST)
         ELSE
             READ(11,*,END=120)   ! east
         ENDIF
! west
         IF(N_COUPLING_WEST.GT.0)THEN
             READ(11,*,END=120)   ! west

             U_COUPLING_WEST(:,1)=U_COUPLING_WEST(:,2)
             V_COUPLING_WEST(:,1)=V_COUPLING_WEST(:,2)
             Z_COUPLING_WEST(:,1)=Z_COUPLING_WEST(:,2)


             READ(11,119,END=120)(U_COUPLING_WEST(I,2),I=1,N_COUPLING_WEST)
             READ(11,119,END=120)(V_COUPLING_WEST(I,2),I=1,N_COUPLING_WEST)
             READ(11,119,END=120)(Z_COUPLING_WEST(I,2),I=1,N_COUPLING_WEST)

         ELSE
             READ(11,*,END=120)   ! west
         ENDIF
! south
         IF(N_COUPLING_SOUTH.GT.0)THEN
             READ(11,*,END=120)   ! south

             U_COUPLING_SOUTH(:,1)=U_COUPLING_SOUTH(:,2)
             V_COUPLING_SOUTH(:,1)=V_COUPLING_SOUTH(:,2)
             Z_COUPLING_SOUTH(:,1)=Z_COUPLING_SOUTH(:,2)


             READ(11,119,END=120)(U_COUPLING_SOUTH(I,2),I=1,N_COUPLING_SOUTH)
             READ(11,119,END=120)(V_COUPLING_SOUTH(I,2),I=1,N_COUPLING_SOUTH)
             READ(11,119,END=120)(Z_COUPLING_SOUTH(I,2),I=1,N_COUPLING_SOUTH)
         ELSE
             READ(11,*,END=120)   ! south
         ENDIF
! north
         IF(N_COUPLING_NORTH.GT.0)THEN
             READ(11,*,END=120)   ! north

             U_COUPLING_NORTH(:,1)=U_COUPLING_NORTH(:,2)
             V_COUPLING_NORTH(:,1)=V_COUPLING_NORTH(:,2)
             Z_COUPLING_NORTH(:,1)=Z_COUPLING_NORTH(:,2)

             READ(11,119,END=120)(U_COUPLING_NORTH(I,2),I=1,N_COUPLING_NORTH)
             READ(11,119,END=120)(V_COUPLING_NORTH(I,2),I=1,N_COUPLING_NORTH)
             READ(11,119,END=120)(Z_COUPLING_NORTH(I,2),I=1,N_COUPLING_NORTH)
         ELSE
             READ(11,*,END=120)   ! north
         ENDIF


    ENDIF  ! time>time_2 and time_1

120 CONTINUE

    tmp2=ZERO
    tmp1=ZERO

    IF(TIME>TIME_COUPLING_1)THEN
      IF(TIME_COUPLING_1.EQ.TIME_COUPLING_2)THEN
        ! no more data
        tmp2=ZERO
        tmp1=ZERO
      ELSE
      tmp2=(TIME_COUPLING_2-TIME) &
            /MAX(SMALL, ABS(TIME_COUPLING_2-TIME_COUPLING_1))
      tmp1=1.0_SP - tmp2;
      ENDIF  ! no more data?
    ENDIF ! time>time_1


! west boundary
   IF(N_COUPLING_WEST>0)THEN
# if defined (PARALLEL)
    if ( n_west .eq. MPI_PROC_NULL ) then
# endif
     IF(IN_DOMAIN_WEST)THEN
      DO J=Kstart_WEST,Kend_WEST
      DO I=1,Nghost
        ETA(I,J)=Z_COUPLING_WEST(J-Nghost+Kshift_WEST,2)*tmp1&
                +Z_COUPLING_WEST(J-Nghost+Kshift_WEST,1)*tmp2
# if defined (COUPLING_NO_UV)
        P(I,J)=P(Ibeg,J)
        H_u(I,J)=H_u(Ibeg,J)
# else
        U(I,J)=U_COUPLING_WEST(J-Nghost+Kshift_WEST,2)*tmp1&
                +U_COUPLING_WEST(J-Nghost+Kshift_WEST,1)*tmp2
        V(I,J)=V_COUPLING_WEST(J-Nghost+Kshift_WEST,2)*tmp1&
                +V_COUPLING_WEST(J-Nghost+Kshift_WEST,1)*tmp2
        H_u(I,J)=H_u(Ibeg,J)
        H_v(I,J)=H_v(Ibeg,J)
        P(I,J)=H_u(I,J)*U(I,J)
        Q(I,J)=H_v(I,J)*V(I,J)
# endif
      ENDDO
      ENDDO
!# if defined(FREESLIP_BC)
!      ! closed wall, free-slip
!      IF(Kstart_WEST.GT.Nghost+1)THEN
!        DO J=1,Kstart_WEST-1
!        DO I=1,Nghost
!          Q(I,J)=Q(Ibeg,J)
!          H_v(I,J)=H_v(Ibeg,J)
!        ENDDO
!        ENDDO
!      ENDIF
!      !!!  Nloc1 or Nloc???
!      IF(Kend_WEST.LT.Nloc1-Nghost)THEN
!        DO J=Kend_WEST+1, Nloc1
!        DO I=1,Nghost
!          Q(I,J)=Q(Ibeg,J)
!          H_v(I,J)=H_v(Ibeg,J)
!        ENDDO
!        ENDDO
!      ENDIF
!# endif
     ENDIF  ! end in domain
# if defined (PARALLEL)
    endif
# endif

!# if defined (FREESLIP_BC)
!    ELSE ! closed wall, free-slip bc
!# if defined (PARALLEL)
!    if ( n_west .eq. MPI_PROC_NULL ) then
!# endif
!     DO J=1,Nloc1
!     DO I=1,Nghost
!       Q(I,J)=Q(Ibeg,J)
!       H_u(I,J)=H_u(Ibeg,J)
!     ENDDO
!     ENDDO
!# if defined (PARALLEL)
!    endif
!# endif
!
!# endif
    ENDIF ! end of n_coupling_west>0

! east boundary
   IF(N_COUPLING_EAST>0)THEN
# if defined (PARALLEL)
    if ( n_east .eq. MPI_PROC_NULL ) then
# endif
     IF(IN_DOMAIN_EAST)THEN
      DO J=Kstart_EAST,Kend_EAST
      DO I=Iend+1,Iend+Nghost
        ETA(I,J)=Z_COUPLING_EAST(J-Nghost+Kshift_EAST,2)*tmp1&
                +Z_COUPLING_EAST(J-Nghost+Kshift_EAST,1)*tmp2

# if defined (COUPLING_NO_UV)
        P(I+1,J)=P(Iend1,J)
        H_u(I+1,J)=H_u(Iend1,J)
# else
        U(I+1,J)=U_COUPLING_EAST(J-Nghost+Kshift_EAST,2)*tmp1&
                +U_COUPLING_EAST(J-Nghost+Kshift_EAST,1)*tmp2
        V(I,J)=V_COUPLING_EAST(J-Nghost+Kshift_EAST,2)*tmp1&
                +V_COUPLING_EAST(J-Nghost+Kshift_EAST,1)*tmp2
        H_u(I+1,J)=H_u(Iend1,J)
        H_v(I,J)=H_v(Iend,J)
        P(I+1,J)=H_u(I+1,J)*U(I+1,J)
        Q(I,J)=H_v(I,J)*V(I,J)
# endif

      ENDDO
      ENDDO
!# if defined (FREESLIP_BC)
!      ! closed wall, free-slip bc
!      IF(Kstart_EAST.GT.Nghost+1)THEN
!       DO J=1,Kstart_EAST-1
!       DO I=Iend+1,Iend+Nghost
!         Q(I,J)=Q(Iend,J)
!         H_v(I,J)=H_v(Iend,J)
!       ENDDO
!       ENDDO
!      ENDIF
!      IF(Kend_EAST.LT.Nloc1+Nghost)THEN
!        DO J=Kend_EAST+1,Nloc1
!        DO I=Iend+1,Iend+Nghost
!          Q(I,J)=Q(Iend,J)
!          H_v(I,J)=H_v(Iend,J)
!        ENDDO
!        ENDDO
!      ENDIF
!# endif
     ENDIF  ! end in domain
# if defined (PARALLEL)
    endif
# endif

!# if defined (FREESLIP_BC)
!    ELSE ! closed wall, free-slip bc
!# if defined (PARALLEL)
!    if ( n_east .eq. MPI_PROC_NULL ) then
!# endif
!     DO J=1,Nloc1
!     DO I=Iend+1,Iend+Nghost
!       Q(I,J)=Q(Iend,J)
!       H_v(I,J)=H_v(Iend,J)
!     ENDDO
!     ENDDO
!# if defined (PARALLEL)
!    endif
!# endif
!# endif
    ENDIF ! end of n_coupling_east>0

! south boundary
   IF(N_COUPLING_SOUTH>0)THEN
# if defined (PARALLEL)
    if ( n_suth .eq. MPI_PROC_NULL ) then
# endif
     IF(IN_DOMAIN_SOUTH)THEN
      DO I=Kstart_SOUTH,Kend_SOUTH
      DO J=1,Nghost
        ETA(I,J)=Z_COUPLING_SOUTH(I-Nghost+Kshift_SOUTH,2)*tmp1&
                +Z_COUPLING_SOUTH(I-Nghost+Kshift_SOUTH,1)*tmp2

# if defined (COUPLING_NO_UV)
        Q(I,J)=Q(I,Jbeg)
        H_v(I,J)=H_v(I,Jbeg)
# else
        U(I,J)=U_COUPLING_SOUTH(J-Nghost+Kshift_SOUTH,2)*tmp1&
                +U_COUPLING_SOUTH(J-Nghost+Kshift_SOUTH,1)*tmp2
        V(I,J)=V_COUPLING_SOUTH(J-Nghost+Kshift_SOUTH,2)*tmp1&
                +V_COUPLING_SOUTH(J-Nghost+Kshift_SOUTH,1)*tmp2
        H_u(I,J)=H_u(I,Jbeg)
        H_v(I,J)=H_v(I,Jbeg)
        P(I,J)=H_u(I,J)*U(I,J)
        Q(I,J)=H_v(I,J)*V(I,J)
# endif

      ENDDO
      ENDDO
!# if defined (FREESLIP_BC)
!     ! closed wall, free-slip bc
!     ELSE
!       IF(Kstart_SOUTH.GT.Nghost+1)THEN
!         DO I=1, Kstart_SOUTH-1
!         DO J=1,Nghost
!           P(I,J)=P(I,Jbeg)
!           H_u(I,J)=H_u(I,Jbeg)
!         ENDDO
!         ENDDO
!       ENDIF
!       IF(Kend_SOUTH.LT.Mloc1)THEN
!         DO I=Kend_SOUTH+1,Mloc1
!         DO J=1,Nghost
!           P(I,J)=P(I,Jbeg)
!           H_u(I,J)=H_u(I,Jbeg)
!         ENDDO
!         ENDDO
!       ENDIF
!# endif
     ENDIF  ! end in domain
# if defined (PARALLEL)
    endif
# endif

!# if defined (FREESLIP_BC)
!    ELSE
!# if defined (PARALLEL)
!    if ( n_suth .eq. MPI_PROC_NULL ) then
!# endif
!    ! closed wall, free-slip bc
!      DO I=1,Mloc1
!      DO J=1,Nghost
!        P(I,J)=P(I,Jbeg)
!        H_u(I,J)=H_u(I,Jbeg)
!      ENDDO
!      ENDDO
!# if defined (PARALLEL)
!    endif
!# endif
!# endif
    ENDIF ! end of n_coupling_south>0

! north boundary
   IF(N_COUPLING_NORTH>0)THEN
# if defined (PARALLEL)
    if ( n_nrth .eq. MPI_PROC_NULL ) then
# endif
     IF(IN_DOMAIN_NORTH)THEN
      DO I=Kstart_NORTH,Kend_NORTH
      DO J=Jend+1,Jend+Nghost
        ETA(I,J)=Z_COUPLING_NORTH(I-Nghost+Kshift_NORTH,2)*tmp1&
                +Z_COUPLING_NORTH(I-Nghost+Kshift_NORTH,1)*tmp2

# if defined (COUPLING_NO_UV)
        Q(I,J+1)=Q(I,Jend1)
        H_v(I,J+1)=H_v(I,Jend1)
# else
        U(I,J)=U_COUPLING_NORTH(J-Nghost+Kshift_NORTH,2)*tmp1&
                +U_COUPLING_NORTH(J-Nghost+Kshift_NORTH,1)*tmp2
        V(I,J+1)=V_COUPLING_NORTH(J-Nghost+Kshift_NORTH,2)*tmp1&
                +V_COUPLING_NORTH(J-Nghost+Kshift_NORTH,1)*tmp2
        H_u(I,J)=H_u(I,Jend1)
        H_v(I,J)=H_v(I,Jend1)
        P(I,J)=H_u(I,J)*U(I+1,J)
        Q(I,J+1)=H_v(I,J+1)*V(I,J+1)
# endif

      ENDDO
      ENDDO
!# if defined (FREESLIP_BC)
!      IF(Kstart_NORTH.GT.Nghost+1)THEN
!        DO I=Nghost+1,Kstart_NORTH-1
!        DO J=Jend+1,Jend+Nghost
!          P(I,J)=P(I,Jend)
!          H_u(I,J)=H_u(I,Jend)
!        ENDDO
!        ENDDO
!      ENDIF
!      IF(Kend_NORTH.LT.Mloc1)THEN
!        DO I=Kend_NORTH+1,Mloc1
!        DO J=Jend+1,Jend+Nghost
!          P(I,J)=P(I,Jend)
!          H_u(I,J)=H_u(I,Jend)
!        ENDDO
!        ENDDO
!      ENDIF
!# endif
     ENDIF  ! end in domain
# if defined (PARALLEL)
    endif
# endif

!# if defined (FREESLIP_BC)
!    ELSE
!# if defined (PARALLEL)
!    if ( n_nrth .eq. MPI_PROC_NULL ) then
!# endif
!      DO I=1,Mloc1
!      DO J=Jend+1,Jend+Nghost
!        P(I,J)=P(I,Jend)
!        H_u(I,J)=H_u(I,Jend)
!      ENDDO
!      ENDDO
!# if defined (PARALLEL)
!    endif
!# endif
!# endif
    ENDIF ! end of n_coupling_north>0

# if defined (DEBUG)
# if defined (PARALLEL)
    IF(myid == 0)THEN
     WRITE(3,*)'Subroutine OneWayCoupling'
    ENDIF
# else
     WRITE(3,*)'Subroutine OneWayCoupling'
# endif
# endif

END SUBROUTINE OneWayCoupling
# endif
