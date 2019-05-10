
# if defined (PARALLEL)
SUBROUTINE phi_exch_1 (PHI)
  USE PARAM
  USE GLOBAL
  IMPLICIT NONE
  REAL(SP),INTENT(INOUT) :: PHI(Mloc,Nloc)

  INTEGER,DIMENSION(MPI_STATUS_SIZE,4) :: status
  INTEGER,DIMENSION(4) :: req
  INTEGER :: nreq,len
  REAL(SP),DIMENSION(Mloc,Nghost) :: rNmsg, sNmsg,rSmsg,sSmsg
  REAL(SP),DIMENSION(Nloc,Nghost) :: rWmsg, sWmsg,rEmsg,sEmsg

! for east-west

  len = Nloc * Nghost

  nreq = 0
  if ( n_west .ne. MPI_PROC_NULL ) then
    nreq = nreq + 1
    call MPI_IRECV( rWmsg, len, MPI_SP, &
      n_west, 0, comm2d, req(nreq), ier )
    do j = 1, Nloc
    do i = 1, Nghost
     sWmsg(j,i) = PHI(Ibeg+i-1,j)
    enddo
    enddo
    nreq = nreq +1
    call MPI_ISEND( sWmsg, len, MPI_SP, &
      n_west, 1, comm2d, req(nreq), ier )
  endif

  if ( n_east .ne. MPI_PROC_NULL ) then
    nreq = nreq + 1
    call MPI_IRECV( rEmsg, len, MPI_SP, &
      n_east, 1, comm2d, req(nreq), ier )
    do j = 1, Nloc
    do i = 1, Nghost
     sEmsg(j,i) = PHI(Iend-i+1,j)
    enddo
    enddo
    nreq = nreq +1
    call MPI_ISEND( sEmsg, len, MPI_SP, &
      n_east, 0, comm2d, req(nreq), ier )
  endif

  call MPI_WAITALL( nreq, req, status, ier )

  if ( n_west .ne. MPI_PROC_NULL ) then
    do j = 1, Nloc
    do i = 1, Nghost
     PHI(Ibeg-i,j) = rWmsg(j,i)
    enddo
    enddo
  endif

  if ( n_east .ne. MPI_PROC_NULL ) then
    do j = 1, Nloc
    do i = 1, Nghost
     PHI(Iend+i,j) = rEmsg(j,i)
    enddo
    enddo
  endif

! for nrth-suth

  len = Mloc * Nghost

  nreq = 0
  if ( n_suth .ne. MPI_PROC_NULL ) then
    nreq = nreq + 1
    call MPI_IRECV( rSmsg, len, MPI_SP, &
      n_suth, 0, comm2d, req(nreq), ier )
    do i = 1, Mloc
    do j = 1, Nghost
     sSmsg(i,j) = PHI(i,Jbeg+j-1)
    enddo
    enddo
    nreq = nreq +1
    call MPI_ISEND( sSmsg, len, MPI_SP, &
      n_suth, 1, comm2d, req(nreq), ier )
  endif

  if ( n_nrth .ne. MPI_PROC_NULL ) then
    nreq = nreq + 1
    call MPI_IRECV( rNmsg, len, MPI_SP, &
      n_nrth, 1, comm2d, req(nreq), ier )
    do i = 1, Mloc
    do j = 1, Nghost
     sNmsg(i,j) = PHI(i,Jend-j+1)
    enddo
    enddo
    nreq = nreq + 1
    call MPI_ISEND( sNmsg, len, MPI_SP, &
      n_nrth, 0, comm2d, req(nreq), ier )
  endif

  call MPI_WAITALL( nreq, req, status, ier )

  if ( n_suth .ne. MPI_PROC_NULL ) then
    do i = 1, Mloc
    do j = 1, Nghost
     PHI(i,Jbeg-j) = rSmsg(i,j)
    enddo
    enddo
  endif

  if ( n_nrth .ne. MPI_PROC_NULL ) then
    do i = 1, Mloc
    do j = 1, Nghost
     PHI(i,Jend+j) = rNmsg(i,j)
    enddo
    enddo
  endif

END SUBROUTINE phi_exch_1
# endif

# if defined (PARALLEL)
SUBROUTINE phi_exch_2 (PHI)
  USE PARAM
  USE GLOBAL
  IMPLICIT NONE
  REAL(SP),INTENT(INOUT) :: PHI(Mloc1,Nloc)

  INTEGER,DIMENSION(MPI_STATUS_SIZE,4) :: status
  INTEGER,DIMENSION(4) :: req
  INTEGER :: nreq,len
  REAL(SP),DIMENSION(Mloc1,Nghost) :: rNmsg, sNmsg,rSmsg,sSmsg
  REAL(SP),DIMENSION(Nloc,Nghost) :: rWmsg, sWmsg,rEmsg,sEmsg

! for east-west

  len = Nloc * Nghost

  nreq = 0
  if ( n_west .ne. MPI_PROC_NULL ) then
    nreq = nreq + 1
    call MPI_IRECV( rWmsg, len, MPI_SP, &
      n_west, 0, comm2d, req(nreq), ier )
    do j = 1, Nloc
    do i = 1, Nghost
     sWmsg(j,i) = PHI(Ibeg+i,j)
    enddo
    enddo
    nreq = nreq +1
    call MPI_ISEND( sWmsg, len, MPI_SP, &
      n_west, 1, comm2d, req(nreq), ier )
  endif

  if ( n_east .ne. MPI_PROC_NULL ) then
    nreq = nreq + 1
    call MPI_IRECV( rEmsg, len, MPI_SP, &
      n_east, 1, comm2d, req(nreq), ier )
    do j = 1, Nloc
    do i = 1, Nghost
     sEmsg(j,i) = PHI(Iend1-i,j)
    enddo
    enddo
    nreq = nreq +1
    call MPI_ISEND( sEmsg, len, MPI_SP, &
      n_east, 0, comm2d, req(nreq), ier )
  endif

  call MPI_WAITALL( nreq, req, status, ier )

  if ( n_west .ne. MPI_PROC_NULL ) then
    do j = 1, Nloc
    do i = 1, Nghost
     PHI(Ibeg-i,j) = rWmsg(j,i)
    enddo
    enddo
  endif

  if ( n_east .ne. MPI_PROC_NULL ) then
    do j = 1, Nloc
    do i = 1, Nghost
     PHI(Iend1+i,j) = rEmsg(j,i)
    enddo
    enddo
  endif

! for nrth-suth

  len = Mloc1 * Nghost

  nreq = 0
  if ( n_suth .ne. MPI_PROC_NULL ) then
    nreq = nreq + 1
    call MPI_IRECV( rSmsg, len, MPI_SP, &
      n_suth, 0, comm2d, req(nreq), ier )
    do i = 1, Mloc1
    do j = 1, Nghost
     sSmsg(i,j) = PHI(i,Jbeg+j-1)
    enddo
    enddo
    nreq = nreq +1
    call MPI_ISEND( sSmsg, len, MPI_SP, &
      n_suth, 1, comm2d, req(nreq), ier )
  endif

  if ( n_nrth .ne. MPI_PROC_NULL ) then
    nreq = nreq + 1
    call MPI_IRECV( rNmsg, len, MPI_SP, &
      n_nrth, 1, comm2d, req(nreq), ier )
    do i = 1, Mloc1
    do j = 1, Nghost
     sNmsg(i,j) = PHI(i,Jend-j+1)
    enddo
    enddo
    nreq = nreq + 1
    call MPI_ISEND( sNmsg, len, MPI_SP, &
      n_nrth, 0, comm2d, req(nreq), ier )
  endif

  call MPI_WAITALL( nreq, req, status, ier )

  if ( n_suth .ne. MPI_PROC_NULL ) then
    do i = 1, Mloc1
    do j = 1, Nghost
     PHI(i,Jbeg-j) = rSmsg(i,j)
    enddo
    enddo
  endif

  if ( n_nrth .ne. MPI_PROC_NULL ) then
    do i = 1, Mloc1
    do j = 1, Nghost
     PHI(i,Jend+j) = rNmsg(i,j)
    enddo
    enddo
  endif

END SUBROUTINE phi_exch_2
# endif

# if defined (PARALLEL)
SUBROUTINE phi_exch_3 (PHI)
  USE PARAM
  USE GLOBAL
  IMPLICIT NONE
  REAL(SP),INTENT(INOUT) :: PHI(Mloc,Nloc1)

  INTEGER,DIMENSION(MPI_STATUS_SIZE,4) :: status
  INTEGER,DIMENSION(4) :: req
  INTEGER :: nreq,len
  REAL(SP),DIMENSION(Mloc,Nghost) :: rNmsg, sNmsg,rSmsg,sSmsg
  REAL(SP),DIMENSION(Nloc1,Nghost) :: rWmsg, sWmsg,rEmsg,sEmsg

! for east-west

  len = Nloc1 * Nghost

  nreq = 0
  if ( n_west .ne. MPI_PROC_NULL ) then
    nreq = nreq + 1
    call MPI_IRECV( rWmsg, len, MPI_SP, &
      n_west, 0, comm2d, req(nreq), ier )
    do j = 1, Nloc1
    do i = 1, Nghost
     sWmsg(j,i) = PHI(Ibeg+i-1,j)
    enddo
    enddo
    nreq = nreq +1
    call MPI_ISEND( sWmsg, len, MPI_SP, &
      n_west, 1, comm2d, req(nreq), ier )
  endif

  if ( n_east .ne. MPI_PROC_NULL ) then
    nreq = nreq + 1
    call MPI_IRECV( rEmsg, len, MPI_SP, &
      n_east, 1, comm2d, req(nreq), ier )
    do j = 1, Nloc1
    do i = 1, Nghost
     sEmsg(j,i) = PHI(Iend-i+1,j)
    enddo
    enddo
    nreq = nreq +1
    call MPI_ISEND( sEmsg, len, MPI_SP, &
      n_east, 0, comm2d, req(nreq), ier )
  endif

  call MPI_WAITALL( nreq, req, status, ier )

  if ( n_west .ne. MPI_PROC_NULL ) then
    do j = 1, Nloc1
    do i = 1, Nghost
     PHI(Ibeg-i,j) = rWmsg(j,i)
    enddo
    enddo
  endif

  if ( n_east .ne. MPI_PROC_NULL ) then
    do j = 1, Nloc1
    do i = 1, Nghost
     PHI(Iend+i,j) = rEmsg(j,i)
    enddo
    enddo
  endif

! for nrth-suth

  len = Mloc * Nghost

  nreq = 0
  if ( n_suth .ne. MPI_PROC_NULL ) then
    nreq = nreq + 1
    call MPI_IRECV( rSmsg, len, MPI_SP, &
      n_suth, 0, comm2d, req(nreq), ier )
    do i = 1, Mloc
    do j = 1, Nghost
     sSmsg(i,j) = PHI(i,Jbeg+j)
    enddo
    enddo
    nreq = nreq +1
    call MPI_ISEND( sSmsg, len, MPI_SP, &
      n_suth, 1, comm2d, req(nreq), ier )
  endif

  if ( n_nrth .ne. MPI_PROC_NULL ) then
    nreq = nreq + 1
    call MPI_IRECV( rNmsg, len, MPI_SP, &
      n_nrth, 1, comm2d, req(nreq), ier )
    do i = 1, Mloc
    do j = 1, Nghost
     sNmsg(i,j) = PHI(i,Jend1-j)
    enddo
    enddo
    nreq = nreq + 1
    call MPI_ISEND( sNmsg, len, MPI_SP, &
      n_nrth, 0, comm2d, req(nreq), ier )
  endif

  call MPI_WAITALL( nreq, req, status, ier )

  if ( n_suth .ne. MPI_PROC_NULL ) then
    do i = 1, Mloc
    do j = 1, Nghost
     PHI(i,Jbeg-j) = rSmsg(i,j)
    enddo
    enddo
  endif

  if ( n_nrth .ne. MPI_PROC_NULL ) then
    do i = 1, Mloc
    do j = 1, Nghost
     PHI(i,Jend1+j) = rNmsg(i,j)
    enddo
    enddo
  endif

END SUBROUTINE phi_exch_3
# endif


# if defined (PARALLEL)
SUBROUTINE PHI_EXCH_SUBGRID (PHI)
  USE PARAM
  USE GLOBAL
  IMPLICIT NONE
  REAL(SP),INTENT(INOUT) :: PHI(Mloc,Nloc,SubMainGridRatio,SubMainGridRatio)

  INTEGER,DIMENSION(MPI_STATUS_SIZE,4) :: status
  INTEGER,DIMENSION(4) :: req
  INTEGER :: nreq,len
  REAL(SP),DIMENSION(Mloc,Nghost,SubMainGridRatio,SubMainGridRatio) :: rNmsg, sNmsg,rSmsg,sSmsg
  REAL(SP),DIMENSION(Nloc,Nghost,SubMainGridRatio,SubMainGridRatio) :: rWmsg, sWmsg,rEmsg,sEmsg

! for east-west

  len = Nloc * Nghost * SubMainGridRatio * SubMainGridRatio

  nreq = 0
  if ( n_west .ne. MPI_PROC_NULL ) then
    nreq = nreq + 1
    call MPI_IRECV( rWmsg, len, MPI_SP, &
      n_west, 0, comm2d, req(nreq), ier )
    do j = 1, Nloc
    do i = 1, Nghost
    do jj = 1, SubMainGridRatio
    do ii = 1, SubMainGridRatio
     sWmsg(j,i,ii,jj) = PHI(Ibeg+i-1,j,ii,jj)
    enddo
    enddo
    enddo
    enddo
    nreq = nreq +1
    call MPI_ISEND( sWmsg, len, MPI_SP, &
      n_west, 1, comm2d, req(nreq), ier )
  endif

  if ( n_east .ne. MPI_PROC_NULL ) then
    nreq = nreq + 1
    call MPI_IRECV( rEmsg, len, MPI_SP, &
      n_east, 1, comm2d, req(nreq), ier )
    do j = 1, Nloc
    do i = 1, Nghost
     do jj = 1, SubMainGridRatio
     do ii = 1, SubMainGridRatio
     sEmsg(j,i,ii,jj) = PHI(Iend-i+1,j,ii,jj)
     enddo
     enddo
    enddo
    enddo
    nreq = nreq +1
    call MPI_ISEND( sEmsg, len, MPI_SP, &
      n_east, 0, comm2d, req(nreq), ier )
  endif

  call MPI_WAITALL( nreq, req, status, ier )

  if ( n_west .ne. MPI_PROC_NULL ) then
    do j = 1, Nloc
    do i = 1, Nghost
     do jj = 1, SubMainGridRatio
     do ii = 1, SubMainGridRatio
     PHI(Ibeg-i,j,ii,jj) = rWmsg(j,i,ii,jj)
     enddo
     enddo
    enddo
    enddo
  endif

  if ( n_east .ne. MPI_PROC_NULL ) then
    do j = 1, Nloc
    do i = 1, Nghost
     do jj = 1, SubMainGridRatio
     do ii = 1, SubMainGridRatio
     PHI(Iend+i,j,ii,jj) = rEmsg(j,i,ii,jj)
     enddo
     enddo
    enddo
    enddo
  endif

! for nrth-suth

  len = Mloc * Nghost * SubMainGridRatio * SubMainGridRatio

  nreq = 0
  if ( n_suth .ne. MPI_PROC_NULL ) then
    nreq = nreq + 1
    call MPI_IRECV( rSmsg, len, MPI_SP, &
      n_suth, 0, comm2d, req(nreq), ier )
    do i = 1, Mloc
    do j = 1, Nghost
     do ii = 1, SubMainGridRatio
     do jj = 1, SubMainGridRatio
     sSmsg(i,j,ii,jj) = PHI(i,Jbeg+j-1,ii,jj)
     enddo
     enddo
    enddo
    enddo
    nreq = nreq +1
    call MPI_ISEND( sSmsg, len, MPI_SP, &
      n_suth, 1, comm2d, req(nreq), ier )
  endif

  if ( n_nrth .ne. MPI_PROC_NULL ) then
    nreq = nreq + 1
    call MPI_IRECV( rNmsg, len, MPI_SP, &
      n_nrth, 1, comm2d, req(nreq), ier )
    do i = 1, Mloc
    do j = 1, Nghost
     do ii = 1, SubMainGridRatio
     do jj = 1, SubMainGridRatio
     sNmsg(i,j,ii,jj) = PHI(i,Jend-j+1,ii,jj)
     enddo
     enddo
    enddo
    enddo
    nreq = nreq + 1
    call MPI_ISEND( sNmsg, len, MPI_SP, &
      n_nrth, 0, comm2d, req(nreq), ier )
  endif

  call MPI_WAITALL( nreq, req, status, ier )

  if ( n_suth .ne. MPI_PROC_NULL ) then
    do i = 1, Mloc
    do j = 1, Nghost
     do ii = 1, SubMainGridRatio
     do jj = 1, SubMainGridRatio
     PHI(i,Jbeg-j,ii,jj) = rSmsg(i,j,ii,jj)
     enddo
     enddo
    enddo
    enddo
  endif

  if ( n_nrth .ne. MPI_PROC_NULL ) then
    do i = 1, Mloc
    do j = 1, Nghost
     do ii = 1, SubMainGridRatio
     do jj = 1, SubMainGridRatio
     PHI(i,Jend+j,ii,jj) = rNmsg(i,j,ii,jj)
     enddo
     enddo
    enddo
    enddo
  endif


END SUBROUTINE phi_exch_subgrid

# else

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

# endif



# if defined (PARALLEL)
SUBROUTINE PHI_EXCH_USUBGRID (PHI)
  USE PARAM
  USE GLOBAL
  IMPLICIT NONE
  REAL(SP),INTENT(INOUT) :: PHI(Mloc1,Nloc,SubMainGridRatio,SubMainGridRatio)

  INTEGER,DIMENSION(MPI_STATUS_SIZE,4) :: status
  INTEGER,DIMENSION(4) :: req
  INTEGER :: nreq,len
  REAL(SP),DIMENSION(Mloc1,Nghost,SubMainGridRatio,SubMainGridRatio) :: rNmsg, sNmsg,rSmsg,sSmsg
  REAL(SP),DIMENSION(Nloc,Nghost,SubMainGridRatio,SubMainGridRatio) :: rWmsg, sWmsg,rEmsg,sEmsg

! for east-west

  len = Nloc * Nghost * SubMainGridRatio * SubMainGridRatio

  nreq = 0
  if ( n_west .ne. MPI_PROC_NULL ) then
    nreq = nreq + 1
    call MPI_IRECV( rWmsg, len, MPI_SP, &
      n_west, 0, comm2d, req(nreq), ier )
    do j = 1, Nloc
    do i = 1, Nghost
    do jj = 1, SubMainGridRatio
    do ii = 1, SubMainGridRatio
     sWmsg(j,i,ii,jj) = PHI(Ibeg+i,j,ii,jj)
    enddo
    enddo
    enddo
    enddo
    nreq = nreq +1
    call MPI_ISEND( sWmsg, len, MPI_SP, &
      n_west, 1, comm2d, req(nreq), ier )
  endif

  if ( n_east .ne. MPI_PROC_NULL ) then
    nreq = nreq + 1
    call MPI_IRECV( rEmsg, len, MPI_SP, &
      n_east, 1, comm2d, req(nreq), ier )
    do j = 1, Nloc
    do i = 1, Nghost
     do jj = 1, SubMainGridRatio
     do ii = 1, SubMainGridRatio
     sEmsg(j,i,ii,jj) = PHI(Iend1-i,j,ii,jj)
     enddo
     enddo
    enddo
    enddo
    nreq = nreq +1
    call MPI_ISEND( sEmsg, len, MPI_SP, &
      n_east, 0, comm2d, req(nreq), ier )
  endif

  call MPI_WAITALL( nreq, req, status, ier )

  if ( n_west .ne. MPI_PROC_NULL ) then
    do j = 1, Nloc
    do i = 1, Nghost
     do jj = 1, SubMainGridRatio
     do ii = 1, SubMainGridRatio
     PHI(Ibeg-i,j,ii,jj) = rWmsg(j,i,ii,jj)
     enddo
     enddo
    enddo
    enddo
  endif

  if ( n_east .ne. MPI_PROC_NULL ) then
    do j = 1, Nloc
    do i = 1, Nghost
     do jj = 1, SubMainGridRatio
     do ii = 1, SubMainGridRatio
     PHI(Iend1+i,j,ii,jj) = rEmsg(j,i,ii,jj)
     enddo
     enddo
    enddo
    enddo
  endif

! for nrth-suth

  len = Mloc1 * Nghost * SubMainGridRatio * SubMainGridRatio

  nreq = 0
  if ( n_suth .ne. MPI_PROC_NULL ) then
    nreq = nreq + 1
    call MPI_IRECV( rSmsg, len, MPI_SP, &
      n_suth, 0, comm2d, req(nreq), ier )
    do i = 1, Mloc1
    do j = 1, Nghost
     do ii = 1, SubMainGridRatio
     do jj = 1, SubMainGridRatio
     sSmsg(i,j,ii,jj) = PHI(i,Jbeg+j-1,ii,jj)
     enddo
     enddo
    enddo
    enddo
    nreq = nreq +1
    call MPI_ISEND( sSmsg, len, MPI_SP, &
      n_suth, 1, comm2d, req(nreq), ier )
  endif

  if ( n_nrth .ne. MPI_PROC_NULL ) then
    nreq = nreq + 1
    call MPI_IRECV( rNmsg, len, MPI_SP, &
      n_nrth, 1, comm2d, req(nreq), ier )
    do i = 1, Mloc1
    do j = 1, Nghost
     do ii = 1, SubMainGridRatio
     do jj = 1, SubMainGridRatio
     sNmsg(i,j,ii,jj) = PHI(i,Jend-j+1,ii,jj)
     enddo
     enddo
    enddo
    enddo
    nreq = nreq + 1
    call MPI_ISEND( sNmsg, len, MPI_SP, &
      n_nrth, 0, comm2d, req(nreq), ier )
  endif

  call MPI_WAITALL( nreq, req, status, ier )

  if ( n_suth .ne. MPI_PROC_NULL ) then
    do i = 1, Mloc1
    do j = 1, Nghost
     do ii = 1, SubMainGridRatio
     do jj = 1, SubMainGridRatio
     PHI(i,Jbeg-j,ii,jj) = rSmsg(i,j,ii,jj)
     enddo
     enddo
    enddo
    enddo
  endif

  if ( n_nrth .ne. MPI_PROC_NULL ) then
    do i = 1, Mloc1
    do j = 1, Nghost
     do ii = 1, SubMainGridRatio
     do jj = 1, SubMainGridRatio
     PHI(i,Jend+j,ii,jj) = rNmsg(i,j,ii,jj)
     enddo
     enddo
    enddo
    enddo
  endif

END SUBROUTINE phi_exch_usubgrid

# endif

# if defined (PARALLEL)
SUBROUTINE PHI_EXCH_VSUBGRID (PHI)
  USE PARAM
  USE GLOBAL
  IMPLICIT NONE
  REAL(SP),INTENT(INOUT) :: PHI(Mloc,Nloc1,SubMainGridRatio,SubMainGridRatio)

  INTEGER,DIMENSION(MPI_STATUS_SIZE,4) :: status
  INTEGER,DIMENSION(4) :: req
  INTEGER :: nreq,len
  REAL(SP),DIMENSION(Mloc,Nghost,SubMainGridRatio,SubMainGridRatio) :: rNmsg, sNmsg,rSmsg,sSmsg
  REAL(SP),DIMENSION(Nloc1,Nghost,SubMainGridRatio,SubMainGridRatio) :: rWmsg, sWmsg,rEmsg,sEmsg

! for east-west

  len = Nloc1 * Nghost * SubMainGridRatio * SubMainGridRatio

  nreq = 0
  if ( n_west .ne. MPI_PROC_NULL ) then
    nreq = nreq + 1
    call MPI_IRECV( rWmsg, len, MPI_SP, &
      n_west, 0, comm2d, req(nreq), ier )
    do j = 1, Nloc1
    do i = 1, Nghost
    do jj = 1, SubMainGridRatio
    do ii = 1, SubMainGridRatio
     sWmsg(j,i,ii,jj) = PHI(Ibeg+i-1,j,ii,jj)
    enddo
    enddo
    enddo
    enddo
    nreq = nreq +1
    call MPI_ISEND( sWmsg, len, MPI_SP, &
      n_west, 1, comm2d, req(nreq), ier )
  endif

  if ( n_east .ne. MPI_PROC_NULL ) then
    nreq = nreq + 1
    call MPI_IRECV( rEmsg, len, MPI_SP, &
      n_east, 1, comm2d, req(nreq), ier )
    do j = 1, Nloc1
    do i = 1, Nghost
     do jj = 1, SubMainGridRatio
     do ii = 1, SubMainGridRatio
     sEmsg(j,i,ii,jj) = PHI(Iend-i+1,j,ii,jj)
     enddo
     enddo
    enddo
    enddo
    nreq = nreq +1
    call MPI_ISEND( sEmsg, len, MPI_SP, &
      n_east, 0, comm2d, req(nreq), ier )
  endif

  call MPI_WAITALL( nreq, req, status, ier )

  if ( n_west .ne. MPI_PROC_NULL ) then
    do j = 1, Nloc1
    do i = 1, Nghost
     do jj = 1, SubMainGridRatio
     do ii = 1, SubMainGridRatio
     PHI(Ibeg-i,j,ii,jj) = rWmsg(j,i,ii,jj)
     enddo
     enddo
    enddo
    enddo
  endif

  if ( n_east .ne. MPI_PROC_NULL ) then
    do j = 1, Nloc1
    do i = 1, Nghost
     do jj = 1, SubMainGridRatio
     do ii = 1, SubMainGridRatio
     PHI(Iend+i,j,ii,jj) = rEmsg(j,i,ii,jj)
     enddo
     enddo
    enddo
    enddo
  endif

! for nrth-suth

  len = Mloc * Nghost * SubMainGridRatio * SubMainGridRatio

  nreq = 0
  if ( n_suth .ne. MPI_PROC_NULL ) then
    nreq = nreq + 1
    call MPI_IRECV( rSmsg, len, MPI_SP, &
      n_suth, 0, comm2d, req(nreq), ier )
    do i = 1, Mloc
    do j = 1, Nghost
     do ii = 1, SubMainGridRatio
     do jj = 1, SubMainGridRatio
     sSmsg(i,j,ii,jj) = PHI(i,Jbeg+j,ii,jj)
     enddo
     enddo
    enddo
    enddo
    nreq = nreq +1
    call MPI_ISEND( sSmsg, len, MPI_SP, &
      n_suth, 1, comm2d, req(nreq), ier )
  endif

  if ( n_nrth .ne. MPI_PROC_NULL ) then
    nreq = nreq + 1
    call MPI_IRECV( rNmsg, len, MPI_SP, &
      n_nrth, 1, comm2d, req(nreq), ier )
    do i = 1, Mloc
    do j = 1, Nghost
     do ii = 1, SubMainGridRatio
     do jj = 1, SubMainGridRatio
     sNmsg(i,j,ii,jj) = PHI(i,Jend1-j,ii,jj)
     enddo
     enddo
    enddo
    enddo
    nreq = nreq + 1
    call MPI_ISEND( sNmsg, len, MPI_SP, &
      n_nrth, 0, comm2d, req(nreq), ier )
  endif

  call MPI_WAITALL( nreq, req, status, ier )

  if ( n_suth .ne. MPI_PROC_NULL ) then
    do i = 1, Mloc
    do j = 1, Nghost
     do ii = 1, SubMainGridRatio
     do jj = 1, SubMainGridRatio
     PHI(i,Jbeg-j,ii,jj) = rSmsg(i,j,ii,jj)
     enddo
     enddo
    enddo
    enddo
  endif

  if ( n_nrth .ne. MPI_PROC_NULL ) then
    do i = 1, Mloc
    do j = 1, Nghost
     do ii = 1, SubMainGridRatio
     do jj = 1, SubMainGridRatio
     PHI(i,Jend1+j,ii,jj) = rNmsg(i,j,ii,jj)
     enddo
     enddo
    enddo
    enddo
  endif

END SUBROUTINE phi_exch_vsubgrid

# endif

# if defined (PARALLEL)
SUBROUTINE phi_int_exch_1 (PHI)
  USE PARAM
  USE GLOBAL
  IMPLICIT NONE
  INTEGER,INTENT(INOUT) :: PHI(Mloc,Nloc)

  INTEGER,DIMENSION(MPI_STATUS_SIZE,4) :: status
  INTEGER,DIMENSION(4) :: req
  INTEGER :: nreq,len
  INTEGER,DIMENSION(Mloc,Nghost) :: rNmsg, sNmsg,rSmsg,sSmsg
  INTEGER,DIMENSION(Nloc,Nghost) :: rWmsg, sWmsg,rEmsg,sEmsg

! for east-west

  len = Nloc * Nghost

  nreq = 0
  if ( n_west .ne. MPI_PROC_NULL ) then
    nreq = nreq + 1
    call MPI_IRECV( rWmsg, len, MPI_SP, &
      n_west, 0, comm2d, req(nreq), ier )
    do j = 1, Nloc
    do i = 1, Nghost
     sWmsg(j,i) = PHI(Ibeg+i-1,j)
    enddo
    enddo
    nreq = nreq +1
    call MPI_ISEND( sWmsg, len, MPI_SP, &
      n_west, 1, comm2d, req(nreq), ier )
  endif

  if ( n_east .ne. MPI_PROC_NULL ) then
    nreq = nreq + 1
    call MPI_IRECV( rEmsg, len, MPI_SP, &
      n_east, 1, comm2d, req(nreq), ier )
    do j = 1, Nloc
    do i = 1, Nghost
     sEmsg(j,i) = PHI(Iend-i+1,j)
    enddo
    enddo
    nreq = nreq +1
    call MPI_ISEND( sEmsg, len, MPI_SP, &
      n_east, 0, comm2d, req(nreq), ier )
  endif

  call MPI_WAITALL( nreq, req, status, ier )

  if ( n_west .ne. MPI_PROC_NULL ) then
    do j = 1, Nloc
    do i = 1, Nghost
     PHI(Ibeg-i,j) = rWmsg(j,i)
    enddo
    enddo
  endif

  if ( n_east .ne. MPI_PROC_NULL ) then
    do j = 1, Nloc
    do i = 1, Nghost
     PHI(Iend+i,j) = rEmsg(j,i)
    enddo
    enddo
  endif

! for nrth-suth

  len = Mloc * Nghost

  nreq = 0
  if ( n_suth .ne. MPI_PROC_NULL ) then
    nreq = nreq + 1
    call MPI_IRECV( rSmsg, len, MPI_SP, &
      n_suth, 0, comm2d, req(nreq), ier )
    do i = 1, Mloc
    do j = 1, Nghost
     sSmsg(i,j) = PHI(i,Jbeg+j-1)
    enddo
    enddo
    nreq = nreq +1
    call MPI_ISEND( sSmsg, len, MPI_SP, &
      n_suth, 1, comm2d, req(nreq), ier )
  endif

  if ( n_nrth .ne. MPI_PROC_NULL ) then
    nreq = nreq + 1
    call MPI_IRECV( rNmsg, len, MPI_SP, &
      n_nrth, 1, comm2d, req(nreq), ier )
    do i = 1, Mloc
    do j = 1, Nghost
     sNmsg(i,j) = PHI(i,Jend-j+1)
    enddo
    enddo
    nreq = nreq + 1
    call MPI_ISEND( sNmsg, len, MPI_SP, &
      n_nrth, 0, comm2d, req(nreq), ier )
  endif

  call MPI_WAITALL( nreq, req, status, ier )

  if ( n_suth .ne. MPI_PROC_NULL ) then
    do i = 1, Mloc
    do j = 1, Nghost
     PHI(i,Jbeg-j) = rSmsg(i,j)
    enddo
    enddo
  endif

  if ( n_nrth .ne. MPI_PROC_NULL ) then
    do i = 1, Mloc
    do j = 1, Nghost
     PHI(i,Jend+j) = rNmsg(i,j)
    enddo
    enddo
  endif

END SUBROUTINE phi_int_exch_1
# endif


# if defined (PARALLEL)
SUBROUTINE phi_int_exch_2 (PHI)
  USE PARAM
  USE GLOBAL
  IMPLICIT NONE
  INTEGER,INTENT(INOUT) :: PHI(Mloc1,Nloc)

  INTEGER,DIMENSION(MPI_STATUS_SIZE,4) :: status
  INTEGER,DIMENSION(4) :: req
  INTEGER :: nreq,len
  INTEGER,DIMENSION(Mloc1,Nghost) :: rNmsg, sNmsg,rSmsg,sSmsg
  INTEGER,DIMENSION(Nloc,Nghost) :: rWmsg, sWmsg,rEmsg,sEmsg

! for east-west

  len = Nloc * Nghost

  nreq = 0
  if ( n_west .ne. MPI_PROC_NULL ) then
    nreq = nreq + 1
    call MPI_IRECV( rWmsg, len, MPI_SP, &
      n_west, 0, comm2d, req(nreq), ier )
    do j = 1, Nloc
    do i = 1, Nghost
     sWmsg(j,i) = PHI(Ibeg+i,j)
    enddo
    enddo
    nreq = nreq +1
    call MPI_ISEND( sWmsg, len, MPI_SP, &
      n_west, 1, comm2d, req(nreq), ier )
  endif

  if ( n_east .ne. MPI_PROC_NULL ) then
    nreq = nreq + 1
    call MPI_IRECV( rEmsg, len, MPI_SP, &
      n_east, 1, comm2d, req(nreq), ier )
    do j = 1, Nloc
    do i = 1, Nghost
     sEmsg(j,i) = PHI(Iend1-i,j)
    enddo
    enddo
    nreq = nreq +1
    call MPI_ISEND( sEmsg, len, MPI_SP, &
      n_east, 0, comm2d, req(nreq), ier )
  endif

  call MPI_WAITALL( nreq, req, status, ier )

  if ( n_west .ne. MPI_PROC_NULL ) then
    do j = 1, Nloc
    do i = 1, Nghost
     PHI(Ibeg-i,j) = rWmsg(j,i)
    enddo
    enddo
  endif

  if ( n_east .ne. MPI_PROC_NULL ) then
    do j = 1, Nloc
    do i = 1, Nghost
     PHI(Iend1+i,j) = rEmsg(j,i)
    enddo
    enddo
  endif

! for nrth-suth

  len = Mloc1 * Nghost

  nreq = 0
  if ( n_suth .ne. MPI_PROC_NULL ) then
    nreq = nreq + 1
    call MPI_IRECV( rSmsg, len, MPI_SP, &
      n_suth, 0, comm2d, req(nreq), ier )
    do i = 1, Mloc1
    do j = 1, Nghost
     sSmsg(i,j) = PHI(i,Jbeg+j-1)
    enddo
    enddo
    nreq = nreq +1
    call MPI_ISEND( sSmsg, len, MPI_SP, &
      n_suth, 1, comm2d, req(nreq), ier )
  endif

  if ( n_nrth .ne. MPI_PROC_NULL ) then
    nreq = nreq + 1
    call MPI_IRECV( rNmsg, len, MPI_SP, &
      n_nrth, 1, comm2d, req(nreq), ier )
    do i = 1, Mloc1
    do j = 1, Nghost
     sNmsg(i,j) = PHI(i,Jend-j+1)
    enddo
    enddo
    nreq = nreq + 1
    call MPI_ISEND( sNmsg, len, MPI_SP, &
      n_nrth, 0, comm2d, req(nreq), ier )
  endif

  call MPI_WAITALL( nreq, req, status, ier )

  if ( n_suth .ne. MPI_PROC_NULL ) then
    do i = 1, Mloc1
    do j = 1, Nghost
     PHI(i,Jbeg-j) = rSmsg(i,j)
    enddo
    enddo
  endif

  if ( n_nrth .ne. MPI_PROC_NULL ) then
    do i = 1, Mloc1
    do j = 1, Nghost
     PHI(i,Jend+j) = rNmsg(i,j)
    enddo
    enddo
  endif

END SUBROUTINE phi_int_exch_2
# endif

# if defined (PARALLEL)
SUBROUTINE phi_int_exch_3 (PHI)
  USE PARAM
  USE GLOBAL
  IMPLICIT NONE
  INTEGER,INTENT(INOUT) :: PHI(Mloc,Nloc1)

  INTEGER,DIMENSION(MPI_STATUS_SIZE,4) :: status
  INTEGER,DIMENSION(4) :: req
  INTEGER :: nreq,len
  INTEGER,DIMENSION(Mloc,Nghost) :: rNmsg, sNmsg,rSmsg,sSmsg
  INTEGER,DIMENSION(Nloc1,Nghost) :: rWmsg, sWmsg,rEmsg,sEmsg

! for east-west

  len = Nloc1 * Nghost

  nreq = 0
  if ( n_west .ne. MPI_PROC_NULL ) then
    nreq = nreq + 1
    call MPI_IRECV( rWmsg, len, MPI_SP, &
      n_west, 0, comm2d, req(nreq), ier )
    do j = 1, Nloc1
    do i = 1, Nghost
     sWmsg(j,i) = PHI(Ibeg+i-1,j)
    enddo
    enddo
    nreq = nreq +1
    call MPI_ISEND( sWmsg, len, MPI_SP, &
      n_west, 1, comm2d, req(nreq), ier )
  endif

  if ( n_east .ne. MPI_PROC_NULL ) then
    nreq = nreq + 1
    call MPI_IRECV( rEmsg, len, MPI_SP, &
      n_east, 1, comm2d, req(nreq), ier )
    do j = 1, Nloc1
    do i = 1, Nghost
     sEmsg(j,i) = PHI(Iend-i+1,j)
    enddo
    enddo
    nreq = nreq +1
    call MPI_ISEND( sEmsg, len, MPI_SP, &
      n_east, 0, comm2d, req(nreq), ier )
  endif

  call MPI_WAITALL( nreq, req, status, ier )

  if ( n_west .ne. MPI_PROC_NULL ) then
    do j = 1, Nloc1
    do i = 1, Nghost
     PHI(Ibeg-i,j) = rWmsg(j,i)
    enddo
    enddo
  endif

  if ( n_east .ne. MPI_PROC_NULL ) then
    do j = 1, Nloc1
    do i = 1, Nghost
     PHI(Iend+i,j) = rEmsg(j,i)
    enddo
    enddo
  endif

! for nrth-suth

  len = Mloc * Nghost

  nreq = 0
  if ( n_suth .ne. MPI_PROC_NULL ) then
    nreq = nreq + 1
    call MPI_IRECV( rSmsg, len, MPI_SP, &
      n_suth, 0, comm2d, req(nreq), ier )
    do i = 1, Mloc
    do j = 1, Nghost
     sSmsg(i,j) = PHI(i,Jbeg+j)
    enddo
    enddo
    nreq = nreq +1
    call MPI_ISEND( sSmsg, len, MPI_SP, &
      n_suth, 1, comm2d, req(nreq), ier )
  endif

  if ( n_nrth .ne. MPI_PROC_NULL ) then
    nreq = nreq + 1
    call MPI_IRECV( rNmsg, len, MPI_SP, &
      n_nrth, 1, comm2d, req(nreq), ier )
    do i = 1, Mloc
    do j = 1, Nghost
     sNmsg(i,j) = PHI(i,Jend1-j)
    enddo
    enddo
    nreq = nreq + 1
    call MPI_ISEND( sNmsg, len, MPI_SP, &
      n_nrth, 0, comm2d, req(nreq), ier )
  endif

  call MPI_WAITALL( nreq, req, status, ier )

  if ( n_suth .ne. MPI_PROC_NULL ) then
    do i = 1, Mloc
    do j = 1, Nghost
     PHI(i,Jbeg-j) = rSmsg(i,j)
    enddo
    enddo
  endif

  if ( n_nrth .ne. MPI_PROC_NULL ) then
    do i = 1, Mloc
    do j = 1, Nghost
     PHI(i,Jend1+j) = rNmsg(i,j)
    enddo
    enddo
  endif

END SUBROUTINE phi_int_exch_3
# endif
