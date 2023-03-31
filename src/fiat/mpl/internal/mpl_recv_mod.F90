! (C) Copyright 2005- ECMWF.
! (C) Copyright 2013- Meteo-France.
! 
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!

MODULE MPL_RECV_MOD

!**** MPL_RECV Receive a message

!     Purpose.
!     --------
!     Receive a message from a named source into a buffer.
!     The data may be REAL*4, REAL*8,or INTEGER, one dimensional array
!                     REAL*4,or REAL*8, two dimensional array
!                  or REAL or INTEGER scalar

!**   Interface.
!     ----------
!        CALL MPL_RECV

!        Input required arguments :
!        -------------------------
!           PBUF     -  buffer to receive the message
!                       (can be type REAL*4, REAL*8 or INTEGER)

!        Input optional arguments :
!        -------------------------
!           KTAG     -  message tag
!           KCOMM    -  Communicator number if different from MPI_COMM_WORLD 
!           KMP_TYPE -  buffering type (see MPL_BUFFER_METHOD)
!                       overrides value provided to MPL_BUFFER_METHOD
!           KSOURCE  -  rank of process sending the message
!                       default is MPI_ANY_SOURCE
!           CDSTRING -  Character string for ABORT messages
!                       used when KERROR is not provided

!        Output required arguments :
!        -------------------------
!           none

!        Output optional arguments :
!        -------------------------
!           KREQUEST -  Communication request
!                       required when buffering type is non-blocking
!           KFROM    -  rank of process sending the message
!           KRECVTAG -  tag of received message
!           KOUNT    -  number of items in received message
!           KERROR   -  return error code.     If not supplied, 
!                       MPL_RECV aborts when an error is detected.
!     Author.
!     -------
!        D.Dent, M.Hamrud     ECMWF

!     Modifications.
!     --------------
!        Original: 2000-09-01
!      F. Vana  05-Mar-2015  Support for single precision

!     ------------------------------------------------------------------

USE EC_PARKIND , ONLY : JPRD, JPIB, JPIM, JPRM
USE OML_MOD   ,ONLY : OML_MY_THREAD

USE MPL_MPIF
USE MPL_DATA_MODULE
USE MPL_STATS_MOD
USE YOMMPLSTATS
USE MPL_MESSAGE_MOD
USE MPL_NPROC_MOD

IMPLICIT NONE

#define SUBNAME(NAME) NAME
#include "mpl_recv_mod.intf.h"
#undef SUBNAME

#ifdef _CUDA
#define SUBNAME(NAME) NAME##_DEVICE
#include "mpl_recv_mod.intf.h"
#undef SUBNAME
#endif

PRIVATE

PUBLIC MPL_RECV

CONTAINS

!     ------------------------------------------------------------------

SUBROUTINE MPL_RECV_PREAMB(KMP_TYPER,KCOMMR,KSOURCER,KTAGR,KMP_TYPE,KCOMM,KSOURCE,KTAG,KREQUEST)
INTEGER(KIND=JPIM),INTENT(OUT) :: KMP_TYPER,KCOMMR,KSOURCER,KTAGR
INTEGER(KIND=JPIM),INTENT(IN),OPTIONAL :: KMP_TYPE,KCOMM,KSOURCE,KTAG
INTEGER(KIND=JPIM),OPTIONAL :: KREQUEST

LOGICAL :: LLABORT=.TRUE.
INTEGER(KIND=JPIM) :: ITID

ITID = OML_MY_THREAD()

IF(MPL_NUMPROC < 1) CALL MPL_MESSAGE(&
  & CDMESSAGE='MPL_RECV: MPL NOT INITIALISED ',LDABORT=LLABORT) 

IF(PRESENT(KMP_TYPE)) THEN
  KMP_TYPER=KMP_TYPE
ELSE
  KMP_TYPER=MPL_METHOD
ENDIF

IF(KMP_TYPER == JP_NON_BLOCKING_STANDARD) THEN
  IF( .NOT. PRESENT(KREQUEST)) THEN
    CALL MPL_MESSAGE(CDMESSAGE='MPL_RECV:KREQUEST MISSING ',LDABORT=LLABORT)
  ENDIF
ENDIF

IF(PRESENT(KCOMM)) THEN
  KCOMMR=KCOMM
ELSE
  KCOMMR=MPL_COMM_OML(ITID)
ENDIF

IF(PRESENT(KSOURCE)) THEN
  IF(KSOURCE < 1 .OR. KSOURCE >MPL_NPROC(KCOMMR)) THEN
    WRITE(MPL_ERRUNIT,*)'MPL_RECV: ERROR KSOURCE=',KSOURCE
    CALL MPL_MESSAGE(CDMESSAGE='MPL_RECV:ILLEGAL KSOURCE ',LDABORT=LLABORT)
  ENDIF
  KSOURCER=KSOURCE-1
ELSE
  KSOURCER=MPI_ANY_SOURCE
ENDIF

IF(PRESENT(KTAG)) THEN
  KTAGR=KTAG
ELSE
  KTAGR=MPI_ANY_TAG
ENDIF

END SUBROUTINE MPL_RECV_PREAMB

!     ------------------------------------------------------------------

SUBROUTINE MPL_RECV_TAIL(KRECV_STATUS,KTYPE,KFROM,KOUNT,KRECVTAG,KERROR,CDSTRING)


#ifdef USE_8_BYTE_WORDS
  USE MPI4TO8, ONLY : &
    MPI_RECV => MPI_RECV8, MPI_GET_COUNT => MPI_GET_COUNT8
#endif

INTEGER(KIND=JPIM),INTENT(IN) :: KRECV_STATUS(MPI_STATUS_SIZE)
INTEGER(KIND=JPIM),INTENT(IN) :: KTYPE
INTEGER(KIND=JPIM),INTENT(OUT),OPTIONAL :: KERROR,KFROM,KRECVTAG,KOUNT
CHARACTER*(*),INTENT(IN),OPTIONAL :: CDSTRING

INTEGER(KIND=JPIM) :: IERROR
INTEGER(KIND=JPIM) :: IFROM,IRECVTAG,IRECVCOUNT
LOGICAL :: LLABORT=.TRUE.

IFROM=KRECV_STATUS(MPI_SOURCE)+1
IF(PRESENT(KFROM)) THEN
  KFROM=IFROM
ENDIF
CALL MPI_GET_COUNT(KRECV_STATUS,KTYPE,IRECVCOUNT,IERROR)
IF(PRESENT(KOUNT)) THEN
  KOUNT=IRECVCOUNT
ENDIF
IF(LMPLSTATS) CALL MPL_RECVSTATS(IRECVCOUNT,KTYPE)
IRECVTAG=KRECV_STATUS(MPI_TAG)
IF(PRESENT(KRECVTAG)) THEN
  KRECVTAG=IRECVTAG
ENDIF
!IF(MPL_OUTPUT > 1 )THEN
!  WRITE(MPL_UNIT,'(A,5I8)') ' MPL_RECV ',IRECVCOUNT,IMP_TYPE,IFROM,IRECVTAG,ICOMM
!ENDIF
IF(PRESENT(KERROR)) THEN
  KERROR=IERROR
ELSE
  IF(IERROR /= 0 ) CALL MPL_MESSAGE(IERROR,'MPL_RECV',CDSTRING,LDABORT=LLABORT)
ENDIF

END SUBROUTINE MPL_RECV_TAIL

#define SUBNAME(NAME) NAME
#define _ATTR_ 
#include "mpl_recv_mod.body.h"
#undef SUBNAME
#undef _ATTR_

#ifdef _CUDA
#define SUBNAME(NAME) NAME##_DEVICE
#define _ATTR_ ,DEVICE
#include "mpl_recv_mod.body.h"
#undef SUBNAME
#undef _ATTR_
#endif

END MODULE MPL_RECV_MOD
