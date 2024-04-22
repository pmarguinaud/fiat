! (C) Copyright 2005- ECMWF.
! (C) Copyright 2013- Meteo-France.
! 
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!

MODULE MPL_SEND_MOD

!**** MPL_SEND Send a message

!     Purpose.
!     --------
!     Send a message to a named source from a buffer.
!     The data may be REAL*4, REAL*8,or INTEGER, one dimensional array
!                     REAL*4,or REAL*8, two dimensional array
!                  or INTEGER scalar

!**   Interface.
!     ----------
!        CALL MPL_SEND

!        Input required arguments :
!        -------------------------
!           PBUF     -  buffer containing message
!                       (can be type REAL*4, REAL*8 or INTEGER)
!           KTAG     -  message tag
!           KDEST    -  rank of process to receive the message

!        Input optional arguments :
!        -------------------------
!           KCOMM    -  Communicator number if different from MPI_COMM_WORLD 
!                       or from that established as the default 
!                       by an MPL communicator routine
!           KMP_TYPE -  buffering type (see MPL_BUFFER_METHOD)
!                       overrides value provided to MPL_BUFFER_METHOD
!           CDSTRING -  Character string for ABORT messages
!                       used when KERROR is not provided

!        Output required arguments :
!        -------------------------
!           none

!        Output optional arguments :
!        -------------------------
!           KREQUEST -  Communication request
!                       required when buffering type is non-blocking
!           KERROR   -  return error code.     If not supplied, 
!                       MPL_SEND aborts when an error is detected.
!     Author.
!     -------
!        D.Dent, M.Hamrud     ECMWF

!     Modifications.
!     --------------
!        Original: 2000-09-01
!        P. Marguinaud : 01-Jan-2011 : Do not raise an error when
!                        the numproc is beyond model limits and KCOMM is passed
!                        as argument
!      F. Vana  05-Mar-2015  Support for single precision
!     ------------------------------------------------------------------

USE EC_PARKIND , ONLY : JPRD, JPIM, JPIB, JPRM
USE OML_MOD   ,ONLY : OML_MY_THREAD

USE MPL_MPIF
USE MPL_DATA_MODULE
USE MPL_MESSAGE_MOD
USE MPL_NPROC_MOD
USE MPL_STATS_MOD
USE YOMMPLSTATS

IMPLICIT NONE

#define SUBNAME(NAME) NAME
#include "mpl_send_mod.intf.h"
#undef SUBNAME

#ifdef _CUDA
#define SUBNAME(NAME) NAME##_DEVICE
#include "mpl_send_mod.intf.h"
#undef SUBNAME
#endif


PRIVATE

!---Moved into subroutines to keep threadsafe----
! INTEGER(KIND=JPIM) :: ICOUNT,IMP_TYPE,ICOMM,IERROR
! LOGICAL :: LLABORT=.TRUE.

PUBLIC MPL_SEND

CONTAINS

#define SUBNAME(NAME) NAME
#define _ATTR_ 
#include "mpl_send_mod.body.h"
#undef SUBNAME
#undef _ATTR_

#ifdef _CUDA
#define SUBNAME(NAME) NAME##_DEVICE
#define _ATTR_ ,DEVICE
#include "mpl_send_mod.body.h"
#undef SUBNAME
#undef _ATTR_
#endif

END MODULE MPL_SEND_MOD
