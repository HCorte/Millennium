C
C SUBROUTINE NET_CHKQUEUE
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]NET_CHKQUEUE.FOV                             $
C  $Date::   17 Apr 1996 14:10:58                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C *** Pre-Baseline Source - chkq.for ***
C
C V02 09-MAR-2001 UXN QUEUE checking re-written.
C V01 11-SEP-1990 MRM RELEASED FOR VAX
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode Island,
C and contains confidential and trade secret information. It may not be
C transferred from the custody or control of GTECH except as authorized in
C writing by an officer of GTECH. Neither this item nor the information it
C contains may be used, transferred, reproduced, published, or disclosed,
C in whole or in part, and directly or indirectly, except as expressly
C authorized by an officer of GTECH, pursuant to written agreement.
C
C Copyright 1994 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Purpose:
C     WILL CHECK ANY QUEUE IF DATA IS ON THE LIST
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE NET_CHKQUEUE(BUF, QUEUE, TIMES)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
C
C LOCAL DECLARATIONS
C
	INTEGER*4	BUF,
     *			CHECK_BUF,
     *			QUEUE(*),
     *			TIMES
C
C COMMON DECLARATIONS
C
	COMMON /TEST_CHECK/ CHECK_BUF
C
	CALL CHKQCOR(QUEUE, BUF, TIMES)
	NET_LAST_BUFFER_USER(BUF) = CHECK_BUF
C
	END
