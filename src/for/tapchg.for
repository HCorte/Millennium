C
C SUBROUTINE TAPCHG
C $Log:   GXAFXT:[GOLS]TAPCHG.FOV  $
C  
C     Rev 1.0   17 Apr 1996 15:27:08   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:48:10   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - tapchg.for **
C
C TAPCHG.FOR
C
C V02 24-APR-91 TKO Do TAPINT after TAPOPEN
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C SUBROUTINE TO PERFORM TAPE LOGGING DEVICE
C SWITCHES.
C
C CALLING SEQUENCE
C     CALL TAPCHG(FDB,CURTAP,NEWTAP)
C INPUT
C     FDB    - FDB FOR TAPE LOGGING
C     CURTAP - CURRENT TAPE UNIT
C     NEWTAP - NEW TAPE UNIT
C
C OUTPUT
C     CURTAP - NEW CURRENT TAPE UNIT
C
C IF ANY TAPE ERRORS OCCUR ALL TAPE LOGGING IS TERMINATED
C
C
C
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode
C Island, and contains confidential and trade secret information. It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expressly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE TAPCHG(FDB,CURTAP,NEWTAP)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INTEGER*4 ST, STATUS, NEWTAP, CURTAP
	INTEGER*4 FDB(7),MESS(EDLEN)
	CHARACTER*20 TAPNAM/'MAGX:'/
C
C IF CURRENTLY LOGGING TO TAPE WRITE EOT MARK
C AND REWIND TAPE.
C
	IF(CURTAP.NE.0)THEN
	  CALL WEOT(FDB,STATUS)
	  IF(STATUS.NE.0)THEN
	    MESS(1)=TAP
	    MESS(2)=TELOG
	    MESS(3)=1
	    MESS(4)=STATUS
	    CALL QUEMES(MESS)
	    CURTAP=0
	    NEWTAP=0
	    CALL TAPCLOS(FDB,ST)
	    RETURN
	  ENDIF
C
C
	  CALL XREWIND(FDB,STATUS)
	  IF(STATUS.NE.0)THEN
	    MESS(1)=TAP
	    MESS(2)=TELOG
	    MESS(3)=1
	    MESS(4)=STATUS
	    CALL QUEMES(MESS)
	    CURTAP=0
	    NEWTAP=0
	    CALL TAPCLOS(FDB,ST)
	    RETURN
	  ENDIF
	  CALL TAPCLOS(FDB,ST)
	ENDIF
C
C LOAD NEW TAPE IF REQUESTED
C
	CURTAP=NEWTAP
	IF(NEWTAP.NE.0)THEN
	  TAPNAM(4:4)=CHAR(CURTAP+48)
	  CALL TAPOPEN(FDB,TAPNAM,ST)
	  IF(ST.NE.0) THEN
	    MESS(1)=TAP
	    MESS(2)=TELOG
	    MESS(3)=8
	    MESS(4)=CURTAP
	    MESS(5)=ST
	    CALL QUEMES(MESS)
	    CURTAP=0
	    NEWTAP=0
	    CALL TAPCLOS(FDB,ST)
	  ENDIF
	  CALL TAPINT(FDB,1,8192)
	ENDIF
	RETURN
	END
