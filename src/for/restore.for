C
C SUBROUTINE RESTORE.FOR
C  
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]RESTORE.FOV                                  $
C  $Date::   18 Dec 1996 12:01:40                                         $
C  $Revision::   1.1                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 01-FEB-89 XXX INITIAL RELEASE FOR SWEDEN
C
C
C     SUBROUTINE TO RESTORE THE STATE OF THE SYSTEM FROM THE CHECKPO
C
C     IN -  REPSER: REPROCESSING STARTS FROM THE FIRST YOUNGER THAN
C           THAN REPSER CHECKPOINT
C     OUT-  RET: RETURN CODE - 0 - OK
C                              1 - CHEKPOINT FILE ERROR
C                              2 - INCONSISTENCY BETWEEN REPSER AND
C                                  CHECKP SER (TOO OLD)
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE RESTORE(REPSER,RET)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:LOGCOM.DEF'
	INCLUDE 'INCLIB:POOLLTO.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
C
	INTEGER*4 LSTTYP, LSTTIM, STTS, SER, EOF
	INTEGER*4 CERR, DATE, ST, RET, REPSER
C
	INTEGER*4 SAVE_POLSER
	INTEGER*4 SAVE_COMSER
C
C
        INTEGER*4   CHKTAB64(64,CNTSEC+SCMSEC+DAYSEC)
        INTEGER*4   CHKTAB(2048,CONCSB)         !
        EQUIVALENCE(CHKTAB(1,1),CHKTAB64(1,1))
C
        COMMON /CHKTAB/CHKTAB
C
	INTEGER*4   CONBLK64(64,CNTSEC+SCMSEC+DAYSEC)
	EQUIVALENCE(CONBLK(1,1),CONBLK64(1,1))

	INTEGER*4 LBUF(LREC*3)
C
C     MAKE SURE EVERYTHING IS FLUSHED
C
	P(NETFLU)=FLUREQ
1	CONTINUE
	CALL XWAIT(50,1,ST)
	IF(P(NETFLU).NE.FLUSHED) GOTO 1    !NOT YET
C
5	CONTINUE
	IF(P(CHKFLG).NE.0.OR.LCHKPNT.NE.0)THEN
	   CALL XWAIT(50,1,ST)
	   GOTO 5
	ENDIF
C
	DATE=DAYCDC
D	TYPE *,'STARTING RESTORE -->  AFTER P(NETFLU) DONE'
C
	CALL RESCHK(DATE,REPSER,CERR)
D	TYPE *,'DONE RESCHK -->  AFTER RESCHK(DATE,REPSER,CERR) DONE'
	IF (CERR.NE.0) THEN
	   RET=1
	   GOTO 9000
	ENDIF
C
	IF (NXTSER.GT.REPSER) THEN
	   RET=2
	   GOTO 9000
	ENDIF
C
C SELECTIVE SAVE/RESTORE OF CONCOM
C
	SAVE_POLSER = P(POLSER)
	SAVE_COMSER = P(COMSER)
C
	CALL FASTMOV(CHKTAB64(MAXTSK+2,1),
     *               CONBLK64(MAXTSK+2,1),
     *              (CNTSEC*64)-(MAXTSK+2))
C
	CALL FASTMOV(CHKTAB64(SUPWAG,CNTSEC+1),
     *               CONBLK64(SUPWAG,CNTSEC+1),
     *             ((SCMSEC+DAYSEC)*64)-SUPWAG)
C
	P(POLSER) = SAVE_POLSER
	P(COMSER) = SAVE_COMSER
C
C ACTUAL REPROCESSING
C
	EOF=0
C
	SER=NXTSER
	IF(SER.EQ.REPSER) GOTO 220
C
C READ NEXT TRANSACTION FROM LOG FILE
C
200	CONTINUE
C
	IF(SER.GE.REPSER) GOTO 210
C
	CALL RLOG(SER,LBUF,NTM,STTS)
	IF(STTS.LT.0) THEN
	  SER=SER+1
	   NXTSER=SER
C
	  GOTO 200
	ENDIF
C
	CALL REPROTRA(LBUF,LSTTIM,LSTTYP,EOF)
C
C CHECK FOR END OF FILE
C
	IF(EOF.NE.0) THEN
	  IF(EOF.GT.5000) GOTO 210
	  SER=SER+1
	ELSE
	  SER=SER+1
	  NXTSER=SER
	ENDIF
	GOTO 200
C
C     NO ERROR EXIT
C
210	CONTINUE
	P(REPFLG)=1
C
220	CONTINUE
	HRSER=REPSER-1
	HSER=HRSER
	HBLOCK=(HRSER-1)/LBLK +1
	HBLKRDY=HBLOCK-1
	RET=0
C
9000	CONTINUE
	P(CMDFRZ)=0
	P(NETFLU)=NOFLU
D	TYPE *,'RESTORE  9000: ',P(CMDFRZ), P(NETFLU)
C
C
	RETURN
	END
