C SCANRETR.FOR
C
C V05 08-SEP-2000 OXK Global 161 update: scan over different days
C V04 07-Aug-2000 OXK Modifications for non-interactive use
C V03 03-Aug-2000 OXK NRM_TITLE added here to avoid output conversion errors
C V02 03-Aug-2000 OXK Localized for Finland
C V01 31-Jul-2000 XXX Initial release as recieved form Global RFSS 161
C
C FIND REJECTED RETRY TICKETS
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM SCANDAYS
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:DESLOG.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
C
	INTEGER*4   ST, SER, COPY

	INTEGER*4   TMFLUN  /1/
	INTEGER*4   DATLUN  /7/	    ! USED FOR BOTH RETRY.FIL AND RETR____.REP

	INTEGER*4   LBUF(LREC*3)
	LOGICAL	    EOT,FIRST

	INTEGER*4 K
C
C SOME INITIALIZATIONS ETC.
C
	CALL COPYRITE

        CALL GETSCONF(SCFREC,ST)
        IF (ST.NE.0) CALL GSTOP(GEXIT_FATAL)

	COPY = 0
	FIRST = .TRUE.
C
C OPEN TMF
C
	WRITE (6,800) IAM(),(SCFSFN(K,PTMF),K=1,5)
	CALL OPENW(TMFLUN, SCFSFN(1,PTMF), 4, 0, 0, ST)
	CALL TOPEN(TMFLUN)
	IF (ST .NE. 0) THEN
	    WRITE(6,820) IAM(),(SCFSFN(K,PTMF),K=1,5), ST
	    CALL GSTOP(GEXIT_FATAL)
	ENDIF
C
C SCAN TMF
C
40    CONTINUE
	CALL FASTSET(0, LBUF, LREC*3)
	CALL READTMF(LBUF, SER, EOT)
	IF (EOT) GOTO 1000
C
	CALL LOGTRA(TRABUF,LBUF)

	IF(FIRST) THEN
	    FIRST = .FALSE.
	    CALL INTRETRY(DATLUN,TRABUF(TCDC),ST)
	    IF (ST.NE.0) CALL GSTOP(GEXIT_FATAL)
	ENDIF
C
	CALL CHKRETRY(DATLUN,TRABUF)
	GOTO 40
C
1000  CONTINUE
	CALL UPDRETRY(DATLUN,COPY,ST)

	CALL USRCLOS1(TMFLUN)
	CALL USRCLOS1(DATLUN)
	CALL GSTOP(GEXIT_SUCCESS)
C
800	FORMAT(1X,A,'Scanning primary TMF : ',5A4)
820	FORMAT(1X,A, 5A4, ' open error > ', Z8, ' hex ')
	END
