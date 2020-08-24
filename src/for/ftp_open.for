C
C SUBROUTINE FTP_OPEN
C $Log:   GXAFXT:[GOLS]FTP_OPEN.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:14:58   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   02 Sep 1994 18:04:08   HXK
C  Merge of May,June RFSS batch 
C  
C     Rev 1.0   26 Jun 1994 14:09:38   HXK
C  Initial revision.
C
C FTP_OPEN.FOR
C
C
C     FTP_OPEN.FTN
C
C     OPEN FTP FILE
C
C     SUBROUTINE FTP_OPEN(NAME,UNIT,RLEN,STATUS)
C     IN -  FILE - REPORT FILE TO ASSIGN AND OPEN
C           UNIT - FILE UNIT #
C           RLEN - RECORD LENGTH IN BYTES
C     OUT - STATUS - OPERATION STATUS
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE FTP_OPEN(I4FILE,UNIT,RLEN,STATUS)
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INTEGER*4 UNIT  !FILE UNIT #
	INTEGER*4 STATUS, RLEN
        BYTE      I4FILE(*)

        INTEGER*4   K
        INTEGER*4   EXTCNT
        INTEGER*4   XLEN
        CHARACTER*40 CXNAME

        EXTCNT = -1
        DO 1100 K = 1, 40
          IF(I4FILE(K).EQ.ICHAR(' '))GOTO 1200
          IF(I4FILE(K).EQ.ICHAR('.'))THEN
            EXTCNT = 0
            CXNAME(K:K) = '.'
            GOTO 1100
          ENDIF
          IF(.NOT. (
     *       (I4FILE(K).GE.ICHAR('A') .AND. I4FILE(K).LE.ICHAR('Z')).OR.
     *       (I4FILE(K).GE.ICHAR('a') .AND. I4FILE(K).LE.ICHAR('z')).OR.
     *       (I4FILE(K).GE.ICHAR('0') .AND. I4FILE(K).LE.ICHAR('9')).OR.
     *       (I4FILE(K).EQ.ICHAR(':'))
     *         ) ) GOTO 1200

          CXNAME(K:K) = CHAR(I4FILE(K))
          IF(EXTCNT.GE.0)THEN
            EXTCNT = EXTCNT+1
            IF(EXTCNT.GT.3)GOTO 1200
          ENDIF
1100    CONTINUE
        K = 41

1200    CONTINUE
        XLEN = MAX(K-1,1)


C TRY TO DELETE FILE 
C ------------------
        CALL DFILX(CXNAME(1:XLEN),0,0,STATUS)
        IF (STATUS.EQ.4) STATUS=0
        IF (STATUS.NE.0) RETURN

C NOW TRY TO CREATE FILE
C ----------------------
        CALL CFILX(CXNAME(1:XLEN),2,RLEN,0,4,0,0,STATUS)
        IF (STATUS.NE.0) RETURN

C ASSIGN FILE TO UNIT
C -------------------
10      CONTINUE
        CALL OPENX(UNIT, CXNAME(1:XLEN), 2, 0, 0, STATUS)
	RETURN
	END
