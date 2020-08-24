C
C SUBROUTINE IOPEN
C $Log:   GXAFXT:[GOLS]IOPEN.FOV  $
C  
C
C V09 02-MAY-11 RXK Use of GETSIZ_USED replaced with use of GETSIZ & VLFROUNDING
C     Rev 1.0   17 Apr 1996 13:39:58   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:41:40   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_hshfili4.for **
C
C HSHFILI4.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V05 17-APR-89 MTK  REPLACE CLOSE WITH CLOSEFIL(FDB)
C B04 10-JUN-88 TKO  USE CLOSE(UNIT=LUN) (NOT CALL CLOSE(LUN))
C B03 11-MAR-88 TKO  SET UP TUB SIZE (DEFAULT TO TUB = 1 BUCKET)
C                    ADDED NEW ROUTINE ITUBSIZE
C B02 01-JUL-87 TKO  CLEAR BIGBUF WHEN IINIB CALLED
C B01 25-MAY-86 TKO  INITIAL RELEASE
C
C This is the main routine for hashed file accessing.  It is simply
C a re-write of Walter Szrek's INDFIL, with an attempt to speed it
C up a bit.  The general flow is similar to INDFIL.
C
C **** RECORD LENGTHS MUST BE A MULTIPLE OF 4 BYTES ****
C
C
C
C
C
C
C *** IOPEN
C
C This is the only way to open an indirect file
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
	SUBROUTINE IOPEN(FILNAM,LUN,I2RECLEN,I2KEYOFF,I4KEYOFF,STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:HSHCOM.DEF'
C
	INTEGER*4 FILNAM(*)              !  INPUT: NAME OF FILE
	INTEGER*4 LUN                    !  INPUT: LOGICAL UNIT #
	INTEGER*4 I2RECLEN               !  INPUT: I*2 RECORD LENGTH
	INTEGER*4 I2KEYOFF               !  INPUT: I*2 OFFSET OF I*2 KEY
	INTEGER*4 I4KEYOFF               !  INPUT: I*2 OFFSET OF I*4 KEY
	INTEGER*4 STATUS                 ! OUTPUT: 0=OK, ELSE ERROR CODE
C
	INTEGER*4 MAXOFF, MAXCNT, I4RECLEN, K, TOTBUC, TOTSEC, ST
C
	LOGICAL   FIRSTCALL
	DATA      FIRSTCALL/.TRUE./
C
C
C
C
C If this is the first call, set LUN # of internal buffer to 0.
C
	IF(FIRSTCALL)THEN
	  FIRSTCALL=.FALSE.
	  MYLUN=0
	ENDIF
C
C
	IF(I2RECLEN .LT. MAX(I2KEYOFF,I4KEYOFF+1))THEN
	  STATUS=ERROPN
	  GO TO 9000
	ENDIF
C
	IF(MOD(I2RECLEN,2).NE.0)THEN
	  STATUS=ERRREQ
	  GO TO 9000
	ENDIF
C
	IF(MOD(I4KEYOFF,2).NE.1)THEN      !I*4 KEY MUST BE AT I*4 OFFSET
	  STATUS=ERRREQ
	  GO TO 9000
	ENDIF
C
	IF(LUN.LE.0 .OR. LUN.GT.MAXLUN)THEN
	  STATUS=ERRUNT
	  GO TO 9000
	ENDIF
C
	IF(FCB(FCBLUN,LUN).NE.0)THEN
	  STATUS=ERROPN
	  GO TO 9000
	ENDIF
C
C open the requested file with shared read/write access
C
	CALL OPENW(LUN,FILNAM,4,0,0,ST)
	IF(ST.NE.0)THEN
	  STATUS=ST
	  GO TO 9000
	ENDIF
C
C find the size of the file in buckets
C
C 	CALL GETSIZ(LUN,TOTSEC)   !Changed to GETSIZ_USED to support 
C                                 !different Cluster Size
C        CALL GETSIZ_USED(LUN,TOTSEC)
C
 	CALL GETSIZ(LUN,TOTSEC)
        TOTSEC=(TOTSEC+VLFROUNDING-1)/VLFROUNDING*VLFROUNDING-1
	TOTBUC=TOTSEC / BUCSEC
	IF(TOTBUC.LE.0)THEN
	  STATUS=ERRERR                       !UNDEFINED ERROR
	  GO TO 9000
	ENDIF
C
C now fill in the FCB
C
	DO 1200 K=1,FCBLEN
	  FCB(K,LUN)=0
1200	CONTINUE
C
	CALL IOINIT(FCB(FCBFDB,LUN),LUN,BUCSEC*256)
C
	FCB(FCBLUN,LUN)=LUN                !LOGICAL UNIT #
	FCB(FCBHSH,LUN)=TOTBUC             !TOTAL # OF BUCKETS
	I4RECLEN=I2RECLEN/2
	FCB(FCBLN4,LUN)=I4RECLEN           !I*4 SIZE OF A RECORD
	FCB(FCBLN2,LUN)=I2RECLEN           !I*2 SIZE OF A RECORD
	FCB(FCBSZ1,LUN)=I4RECLEN           !SIZE OF 1 RECORD
	FCB(FCBSZ2,LUN)=I4RECLEN*2         !SIZE OF 2 RECORDS
	FCB(FCBSZ3,LUN)=I4RECLEN*3         !SIZE OF 3 RECORDS
	FCB(FCBSZ4,LUN)=I4RECLEN*4         !SIZE OF 4 RECORDS
C
	FCB(FCBI4K,LUN)=(I4KEYOFF-1)/2 + 1 !I*4 OFFSET OF I4KEY
	FCB(FCBI2K,LUN)=(I2KEYOFF-1)/2 + 1 !I*4 OFFSET OF I2KEY
	FCB(FCBI2H,LUN)=MOD(I2KEYOFF-1,2)+1 !   WHICH HALF (1,2)
C
	MAXCNT=I2BUCSIZ/I2RECLEN - 3       !(leave room for max size)
	FCB(FCBMAX,LUN)=MAXCNT             !MAX # OF RECORDS/BUCKET
	MAXOFF=(MAXCNT-1)*I4RECLEN         ! (offset relative to 0)
	FCB(FCBLOF,LUN)=MAXOFF             !LAST OFFSET USABLE
	FCB(FCBMSZ,LUN)=I4RECLEN*4         !MAX SIZE OF A RECORD
C
C set default tub size
C
	FCB(FCBTUBSIZ,LUN)=1               !# OF BUCKETS IN A TUB
	FCB(FCBTUBOFF,LUN)=0               !OFFSET OF BUCKET IN TUB
	FCB(FCBTUBLST,LUN)=TOTBUC+1        !LAST TUB # (MAXIMUM)
C
	STATUS=0
C
9000	CONTINUE
	RETURN
	END
