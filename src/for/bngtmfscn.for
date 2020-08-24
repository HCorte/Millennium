C  GXSRC:BNGTMFSCN.FOR
C  
C V08 18-MAR-1999 RXK Game type/game index change.
C     Rev 1.5   27 Jan 1995 19:21:48   HXK
C  Fix for incorrect game status check
C     Rev 1.4   12 Jan 1995 10:58:40   PXB
C  Cannot be run if game is open.
C     Rev 1.3   13 Dec 1994 15:42:10   JXP
C  Write first good seed to temporary file 
C     Rev 1.2   11 Dec 1994 14:51:06   HXK
C  Call BNGREADTMF instead of READTMF
C     Rev 1.1   09 Dec 1994 12:49:22   JXP
C  Applying PVCS header for automatic revision history
C     Rev 1.0    09 Dec 1994 12:47:56   JXP
C  Initial revision.
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        PROGRAM BNGTMFSCN
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:RESCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:BNGCOM.DEF'
C
        INTEGER*4 LOGREC(LMUREC)	 !Log record buffer
        INTEGER*4 SER			 !Serial to read from TM
	INTEGER*4 ST			 !Subroutine return status
	INTEGER*4 GNUM			 !Game # we want to run on
	INTEGER*4 GAMN			 !Game # we read from TM
	INTEGER*4 GIND                   !Game index
	INTEGER*4 STAT		         !TM Record Status
	INTEGER*4 TYPE			 !TM Transaction Type
	INTEGER*4 S
        INTEGER*4  FDB(7)
        INTEGER*4  AMTBET,FIRST_SEED,LAST_SEED,CUR_SEED,BYTLEN,FLUN,J
        INTEGER*4  MAX_NO
        INTEGER*4  SALAMT,CANCNT,VALCNT
        INTEGER*4  FILENAME(5) /'FILE',':TMF','TMP.','FTP ','    '/
        LOGICAL    FIRST/.TRUE./

        LOGICAL   EOF/.FALSE./		 !End of file flag
C
	INTEGER*4 I4TEMP		 !Temporarily variables.
	INTEGER*2 I2TEMP(2)
	BYTE      I1TEMP(4)
	EQUIVALENCE(I4TEMP,I2TEMP,I1TEMP)
        DATA      MAX_NO/2147483647/
C
        INTEGER*4 SCFNAM(5)
        DATA SCFNAM/'SCF.','FIL ',3*'    '/
C
	CALL COPYRITE
	TYPE *, IAM()
	TYPE *, IAM(),
     *		      '<<<<< TFTPSCAN Bingo TMF seeds & cancels Scan V01 >>>>>'
	TYPE *, IAM()
C
C SET / CLEAR VARIABLES
C
        SALAMT=0
        CANCNT=0
        VALCNT=0
	SER = 0

C READ SCF RECORD
C
        CALL OPENW(1,SCFNAM,4,0,0,ST)
        CALL IOINIT(FDB,1,SCFSEC*256)
        IF(ST.NE.0) CALL FILERR(SCFNAM,1,ST,0)
        CALL READW(FDB,1,SCFREC,ST)
        IF(ST.NE.0) CALL FILERR(SCFNAM,2,ST,1)
        CALL CLOSEFIL(FDB)

        GNUM = SCFGTN(TBNG,1)
        GIND = 1

	IF (BNGSTS(GIND).LE.GAMOPN) then
	    TYPE*,IAM(),'WARNING Game is still open'
	    CALL GPAUSE
	ENDIF

C
C OPEN FTP-FILE
C
        BYTLEN=85
        FLUN=2
        CALL FTP_OPEN(FILENAME,FLUN,BYTLEN,ST)
        IF(ST.NE.0) CALL FILERR(FILENAME,1,ST,0)
C
C OPEN TM-FILE
C
	CALL OPENW(PTMF,SFNAMES(1,PTMF),4,0,0,ST)
	IF(ST.NE.0) CALL FILERR(SFNAMES(1,PTMF),1,ST,0)
	CALL TOPEN(PTMF)
C
C LOOP THROUGH TM-FILE
C
1000	CONTINUE
	CALL BNGREADTMF(LOGREC,SER,EOF)
	IF(EOF) GOTO 2000

	IF(MOD(SER,50000).EQ.0) TYPE*,IAM(),'Processing serial ',SER
C
C GET GAME NUMBER, STATUS AND TYPE
C
	I4TEMP = LOGREC(5)
	S = ZEXT(I1TEMP(4))
        GAMN = IAND (S,'7F'X ) 
C
	I4TEMP = LOGREC(6)
	STAT = ZEXT(I1TEMP(1))
	TYPE = ISHFT(ZEXT(I1TEMP(2)),-4)
C
	AMTBET = LOGREC(9)
        CUR_SEED = LOGREC(25)
C
C IF WE ARE N0T DOING WHAT WE WANT TO, READ NEXT TRANSACTION
C
	IF(GNUM.NE.GAMN.OR.STAT.NE.GOOD.OR.TYPE.NE.TWAG) GOTO 1000
C

        IF(FIRST) THEN
            FIRST=.FALSE.
            FIRST_SEED=CUR_SEED
            LAST_SEED=CUR_SEED-1
	    WRITE(FLUN,930) FIRST_SEED
        ENDIF
C
	IF(LAST_SEED.EQ.CUR_SEED) GOTO 1000
C
	IF(LAST_SEED.NE.CUR_SEED-1) THEN
	    IF(LAST_SEED.LT.CUR_SEED-1) THEN
                DO J=LAST_SEED+1,CUR_SEED-1
                    WRITE(FLUN,930) J
                    CANCNT=CANCNT+1
                ENDDO
            ELSE
                DO J=LAST_SEED+1,MAX_NO
                    WRITE(FLUN,930) J
                    CANCNT=CANCNT+1
                ENDDO
                DO J=0,CUR_SEED-1
                    WRITE(FLUN,930) J
                    CANCNT=CANCNT+1
                ENDDO
            ENDIF
        ENDIF
C
        LAST_SEED=CUR_SEED
        VALCNT=VALCNT+1
        SALAMT=SALAMT+AMTBET
C
C GET ANOTHER TRANSACTION
C
	GOTO 1000
C
C END OF FILE
C
2000	CONTINUE
	CALL USRCLOS1(PTMF)
	SALAMT=(SALAMT*5)/100
        WRITE(FLUN,940) SALAMT,VALCNT,CANCNT,FIRST_SEED,LAST_SEED
        CALL USRCLOS1(FLUN)
 	CALL GSTOP(GEXIT_SUCCESS)
930     FORMAT(I20)
940     FORMAT('TOTAL',5(I20))
	END	
