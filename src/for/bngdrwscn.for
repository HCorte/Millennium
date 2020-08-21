C  GXSRC:BNGDRWSCN.FOR
C
C V06 15-FEB-2000 UXN P(REG_DRWPCK) added.
C V05 18-MAR-1999 RXK Game type/game index change.
C V04 20-DEC-1994 HXK Reduce draw file loop by draw date draw file
C V03 13-DEC-1994 JXP Write first good seed to temporary file
C V02 09-DEC-1994 JXP Applying PVCS header for automatic revision history
C V01 09-DEC-1994 JXP Initial revision.
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
        PROGRAM BNGDRWSCN
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:BNGCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:RECDAF.DEF'
C
        INTEGER*4 SER			 !Serial to read from TM
	INTEGER*4 ST			 !Subroutine return status
	INTEGER*4 GNUM			 !Game # we want to run on
	INTEGER*4 GAMN			 !Game # we read from TM
	INTEGER*4 STAT		         !TM Record Status
	INTEGER*4 TYPE			 !TM Transaction Type
	INTEGER*4 TMFBUF(8192)		 !Log buffers
	INTEGER*4 DFDB(7)		 !Daf , File Descriptor Block.
	INTEGER*4 TFDB(7)		 !Draw file, File Descriptor Block.
	INTEGER*4 BLOCK			 !Block # to read from Draw File
	INTEGER*4 IND			 !Logbufer within TMFBUF.
	INTEGER*4 INPLEN		 !Input length (PTMTEXT)
	INTEGER*4 CDC			 !CDC Loop Variable for DAF Reads.
	INTEGER*4 FILCNT		 !Draw File Count to Read.
	INTEGER*4 FTYPE(200)		 !File Type we read.
	INTEGER*4 EOFCNT		 !End of File Count.
	INTEGER*4 LENGTH		 !Current Record Length in TM
  	INTEGER*4 I,S			 !Loop Variables
        INTEGER*4 FDB(7)
        INTEGER*4 AMTBET,FIRST_SEED,LAST_SEED,CUR_SEED,BYTLEN,FLUN,J
	INTEGER*4 GIND,DRAW                    
        INTEGER*4 SALAMT,CANCNT,VALCNT,MAX_NO
        INTEGER*4 FILENAME(5) /'FILE',':DRW','TMP.','FTP ','    '/
        LOGICAL   FIRST/.TRUE./
C
	INTEGER*4 FILES(5,200)
	CHARACTER*20 CFILES(200)
	EQUIVALENCE(FILES,CFILES)
	DATA	  MAX_NO/2147483647/
C
	INTEGER*4 BVOL			 !Bingo Draw Files Volume Name.
	CHARACTER*4  CXBVOL
	EQUIVALENCE(BVOL,CXBVOL)
C
	LOGICAL	  EXFIL			 !Does Draw file Exist?
	LOGICAL	  SCAN/.TRUE./		 !Should we or should we not?
C
	INTEGER*4 I4TEMP		 !Temporarily variables.
	INTEGER*2 I2TEMP(2)
	BYTE      I1TEMP(4)
	EQUIVALENCE(I4TEMP,I2TEMP,I1TEMP)
C
C
C
	CALL COPYRITE
	TYPE *, IAM()
	TYPE *, IAM(),
     *	 '<<<<< DFTPSCAN Bingo Draw file seeds & cancels Scan V01 >>>>>'
	TYPE *, IAM()
C
C SET / CLEAR VARIABLES
C
	SER = 0
	BVOL = 0
	FILCNT = 0
        SALAMT=0
	CANCNT=0
	VALCNT=0
C
C READ SCF RECORD
C
	CALL GETSCONF(SCFREC,ST)
	IF(ST.NE.0) CALL GSTOP(GEXIT_FATAL)

	GIND=1
        GNUM=SCFGTN(TBNG,GIND)
        DRAW=DAYDRW(GNUM)
C	    
C	GNUM = GTNTAB(TBNG,GIND)
C
C	IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) THEN
C	    TYPE*,IAM(),'Invalid game number specified ',GNUM,GIND
C	    CALL GSTOP(GEXIT_FATAL)
C	ENDIF
C
        IF(ONLINE) THEN
            CALL GAMLOG(TBNG,GIND,DBNREC,BNGBLK)
        ELSE
            CALL OPENW(2,SCFGFN(1,GNUM),4,0,0,ST)
            CALL IOINIT(FDB,2,DBNSEC*256)
            IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)
            CALL READW(FDB,DRAW,DBNREC,ST)
            IF(ST.NE.0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)
            CALL CLOSEFIL(FDB)
        ENDIF

	BVOL = SCFPAR(REG_DRWPCK)
        IF(BVOL.EQ.0) THEN
           CALL PRMTEXT('Enter Bingo draw pack volume name: ',
     *           CXBVOL, INPLEN)
           IF(CXBVOL.EQ.'E ')CALL GSTOP(GEXIT_OPABORT)
	ENDIF
C
	CALL OPENW(1,SFNAMES(1,DAF),4,0,0,ST)
	CALL IOINIT(DFDB,1,DAFSEC*256)
	IF(ST.NE.0) CALL FILERR(SFNAMES(1,DAF),1,ST,0)
	DO 10 CDC = DBNBSD,DBNESD-1
	    CALL READW(DFDB,CDC,DAFREC,ST)
	    IF(ST.NE.0) CALL FILERR(SFNAMES(1,DAF),2,ST,CDC)
	    IF(DAFSTS.EQ.DNOSAL) GOTO 10
	    IF(DAFSTS.EQ.DSOPEN) GOTO 10
	    FILCNT = FILCNT + 1
	    FTYPE(FILCNT) = 0
	    WRITE(CFILES(FILCNT),900) BVOL,GSNAMES(GNUM),CDC
	    INQUIRE(FILE=CFILES(FILCNT),EXIST=EXFIL)
	    IF(.NOT.EXFIL) THEN
		SCAN = .FALSE.
		WRITE(5,901) IAM(),CFILES(FILCNT)
	    ENDIF
10	CONTINUE
	CALL CLOSEFIL(DFDB)
C
C
	IF(.NOT.SCAN) THEN
	    TYPE*,IAM(),'Sorry, all draw files needed does not exist ',
     *      'where specified, can not perform the draw file scan! '
	     CALL GSTOP(GEXIT_FATAL)
	ENDIF
C
C OPEN FTP-FILE
C
	BYTLEN=85
	FLUN=6 
	CALL FTP_OPEN(FILENAME,FLUN,BYTLEN,ST)
	IF(ST.NE.0) CALL FILERR(FILENAME,1,ST,0)
C
C OPEN DRAW FILE
C
	DO 3000 I = 1,FILCNT
	    CALL WIN_OPNDRW(FILES(1,I),PTMF)
	    CALL IOINIT(TFDB,PTMF,128*256)
	    WRITE(5,910) IAM(),(FILES(S,I),S=1,5)
C
C SCAN DRAW FILE
C
	    BLOCK = 0
	    EOFCNT = 0
	    IND = 8192
2030	    CONTINUE
	    IF(IND.GE.8157) THEN
		BLOCK = BLOCK + 1
		IND = 1
		CALL READW(TFDB,BLOCK,TMFBUF,ST)
		IF(ST.NE.0) THEN
		    WRITE(5,920) IAM(),(FILES(S,I),S=1,5),ST,BLOCK
		    CALL GPAUSE
		ENDIF
	    ENDIF
	    IF(EOFCNT.GT.1000) GOTO 3000
C
C
	    IF(TMFBUF(IND).EQ.0) THEN
		EOFCNT = EOFCNT + 1
		IND = IND + LREC
		GOTO 2030
	    ENDIF
C
C
	    EOFCNT = 0
	    I4TEMP = TMFBUF(IND+LREC-1)
	    TYPE = I1TEMP(4)
	    IF(TYPE.NE.LONE.AND.TYPE.NE.LREG) THEN
		TYPE*,IAM(),' Bad record type > ',TYPE,' index > ',IND
		IND = IND + LREC
		GOTO 2030
	    ENDIF
C
C
	    LENGTH = LREC
	    IF(TYPE.EQ.LONE) THEN
		I4TEMP = TMFBUF(IND+LREC*2-1)
		TYPE = I1TEMP(4)
		IF(TYPE.EQ.LEND) LENGTH = LREC*2
		IF(TYPE.EQ.LTWO) LENGTH = LREC*3
	    ENDIF
C
C GET GAME NUMBER, STATUS AND TYPE
C
	    I4TEMP = TMFBUF(IND+5-1)
            S      = ZEXT(I1TEMP(4))
	    GAMN   = IAND(S,'7F'X)
C
	    I4TEMP = TMFBUF(IND+6-1)
	    STAT = ZEXT(I1TEMP(1))
	    TYPE = ISHFT(ZEXT(I1TEMP(2)),-4)
	    AMTBET = TMFBUF(IND+9-1)
	    CUR_SEED = TMFBUF(IND+25-1)	
C
C INCREMENT INDEX INTO TMFBUF
C
	    IND = IND + LENGTH
C
C IF WE ARE N0T DOING WHAT WE WANT TO, READ NEXT TRANSACTION
C
	    IF(GNUM.NE.GAMN.OR.STAT.NE.GOOD.OR.TYPE.NE.TWAG) GOTO 2030

	    SALAMT=SALAMT+AMTBET
            VALCNT=VALCNT+1
C
            IF(FIRST) THEN
                FIRST=.FALSE.
		FIRST_SEED = CUR_SEED
                LAST_SEED=CUR_SEED-1
		WRITE(FLUN,930) FIRST_SEED
            ENDIF
C
	    IF(CUR_SEED.EQ.LAST_SEED) GOTO 2030
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
C
C GET ANOTHER TRANSACTION
C
	    GOTO 2030
C
C END OF FILE, SCAN NEXT DRAW FILE
C
3000	CONTINUE
	SALAMT=(SALAMT*5)/100
        WRITE(FLUN,940) SALAMT,VALCNT,CANCNT,FIRST_SEED,LAST_SEED
	CALL USRCLOS1(FLUN)
	CALL GSTOP(GEXIT_SUCCESS)
C
C
900     FORMAT(A4,':',A4,I4.4,'.FIL')
901	FORMAT(1X,A,'File ',A20,' does not exist! ')
910     FORMAT(1X,A,1X,'Scanning file ',5A4,' for cancels')
920     FORMAT(1X,A,1X,5A4,' read error> ',I4,' block> ',I8)
930	FORMAT(I20)
940     FORMAT('TOTAL',5(I20))
	END	
