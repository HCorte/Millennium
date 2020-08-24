C SCANVLF.FOR
C
C V02 26-MAR-93 HDB  ADDED REPORT LISTING ALL VLF RECORDS
C V01 04-MAR-93 HDB  ADDED SOME COUNTERS
C V00 12-FEB-93 HDB  RELEASE FOR NETH
C
C
C TOOL TO SCAN VLF FILE SEQUENTIALLY
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
	PROGRAM SCANVLF
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PRMHSH.DEF'
	INCLUDE 'INCLIB:DESVAL.DEF'
	INCLUDE 'INCLIB:VALFIL.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:TNAMES.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
C	INTEGER*4 DRAWS(MAXGAM)	    ! current draw for game
C
        INTEGER*4    TUBSIZ	    !
        PARAMETER   (TUBSIZ=I4BUCSIZ*7)
C
	INTEGER*4 VLFBUF(TUBSIZ)	! vlf buffer
	INTEGER*4 GIND			! game index
	INTEGER*4 GTYP			! game type
	INTEGER*4 GNUM			! game number
	INTEGER*4 GAM			! game counter
	INTEGER*4 I			! counter
	INTEGER*4 T			! counter
	INTEGER*4 ST			! result state after call
	INTEGER*4 COUNT			! total number of records found
	INTEGER*4 TOTAL(MAXTYP,MAXIND)	! total number of val record per typ+ind
        INTEGER*4 SCFNAM(5)		!
	INTEGER*4 SCFFDB(7)		!
	INTEGER*4 REPALL		! report logical unit
	INTEGER*4 REPUCH		! report logical unit
	INTEGER*4 SSER			!
	INTEGER*4 SCHK			!
	INTEGER*2 DATE(LDATE_LEN)	! date buffer	
	INTEGER*4 READCOUNT             ! number of records read
C
        DATA      SCFNAM/'SCF.','FIL ',3*'    '/
C
C READ SCF RECORD
C
        CALL OPENW(1,SCFNAM,4,0,0,ST)
        CALL IOINIT(SCFFDB,1,SCFSEC*256)
        IF(ST.NE.0) CALL FILERR(SCFNAM,1,ST,0)
        CALL READW(SCFFDB,1,SCFREC,ST)
        IF(ST.NE.0) CALL FILERR(SCFNAM,2,ST,1)
        CALL CLOSEFIL(SCFFDB)
        DO 10 I=1,MAXFIL
           IF(SCFSFN(1,I).EQ.'    ') CALL SYSVOL(SCFSFN(1,I))
10      CONTINUE
C
C INIT VARS
C
	COUNT     = 0
	READCOUNT = 0
	REPALL    = 6
	REPUCH    = 7
	CALL FASTSET(0,TOTAL,MAXTYP*MAXIND)
C
C OPEN THE REPORT FILES AND WRITE HEADER
C

	WRITE (5,3000) IAM() 
	CALL ROPEN('VLFTOTAL.REP',REPALL,ST)
	WRITE (5,3001) IAM()
	CALL ROPEN('VLFUNCSH.REP',REPUCH,ST)
	WRITE(REPALL,4000)
	WRITE(REPUCH,4000)
	WRITE (5,3002) IAM()
C
C OPEN AND START SCAN VLF
C
        CALL IOPEN(SFNAMES(1,VLF),1,VFLEN*2,VFSCDC,VFSSER*2-1,ST)
        IF(ST.NE.0) CALL FILERR(SFNAMES(1,VLF),1,ST,0)
        CALL ITUBSIZE(1,TUBSIZ)
C
C
1000	CONTINUE
          CALL ISREAD(V4BUF,1,VLFBUF,ST)
          IF(ST.EQ.ERREND) THEN
	    GOTO 2000
	  ENDIF
	  READCOUNT = READCOUNT + 1
	  IF(MOD(READCOUNT,10000).EQ.0) WRITE(5,3004) IAM(),READCOUNT
	  IF(ST.NE.0) CALL FILERR(SFNAMES(1,VLF),2,ST,0)
	  CALL LOGVAL(VALREC,V4BUF)
	  GAM  = VALREC(VGAM)
	  GTYP = VALREC(VGTYP)
	  GIND = VALREC(VGIND)
	  TOTAL(GTYP,GIND) = TOTAL(GTYP,GIND) + 1
	  COUNT = COUNT + 1
	  DATE(VCDC)=VALREC(VSCDC)
	  CALL LCDATE(DATE)
	  CALL OUTGEN(VALREC(VSCDC),VALREC(VSSER),SSER,SCHK)
	  WRITE(REPALL,4001)  AGTTAB(AGTNUM,VALREC(VSTER)),
     *	                     VALREC(VSTER),
     *			     DATE(VJUL),
     *                       SSER,
     *                       SCHK,
     *	                     VALREC(VSCDC),
     *                       VALST(VALREC(VSTAT)),
     *	                     VALREC(VEXP),
     *                       CMONY(VALREC(VPAMT),11,VALUNIT),
     *                       CMONY(VALREC(VRAMT),11,BETUNIT),
     *                       VALREC(VGTYP),
     *                       VALREC(VGIND)
	  IF(VALREC(VSTAT).EQ.VUNCSH) THEN
	    WRITE(REPUCH,4001)  AGTTAB(AGTNUM,VALREC(VSTER)),
     *	                     VALREC(VSTER),
     *			     DATE(VJUL),
     *                       SSER,
     *                       SCHK,
     *	                     VALREC(VSCDC),
     *                       VALST(VALREC(VSTAT)),
     *	                     VALREC(VEXP),
     *                       CMONY(VALREC(VPAMT),11,VALUNIT),
     *                       CMONY(VALREC(VRAMT),11,BETUNIT),
     *                       VALREC(VGTYP),
     *                       VALREC(VGIND)
	  ENDIF
	GOTO 1000
C
C WRITE OUT TOTALS
C
2000	CONTINUE
	DO 2010 T=1,MAXTYP
	   DO 2010 I=1,MAXIND
	      GNUM = SCFGTN(T,I)
	      IF(GNUM.LE.0.OR.GNUM.GT.MAXGAM) THEN
		WRITE(REPALL,5001) T,I,TOTAL(T,I)
	      ELSE
		WRITE(REPALL,5000) SCFSGN(GNUM),T,I,TOTAL(T,I)
	      ENDIF
2010	CONTINUE
	WRITE(REPALL,5002) COUNT
C
	WRITE (5,3003) IAM()
	CALL ICLOSE(1,VLFBUF,ST)
	CALL USRCLOS1(REPALL)
	CALL USRCLOS1(REPUCH)
C
	CALL GSTOP(GEXIT_SUCCESS)
C
C FORMAT STATEMENTS
C
3000	FORMAT(1X,A18,' Opening VLFTOTAL.REP ')
3001	FORMAT(1X,A18,' Opening VLFUNCSH.REP ')
3002	FORMAT(1X,A18,' Starting VLF scan ')
3003	FORMAT(1X,A18,' Finishing and closing reports')
3004	FORMAT(1X,A18,' ',I8,' Records processed')
C
4000    FORMAT(/,2X,' SELLING   SELLING',T43,'CDC   TICKET  ',
     *       2X,'DRAW',/,4X,' AGENT  TERMINAL      TICKET SERIAL  SOLD',
     *         '   STATUS  EXPIRE      PRIZE AMT    REFUND AMT',
     *         ' GAM IDX',/)
4001	FORMAT(4X,I6.6,5X,I5,3X,I3.3,'-',I8.8,'-',I3.3,
     *	       1X,I5,5X,A4,3X,I5,4X,A11,4X,A11,1X,I2,1X,I2)
C
5000	FORMAT(1X,A4,' GTYP ',I2,' GIND ',I2,1X,I8)
5001	FORMAT(1X,'----',' GTYP ',I2,' GIND ',I2,1X,I8)
5002    FORMAT(1X,'TOTAL RECORDS ',I8)
C
	END
