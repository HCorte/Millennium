C  GXSRC:CPLSET.FOR
C
C V11 24-MAY-1999 UXN Minimum stake added.
C V10 05-JUN-1997 UXN Calculation of REV3 changed.
C V09 14-APR-1996 HXK Putting RXK's (Rita's) changes into archive
C V08 03-APR-1996 RXK Calculation of control revision byte changed
C V07 22-FEB-1996 HXK Fix for blank lines in description
C V06 21-FEB-1996 HXK Fixes for blank descriptions
C V05 21-FEB-1996 HXK Fix for blank description lines
C V04 18-FEB-1996 HXK Changed method of updating First revision byte such 
C                     that it changes from draw to draw
C V03 13-DEC-1995 PXB Bug fix in listing couple game
C V02 05-DEC-1995 HXK Changed WxPnnnn.FIL to CxPnnnn.FIL 
C V01 23-NOV-1995 PXB Initial revision.
C
C SUBROUTINE CPLSET
C
C SUPER DOUBLE GAME SETUP
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
C Copyright 1995 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C=======OPTIONS /CHECK=NOOVERFLOW

	SUBROUTINE CPLSET(GIND)

	IMPLICIT NONE

C---- Include files used.

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:DCPREC.DEF'
	INCLUDE 'INCLIB:RECDAF.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'

C---- Local variables used.

	COMMON SCFREC
	INTEGER*4    DFDB(7),FDB(7),VFDB(7),INPFDB(7),BYTTAB(200)
	INTEGER*2    BDATE(LDATE_LEN),EDATE(LDATE_LEN),DATE(LDATE_LEN)
	INTEGER*2    DDATE(LDATE_LEN)
	INTEGER*4    GNUM,GIND,ST,I,DRW,EXT,LSTAT,WEK,J,L,YEAR
	INTEGER*4    K,BROW,EROW,CDC,ROW,BUFIND,REV1,REV2,REV3,REV4
	INTEGER*4    WHICH_EVENT
	INTEGER*4    PREV3,VERR,SALPER,PRICE
	INTEGER*4    CLEARED
	INTEGER*4    DESCRIPT_LINES
	INTEGER*4    DSCIND,DSCIND2
	CHARACTER*6  PASS,OKPASS
	CHARACTER*8  CPINT_NAME
	CHARACTER*20 PWORD,CDCPPFN
	EQUIVALENCE  (PASS,PWORD)
	CHARACTER*2  FUN
	INTEGER*4    NAME(CPLENM_LEN/4),ENAME(CPLENM_LEN/4)
	INTEGER*4    DNAME(CPLDES_LEN/4,2)
	CHARACTER    INPFIL,ANS
	CHARACTER    CENAME(CPLENM_LEN),CNAME(CPLENM_LEN)
	CHARACTER    CDNAME(CPLDES_LEN,2)
	BYTE         BENAME(CPLENM_LEN),BNAME(CPLENM_LEN)
	BYTE         BDNAME(CPLDES_LEN,2)
	INTEGER*4    TEMP		!used to restore to old values
	EQUIVALENCE  (DCPPFN,CDCPPFN)
	EQUIVALENCE  (NAME,CNAME,BNAME)
	EQUIVALENCE  (DNAME,CDNAME,BDNAME)
	EQUIVALENCE  (ENAME,CENAME,BENAME)
	CHARACTER*12 GSTAT(11)

	DATA GSTAT   /'Not set     ','Game closed ',
     *	              'Info entered','Game open   ',
     *	            3*'End of game ',
     *	              'Drawing done','Game final  ',
     *	              'Cancelled   ','Refund      '/

	CHARACTER*14 CDAY
	INTEGER*2    DAY(7)
	LOGICAL	     TSTFLG
	DATA	     CDAY/'---not set--- '/
	DATA	     OKPASS/'PERKIN'/
	EQUIVALENCE  (CDAY,DAY)

        INTEGER*4 M251
        PARAMETER(M251=251)


C---------------------------- Start of Code ----------------------------


C---- Initialize / set variables 

	WRITE(CPINT_NAME,990) GTNAMES(TCPL)

	DESCRIPT_LINES = CPLDES_LEN/30		!30 Bytes / line

C---- Check if super double game is active

	GNUM = SCFGTN(TCPL,GIND)

	IF (GNUM .LT. 1) THEN
	  WRITE(5,991) IAM(),GTNAMES(TCPL),GIND
	  CALL XWAIT(2,2,ST)
	  RETURN
	END IF

C---- Open required files.
 
	CALL OPENW(1,
     *		   SCFSFN(1,DAF),
     *	           4,
     *             0,
     *             0,
     *             ST)

	CALL IOINIT(DFDB,
     *	            1,
     *		    DAFSEC*256)

	IF (ST .NE. 0) THEN
	  CALL FILERR(SCFSFN(1,DAF),1,ST,0)
	  RETURN
	END IF

	CALL OPENW(2,
     *             SCFGFN(1,GNUM),
     *             4,
     *             0,
     *             0,
     *             ST)

	CALL IOINIT(FDB,
     *              2,
     *              DCPSEC*256)

	IF (ST .NE. 0) THEN
	  CALL FILERR(SCFGFN(1,GNUM),1,ST,0)
	  CALL CLOSEFIL(DFDB)
	  RETURN
	END IF

	CALL OPENW(3,
     *             SCFGVN(1,GNUM),
     *             4,
     *             0,
     *             0,
     *             ST)

	CALL IOINIT(VFDB,
     *              3,
     *              DCPSEC*256)

	IF (ST .NE. 0) THEN
	  CALL FILERR(SCFGVN(1,GNUM),1,ST,0)
	  CALL CLOSEFIL(DFDB)
	  CALL CLOSEFIL(FDB)
	  RETURN
	END IF

10	CONTINUE

	TSTFLG = .FALSE.

	CALL CLRSCR(5)

	WRITE(5,900)

	CALL WIMG(5,'Enter function: ')

	READ(5,901) FUN

	IF (FUN .EQ. 'EX') THEN
	  CALL CLOSEFIL(DFDB)
	  CALL CLOSEFIL(FDB)
	  CALL CLOSEFIL(VFDB)
	  RETURN
	END IF

	IF (FUN .NE. 'LI' .AND. 
     *	    FUN .NE. 'MO' .AND.
     *	    FUN .NE. 'VE' .AND. 
     *      FUN .NE. 'UN' .AND.
     *	    FUN .NE. 'CH') THEN
	  CALL CLRSCR(5)
	  TYPE*,'Invalid input '
	  CALL XWAIT(1,2,ST)
	  GOTO 10
	END IF

	CALL FASTMOV(FDB,INPFDB,7)

	IF (FUN .EQ. 'LI' .OR. FUN .EQ. 'MO') THEN
15	  CONTINUE
          CALL WIMG(5,'Use (P)rimary or (V)erification file as input?')
	  READ(5,912)INPFIL
	  IF (INPFIL .EQ. 'E') GOTO 10
	  IF (INPFIL .EQ. 'V') THEN
	     CALL FASTMOV(VFDB,INPFDB,7)
	  ELSE
	     IF (INPFIL .NE. 'P') THEN
	       TYPE*,'OPTIONS ARE P, V, AND E ONLY...RETRY.'
	       GOTO 15
	     END IF
	  END IF
	END IF


20	CONTINUE

	IF (FUN .NE. 'LI') CALL CLRSCR(5)
	CALL INPNUM('Enter event number(E-exit) ',DRW,-9999,9999,EXT)
	IF (EXT .LT. 0 .OR. DRW .EQ. 0) GOTO 10
	IF (DRW .LT. 1) TSTFLG = .TRUE.
	DRW = ABS(DRW)
C
C	Read previous text checksum
C
	PREV3 = 0
	IF(DRW.GT.1) THEN
	  CALL READW(INPFDB,DRW-1,DCPREC,ST)

	  IF (ST .NE. 0) THEN
	    CALL CLRSCR(5)
	    IF (INPFIL .EQ. 'V') THEN
	      CALL FILERR(SCFGVN(1,GNUM),2,ST,DRW-1)
	    ELSE
	      CALL FILERR(SCFGFN(1,GNUM),2,ST,DRW-1)
	    END IF
	    CALL XWAIT(2,2,ST)
	    GOTO 20
	  END IF
	  CALL ILBYTE(PREV3,DCPREV,2)          !GET PREVIOUS TEXT REV #
        ENDIF
C
C
	CALL CLRSCR(5)
	CALL READW(INPFDB,
     *             DRW,
     *             DCPREC,
     *		   ST)

	IF (ST .NE. 0) THEN
	  CALL CLRSCR(5)
	  IF (INPFIL .EQ. 'V') THEN
	    CALL FILERR(SCFGVN(1,GNUM),2,ST,DRW)
	  ELSE
	    CALL FILERR(SCFGFN(1,GNUM),2,ST,DRW)
	  END IF
	  CALL XWAIT(2,2,ST)
	  GOTO 20
	END IF

	LSTAT = DCPSTS + 1

	DO 30 I = 1,7
	  BDATE(I+6) = DAY(I)
	  EDATE(I+6) = DAY(I)
	  DDATE(I+6) = DAY(I)
30	CONTINUE

	BDATE(5) = DCPBSD
	IF (BDATE(5) .NE. 0) CALL LCDATE(BDATE)

	EDATE(5) = DCPESD
	IF (EDATE(5) .NE. 0) CALL LCDATE(EDATE)

	DDATE(5) = DCPDAT
	IF (DDATE(5) .NE. 0) CALL LCDATE(DDATE)

	WEK = DCPWEK
	SALPER = DCPSPR
	PRICE = DCPPRC

	IF (FUN .EQ. 'MO' .AND. DCPSTS .GE. GAMOPN) THEN
	  CALL CLRSCR(5) 
	  WRITE(5,992) IAM(),GTNAMES(TCPL),GIND,DRW,IAM()
	  CALL WIMG(5,'Hit return to continue')
	  READ(5,901) ANS
	  GOTO 10
	END IF
 
	DO 80 J = 1,MAXCPLRW
	  DO 70 K = 1,CPLNMS_LEN/4
	    IF (DCPNMS(K,J) .EQ. 0) DCPNMS(K,J) = '    '
70	  CONTINUE
80	CONTINUE

	DO 91 J = 1,2
	  DO 90 I = 1,CPLENM_LEN/4
	    IF (DCPENM(I,J) .EQ. 0) DCPENM(I,J) = '    '
90	  CONTINUE
91	CONTINUE

	IF (FUN .EQ. 'MO') GOTO 120
	IF (FUN .EQ. 'VE') GOTO 1000
	IF (FUN .EQ. 'UN') GOTO 2000
	IF (FUN .EQ. 'CH') GOTO 3000

C---- Display game data.

	BROW = 1
	EROW = 17 - DESCRIPT_LINES 

105	CONTINUE

	CALL CLRSCR(5)

	WRITE(5,903) GTNAMES(TCPL),
     *		     GIND,
     *		     DRW,
     *	             WEK,
     *               GSTAT(LSTAT)

	WRITE(5,9031) (DCPENM(K,1),K=1,CPLENM_LEN/4),
     *                (DCPENM(K,2),K=1,CPLENM_LEN/4)

	DSCIND = 0
	DSCIND2 = 0

	CALL FASTMOV(DCPDES,DNAME,(CPLDES_LEN/4*2))

	DO I = 1,DESCRIPT_LINES
	   WRITE(5,9141) (CDNAME(K,1),K=DSCIND+1,DSCIND+30),
     *		         (CDNAME(L,2),L=DSCIND2+1,DSCIND2+30)
	   DSCIND = DSCIND + 30
	   DSCIND2 = DSCIND2 + 30
	END DO

        WRITE(5,904) 
     *	(BDATE(I),I=7,13),BDATE(5),(EDATE(I),I=7,13),EDATE(5)

	WRITE(5,910) (DCPPFN(K),K=1,5), (DCPTVC(K),K=1,CPLTVC_LEN/4)

	WRITE(5,905) DISTIM(DCPTIM),(DDATE(K),K=7,13),DDATE(5)
        WRITE(5,945) CMONY(DCPPRC,10,BETUNIT)

	WRITE(5,9051) 

	DO 110 I=BROW,EROW
	  WRITE(5,906) I,
     *		       (DCPNMS(J,I),J=1,CPLNMS_LEN/4),
     *		       I,
     *		       (DCPNMS(J,I+18),J=1,CPLNMS_LEN/4)
110	CONTINUE

	IF (EROW .NE. (MAXCPLRW/2)) THEN
	  CALL WIMG(5,'Hit return to see remaining rows')
	  READ(5,901) ANS
	  BROW = EROW + 1
	  EROW = EROW + 17 - DESCRIPT_LINES    
	  IF (EROW .GT. (MAXCPLRW/2)) EROW = MAXCPLRW/2
	  GOTO 105
	END IF

	IF (TSTFLG) THEN
	  WRITE(5,99999) IAM(),'REVISION:   ',DCPREV
	  WRITE(5,99998) IAM(),'DESCRIPT:   ',(DCPDES(K,1),K=1,CPLDES_LEN/4)
	  WRITE(5,99998) IAM(),'DESCRIPT:   ',(DCPDES(K,2),K=1,CPLDES_LEN/4)
	END IF

	GOTO 20

C---- Modify game date

120	CONTINUE

	WRITE(5,917) IAM()

	WRITE(5,918) IAM(),(BDATE(I),I=7,13) 

	CALL INPDAT(CDC,EXT)

	IF (EXT .NE. 0) CDC = DCPBSD

	BDATE(5) = CDC

	CALL LCDATE(BDATE)

	WRITE(5,919) IAM()

	WRITE(5,918) IAM(),(EDATE(I),I=7,13) 

	CALL INPDAT(CDC,EXT)

	IF (EXT .NE. 0) CDC = DCPESD

	EDATE(5)=CDC

	CALL LCDATE(EDATE)

	CALL FIGWEK(CDC-WEEK_OFFSET,WEK,YEAR)

	WRITE(5,923) IAM()

	WRITE(5,924) IAM(),(DDATE(I),I=7,13)

	CALL INPDAT(CDC,EXT)

	IF (EXT .NE. 0) CDC = DCPDAT

	DDATE(5) = CDC

	CALL LCDATE(DDATE)

	WRITE(5,921) IAM()

	WRITE(5,922) IAM(),DISTIM(DCPTIM)

	TEMP = DCPTIM

	CALL INPTIM('Enter time HH:MM:SS....................',DCPTIM,EXT)

	IF (EXT .NE. 0) DCPTIM = TEMP 

        WRITE(5,944) IAM(), CMONY(PRICE,10,BETUNIT)
        CALL INPMONY('Enter minimum stake.........',PRICE,BETUNIT,EXT)
        IF(EXT.NE.0) DCPPRC=PRICE

	WRITE(5,929) IAM()

	WRITE(5,930) IAM(),DCPPFN(1)

	WRITE (CDCPPFN,911) GIND,DRW

	CALL WIMG(5,'Enter pool file volume name............')

	READ(5,907) NAME(1)

	IF (NAME(1) .NE. '    ') DCPPFN(1)=NAME(1)

C---- Replace nulls with spaces

	DO I = 1,CPLENM_LEN
	   IF(BENAME(I) .EQ. 0) BENAME(I) = ' '
	ENDDO

C---- Event one is prompted first and then Event 2.

	WRITE(5,931) IAM()

	WRITE(5,932) IAM(),(DCPENM(K,1),K=1,CPLENM_LEN/4)

	CALL WIMG(5,'Enter first event name.......................')

	CLEARED = 0

	READ(5,933) ENAME

C---- Did he/she hit return?

	DO I = 1,CPLENM_LEN/4
	  IF(ENAME(I).EQ.'    ') CLEARED = CLEARED + 1
	END DO

	IF (CLEARED .EQ. CPLENM_LEN/4) GOTO 135

C---- Does he want us to clear it?

	IF (CENAME(1) .EQ. '+') THEN
	   DO I = 1,CPLENM_LEN/4
	     ENAME(I) = '    '
	   END DO
	END IF

C---- Replace nulls with spaces

	DO I = 1,CPLENM_LEN
	   IF(BENAME(I) .EQ. 0) BENAME(I) = ' '
	ENDDO

C---- Move the input over.

	DO I=1,CPLENM_LEN/4
	  DCPENM(I,1) = ENAME(I)
	END DO

135	CONTINUE

C---- Event 2.

	WRITE(5,9311) IAM()

	WRITE(5,932) IAM(),(DCPENM(K,2),K=1,CPLENM_LEN/4)

	CALL WIMG(5,'Enter second event name.......................')

	CLEARED = 0

	READ(5,933) ENAME

C---- Did he/she hit return?

	DO I = 1,CPLENM_LEN/4
	  IF(ENAME(I).EQ.'    ') CLEARED = CLEARED + 1
	END DO

	IF (CLEARED .EQ. CPLENM_LEN/4) GOTO 140

C---- Does he want us to clear it?

	IF (CENAME(1) .EQ. '+') THEN
	   DO I = 1,CPLENM_LEN/4
	     ENAME(I) = '    '
	   END DO
	END IF

C---- Replace nulls with spaces

	DO I = 1,CPLENM_LEN
	   IF(BENAME(I) .EQ. 0) BENAME(I) = ' '
	ENDDO

C---- Move the input over.

	DO I=1,CPLENM_LEN/4
	  DCPENM(I,2) = ENAME(I)
	END DO

C---- Description of event 1 first then event 2.

140	CONTINUE
	WRITE(5,934) IAM()
	DSCIND = 0
	CALL FASTMOV(DCPDES,DNAME,(CPLDES_LEN/4*2))
	DO I = 1,DESCRIPT_LINES
	   WRITE(5,935) IAM(),I,(CDNAME(K,1),K=DSCIND+1,DSCIND+30)
	   DSCIND = DSCIND + 30
	END DO
	TYPE*,IAM()
	DSCIND = 0
	CALL WIMG(5,'Description Length        <                             ')
	DO I = 1,DESCRIPT_LINES
	   CALL WIMG(5,'Enter event 1 description')
	   READ(5,936) (CDNAME(K,1),K=DSCIND+1,DSCIND+30)
	   DSCIND = DSCIND + 30
	END DO

C---- Did he/she hit return?

	CLEARED = 0
	DO I = 1,CPLDES_LEN/4
	   IF (DNAME(I,1) .EQ. '    ') CLEARED = CLEARED + 1
	END DO
	IF (CLEARED .EQ. CPLDES_LEN/4) THEN
	   CLEARED = 0 
	   DO I = 1,CPLDES_LEN/4
	      IF (DCPDES(I,1) .EQ. 0) CLEARED = CLEARED + 1
	   END DO
	   IF (CLEARED .EQ. CPLDES_LEN/4) THEN
	      DO I = 1,CPLDES_LEN/4
	         DCPDES(I,1) = '    '
	      ENDDO
           ENDIF
           GOTO 141
	ENDIF

C---- Does he want us to clear it?

	IF (CDNAME(1,1) .EQ. '+') THEN
	   DO I = 1,CPLDES_LEN/4
	      DNAME(I,1) = '    '
	   END DO
	ENDIF

C---- Move the description over.

	DO I = 1,CPLDES_LEN/4
	   DCPDES(I,1) = DNAME(I,1)
	END DO

141	CONTINUE

	WRITE(5,934) IAM()
	DSCIND = 0
	DO I = 1,DESCRIPT_LINES
	   WRITE(5,935) IAM(),I,(CDNAME(K,2),K=DSCIND+1,DSCIND+30)
	   DSCIND = DSCIND + 30
	ENDDO

	TYPE*,IAM()

	DSCIND = 0

	CALL WIMG(5,'Description Length        <                             ')

	DO I = 1,DESCRIPT_LINES
	  CALL WIMG(5,'Enter event 2 description')
	  READ(5,936) (CDNAME(K,2),K=DSCIND+1,DSCIND+30)
	  DSCIND = DSCIND + 30
	END DO

C---- Did he/she hit return?

	CLEARED = 0
	DO I = 1,CPLDES_LEN/4
	   IF (DNAME(I,2) .EQ. '    ') CLEARED = CLEARED + 1
	END DO
	IF (CLEARED .EQ. CPLDES_LEN/4) THEN
	   CLEARED = 0 
	   DO I = 1,CPLDES_LEN/4
	      IF (DCPDES(I,2) .EQ. 0) CLEARED = CLEARED + 1
	   END DO
	   IF (CLEARED .EQ. CPLDES_LEN/4) THEN
	      DO I = 1,CPLDES_LEN/4
	         DCPDES(I,2) = '    '
	      ENDDO
           ENDIF
           GOTO 165
	ENDIF

C---- Does he want us to clear it?

	IF (CDNAME(1,2) .EQ. '+') THEN
	  DO I = 1,CPLDES_LEN/4
	    DNAME(I,2) = '    '
	  END DO
	END IF

C---- Move the description over.

	DO I = 1,CPLDES_LEN/4
	  DCPDES(I,2) = DNAME(I,2)
	END DO

165	CONTINUE

	WRITE(5,937) IAM()

	WRITE(5,938) IAM(),(DCPTVC(K),K=1,CPLTVC_LEN/4)

	CALL WIMG(5,'Enter TV-Chanel Name...................')

	READ(5,939) (NAME(K),K=1,CPLTVC_LEN/4) 

C---- Did he/she hit return?

	CLEARED = 0

	DO I = 1,CPLTVC_LEN/4
	   IF (NAME(I) .EQ. '    ') CLEARED = CLEARED + 1
	END DO

	IF (CLEARED .EQ. CPLTVC_LEN/4) GOTO 299

C---- Does he want us to clear it?

	IF (CNAME(1) .EQ. '+') THEN
	   DO I = 1,CPLTVC_LEN/4
	      NAME(I) = '    '
	   END DO
	END IF

C---- Move the name over.

	DO I = 1,CPLTVC_LEN/4
	   DCPTVC(I) = NAME(I)
	END DO


299	CONTINUE

	CALL INPNUM('Enter which event the rows belong to (E-Exit)',
     *	      WHICH_EVENT,1,2,EXT)

	IF (EXT .LT. 0) GOTO 500

300	CONTINUE

	TYPE*,IAM()

	CALL INPNUM('Enter row number to be modified (E-Exit)',
     *	     ROW,1,MAXCPLRW/2,EXT)

	IF (EXT .LT. 0) GOTO 299

	WRITE(5,940) IAM()

	IF (WHICH_EVENT .EQ. 1) THEN
	    WRITE(5,941) IAM(),(DCPNMS(K,ROW),K=1,CPLNMS_LEN/4)
	ELSE
	    WRITE(5,941) IAM(),(DCPNMS(K,(ROW+MAXCPLRW/2)),K=1,CPLNMS_LEN/4)
	END IF

	CALL WIMG(5,'Enter Row Name........................')

	READ(5,942) (NAME(K),K=1,CPLNMS_LEN/4)

C---- Did he/she hit return?

	CLEARED = 0

	DO I = 1,CPLNMS_LEN/4
	   IF (NAME(I) .EQ. '    ') CLEARED = CLEARED + 1
	END DO

	IF (CLEARED .EQ. CPLNMS_LEN/4) GOTO 210

C---- Does he want us to clear it?

	IF (CNAME(1) .EQ. '+') THEN
	   DO I = 1,CPLNMS_LEN/4
	     NAME(I) = '    '
	   END DO
	END IF

C---- Replace nulls with spaces

	DO I = 1,CPLNMS_LEN
	  IF(BNAME(I) .EQ. 0) BNAME(I) = ' '
	ENDDO

C---- Move the name over.

	DO I = 1,CPLNMS_LEN/4
	  IF (WHICH_EVENT .EQ. 1) THEN
	    DCPNMS(I,ROW) = NAME(I)
	  ELSE
	    DCPNMS(I,(ROW+MAXCPLRW/2)) = NAME(I)
	  END IF
	END DO

210	CONTINUE

	GOTO 300

500	CONTINUE

	DCPBSD = BDATE(5)
	DCPESD = EDATE(5)
	DCPDAT = DDATE(5)
	DCPWEK = WEK
	DCPSTS = GAMINF
	DCPDRW = DRW
	DCPSPR = SALPER
	DCPPRC = PRICE

	DO 510 I = 1,MAXCPLRW
	  DCPSTA(I) = 0
	  IF (DCPNMS(1,I) .NE. '    ') DCPSTA(I) = GAMINF
510	CONTINUE

C---- Create table of text message to checksum for rev.

	BUFIND = 1

C---- Load up event name and event descriptions

	CALL MOVBYT(DCPENM(1,1),
     *              1,
     *              BYTTAB,
     *              BUFIND,
     *              CPLENM_LEN)

	BUFIND = BUFIND + CPLENM_LEN

	CALL MOVBYT(DCPENM(1,2),
     *              1,
     *              BYTTAB,
     *              BUFIND,
     *              CPLENM_LEN)

	BUFIND = BUFIND + CPLENM_LEN

	CALL MOVBYT(DCPDES(1,1),
     *              1,
     *              BYTTAB,
     *              BUFIND,
     *              CPLDES_LEN)

	BUFIND = BUFIND + CPLDES_LEN

	CALL MOVBYT(DCPDES(1,2),
     *              1,
     *              BYTTAB,
     *              BUFIND,
     *              CPLDES_LEN)

	BUFIND = BUFIND + CPLDES_LEN

C---- Load up player names.

	DO I = 1,MAXCPLRW                     !FOR ALL ROWS
	   IF (DCPNMS(1,I) .NE. '    ') THEN
	     CALL MOVBYT(DCPNMS(1,I),
     *			 1,
     *			 BYTTAB,
     *			 BUFIND,
     *			 CPLNMS_LEN-2)
	     BUFIND = BUFIND + (CPLNMS_LEN-2)
	   END IF
	END DO

	BUFIND = BUFIND - 1

	CALL CHECKSUM(BYTTAB,1,BUFIND,REV4)

	CALL ILBYTE(REV1,DCPREV,0)	    

        IF(DCPDRW-1.EQ.M251) THEN
           REV1 = MOD(REV1+DCPDRW,(M251-10)) + 1
        ELSE
           REV1 = MOD(REV1+DCPDRW,M251) + 1
        ENDIF
	REV2 = MOD(DCPDRW,255)

	CALL ILBYTE(REV3,DCPREV,2)          !GET PREVIOUS TEXT REV #

	REV3 = MOD(PREV3 + REV3,255) + 1

	CALL ISBYTE(REV1,DCPREV,0)          !CONTROL REV BYTE (SEQUENCE#)
	CALL ISBYTE(REV2,DCPREV,1)          !DRAW REV BYTE
	CALL ISBYTE(REV3,DCPREV,2)          !TEXT REV # BYTE  (SEQUENCE#)
	CALL ISBYTE(REV4,DCPREV,3)          !TEXT CHECKSUM BYTE

	CALL WRITEW(INPFDB,
     *		    DRW,
     *              DCPREC,
     *		    ST)

	IF (ST .NE. 0) THEN
	  CALL CLRSCR(5)
	  IF (INPFIL .EQ. 'V')THEN
	      CALL FILERR(SCFGVN(1,GNUM),3,ST,DRW)
	  ELSE
	      CALL FILERR(SCFGFN(1,GNUM),3,ST,DRW)
	  END IF
	  GOTO 10
	END IF

	GOTO 20

C---- Verification

1000	CONTINUE

	CALL CLRSCR(5)
	CALL PASSWORD(5,PWORD)

	IF (PASS .NE. OKPASS) THEN
	  TYPE*,'Invalid password entered'
	  CALL XWAIT(2,2,ST)
	  GOTO 10
	END IF

	VERR = 0

	IF (DCPSTS .EQ. GAMOPN) THEN
	  TYPE*,IAM(),CPINT_NAME,GIND,' event ',DRW,' already verified'
	  CALL WIMG(5,'Hit return to continue')
	  READ(5,901) ANS
	  GOTO 10
	END IF

	IF (DCPSTS .NE. GAMINF) THEN
	  TYPE*,IAM(),CPINT_NAME,GIND,' event ',DRW,' invalid status ',
     *          DCPSTS
	  CALL WIMG(5,'Hit return to continue')
	  READ(5,901) ANS
	  GOTO 10
	END IF

	IF (DCPBSD .GT. DCPESD) THEN
	  TYPE*,IAM(),'Begining sales date greater then ending sales date'
	  VERR = VERR + 1
	END IF

	IF (DCPBSD .EQ. 0) THEN
	  TYPE*,IAM(),'Begining sales date not set '
	  VERR = VERR + 1
	END IF

	IF (DCPDAT .LT. DCPESD) THEN
	  TYPE*,IAM(),'Event date is before last sales date'
	  VERR = VERR + 1
	END IF

	IF (DCPESD .EQ. 0) THEN
	  TYPE*,IAM(),'Ending sales date not set'
	  VERR = VERR + 1
	END IF

	IF (DCPPRC .EQ. 0) THEN
	  TYPE*,IAM(),'Base price not set'
	  VERR = VERR + 1
	END IF

	IF (DCPSPR .EQ. 0) THEN
	  TYPE*,IAM(),'Pool percentage not set'
	  VERR = VERR + 1
	END IF

	IF (VERR .NE. 0) THEN
          TYPE*,IAM(),VERR,' data entry errors for ',CPINT_NAME,
     *        GIND,' event ',DRW
	  TYPE*,IAM(),'Event has not been verified'
	  CALL WIMG(5,'Hit return to continue')
	  READ(5,901) ANS
	  GOTO 10
	END IF

	DO 1020 I = DCPBSD,DCPESD
	  DATE(5) = I
	  CALL LCDATE(DATE)
	  CALL READW(DFDB,I,DAFREC,ST)
	  IF (ST .NE. 0) THEN
	    CALL FILERR(SCFSFN(1,DAF),2,ST,I)
	    GOTO 10
	  END IF
	  IF(DAFSTS.GT.DSOPEN) THEN
	    WRITE(5,908) (DATE(K),K=7,13)
	    VERR = VERR + 1
	  END IF
	  IF (DAFDRW(GNUM) .NE. 0 .AND. DAFDRW(GNUM) .NE. DRW) THEN
	    WRITE(5,909) (DATE(K),K=7,13),GIND,DAFDRW(GNUM)
	    VERR = VERR + 1
	  END IF 
1020	CONTINUE

	IF (VERR .NE. 0) THEN
	  TYPE*,IAM(),VERR,' game date errors for ',CPINT_NAME,
     *          GIND,' event ',DRW
	  TYPE*,IAM(),'Event has not been verified'
	  CALL WIMG(5,'Hit return to continue')
	  READ(5,901) ANS
	  GOTO 10
	END IF

	DCPSTS = GAMOPN
	DO 1030 I = 1,MAXCPLRW
	  IF (DCPSTA(I) .EQ. GAMINF) DCPSTA(I) = GAMOPN
1030	CONTINUE

	DO 1040 I = DCPBSD,DCPESD
	  CALL READW(DFDB,I,DAFREC,ST)
	  IF (ST .NE. 0) THEN
	    CALL FILERR(SCFSFN(1,DAF),2,ST,I)
	    GOTO 10
	  END IF
	  DAFDRW(GNUM) = DRW
	  CALL WRITEW(DFDB,I,DAFREC,ST)
	  IF (ST .NE. 0) THEN
	    CALL FILERR(SCFSFN(1,DAF),3,ST,I)
	    GOTO 10
	  END IF
1040	CONTINUE

	 CALL WRITEW(FDB,
     *               DRW,
     *               DCPREC,
     *               ST)

	 IF (ST .NE. 0) THEN
	   CALL FILERR(SCFGFN(1,GNUM),3,ST,DRW)
	   GOTO 10
	 END IF

	 TYPE*,IAM(),CPINT_NAME,GIND,' event ',DRW,' verify complete'

	 CALL WIMG(5,'Hit return to continue')

	 READ(5,901) ANS

	 GOTO 10

2000	CONTINUE

	CALL CLRSCR(5)

	CALL PASSWORD(5,PWORD)

	IF (PASS .NE. OKPASS) THEN
	  TYPE*,IAM(),'Invalid password entered'
	  CALL XWAIT(2,2,ST)
	  GOTO 10
	END IF

 	IF (DCPSTS .LE. GAMINF) THEN
	  TYPE*,IAM(),CPINT_NAME,GIND,' event ',DRW,' has not been verified '
	  CALL WIMG(5,'Hit return to continue')
	  READ(5,901) ANS
	  GOTO 10
	END IF

	IF (DCPSTS .GT. GAMOPN) THEN
	  TYPE*,IAM(),CPINT_NAME,GIND,' event ',DRW,' has already ended'
	  CALL WIMG(5,'Hit return to continue')
	  READ(5,901) ANS
	  GOTO 10
	END IF

	DO 2010 I = DCPBSD,DCPESD
	  CALL READW(DFDB,I,DAFREC,ST)
	  IF (DAFSTS .GT. GAMOPN) THEN
	    TYPE*,IAM(),CPINT_NAME,GIND,' currently active, cannot be changed'
	    CALL WIMG(5,'Hit return to continue')
	    READ(5,901) ANS
	    GOTO 10
	  ENDIF
2010	CONTINUE

	DCPSTS = GAMINF
	DO 2020 I = 1,MAXCPLRW
	  IF (DCPSTA(I) .EQ. GAMOPN) DCPSTA(I) = GAMINF
2020	CONTINUE

	DO 2030 I = DCPBSD,DCPESD
	  CALL READW(DFDB,I,DAFREC,ST)
	  IF (ST .NE. 0) THEN
	    CALL FILERR(SCFSFN(1,DAF),2,ST,I)
	    GOTO 10
	  END IF
	  DAFDRW(GNUM) = 0
	  CALL WRITEW(DFDB,I,DAFREC,ST)
	  IF (ST .NE. 0) THEN
	    CALL FILERR(SCFSFN(1,DAF),3,ST,I)
	  END IF
2030	CONTINUE

	CALL WRITEW(FDB,DRW,DCPREC,ST)

	IF (ST .NE. 0) THEN
	  CALL FILERR(SCFGFN(1,GNUM),3,ST,DRW)
	  GOTO 10
	END IF

	TYPE*,IAM(),CPINT_NAME,GIND,' event ',DRW,
     *	     'is un-verified and can be modified'

	CALL WIMG(5,'Hit return to continue')

	READ(5,901) ANS

	GOTO 10

C---- Check and validate data file.

3000	CONTINUE

	IF ((FUN .EQ. 'CH') .AND. (DCPSTS .GE. GAMOPN)) THEN
	  CALL CLRSCR(5)
          TYPE*,IAM(),CPINT_NAME,GIND,' EVENT ',DRW,
     *        ' DATA HAS BEEN VERIFIED.'
	  TYPE*,IAM(),' CHANGES CANNOT BE MADE UNLESS EVENT IS UNVERIFIED.'
	  CALL WIMG(5,'HIT RETURN TO CONTINUE.')
	  READ(5,901) ANS
	  GOTO 10
	END IF

	CALL CPLCOMP(FDB,VFDB,GIND,DRW,GNUM,ST)

	IF (ST .NE. 0) TYPE*,IAM(),' ERROR IN WICOMP, ST IS: ',ST

	GOTO 20


C------------------  Format statements. ----------------------------------

900	FORMAT(' LI - List event data',/,
     *	       ' MO - Modify event data',/,
     *	       ' VE - Verify event data',/,
     *	       ' UN - Unverify event data',/,
     *	       ' CH - Check event data',/,
     *	       ' EX - Return to main menu',/)

901	FORMAT(A2)

902	FORMAT(3A4)

903	FORMAT(1X,A8,I1,1X,'Draw ',I4.4,1X,'Week ',I2.2,
     *	       1X,'*',A12,'*',' Name ',<CPLENM_LEN/4>A4)

9031	FORMAT(2X,<CPLENM_LEN/4>A4,12X,<CPLENM_LEN/4>A4)

904	FORMAT(' Sales dates  ',7A2,'  Cdc - ',I4,
     *	               '  < to >  ',7A2,'  Cdc - ',I4)

905	FORMAT(1X,'Time ',A8,1X,
     *	       ' Event date ',7A2,'  Cdc - ',I4)

9051	FORMAT(' Row      Name',12X,' Row      Name')

906	FORMAT(1X,I2.2,1X,<CPLNMS_LEN/4>A4,
     *	       7X,I2.2,1X,<CPLNMS_LEN/4>A4)

907	FORMAT(A4)

908	FORMAT(1X,7A2,' has an invalid day status ')

909	FORMAT(1X,7A2,' is already active for Today Couple',
     *	       I1,' event ',I4)

910	FORMAT(1X,'Pool file name ',5A4,' TV-Chanel Name:',
     *	       <CPLTVC_LEN/4>A4)

911	FORMAT(4X,':C',I1,'P',I4.4,'.FIL    ')

912	FORMAT(A)

913	FORMAT(5A4)

914	FORMAT(1X,'( ',30A1' )')

9141	FORMAT(1X,'( ',30A1' )',10X,'(',30A1,')')

915     FORMAT(1X,'Base price ',A10)

916     FORMAT(1X,'Pool percentage ',F8.3)

917	FORMAT(1X,A,'Begining sales date')

918	FORMAT(1X,A,'Current is, (E-no change)...............',7A2)

919	FORMAT(1X,A,'Ending sales date')

921	FORMAT(1X,A,'Close time')

922	FORMAT(1X,A,'Current is, (E-no change)...............',A8)

923	FORMAT(1X,A,'Event date')

924	FORMAT(1X,A,'Current is, (E-no change)...............',7A2)

925	FORMAT(1X,A,'Base price')

926	FORMAT(1X,A,'Current: ',A10,' (E-no change)')

927	FORMAT(1X,A,'Pool percentage')

928	FORMAT(1X,A,'Current: ',F8.3,' (E-no change)')

929	FORMAT(1X,A,'Pool file volume name')

930	FORMAT(1X,A,'Current is, (RETURN for System Volume)..',3A4)

931	FORMAT(1X,A,'Event 1 name')

932	FORMAT(1X,A,'(RETURN no change /+ clear)',<CPLENM_LEN/4>A4)

9311	FORMAT(1X,A,'Event 2 name')

933	FORMAT(<CPLENM_LEN>A4)

934	FORMAT(1X,A,'Discription lines (RETURN no change /+ clear)')

935	FORMAT(1X,A,'Description Line<',I1,'>',30A1)

936	FORMAT(30A1)

937	FORMAT(1X,A,'TV-Chanel Name')

938	FORMAT(1X,A,'Current is, (RETURN no change /+ clear)',
     *         <CPLTVC_LEN/4>A4)

939	FORMAT(<CPLTVC_LEN/4>A4)

940	FORMAT(1X,A,'Team name')

941	FORMAT(1X,A,'Current is, (RETURN no change /+ clear)',
     *         <CPLNMS_LEN/4>A4)

942	FORMAT(<CPLNMS_LEN/4>A4)
944     FORMAT(1X,A,'Current stake .....................',A10)
945     FORMAT(1X,'Minimum stake   ',A10)

990	FORMAT(A8)

991	FORMAT(1X,A,'Sorry, ',A8,1X,I1,' game not active')

992	FORMAT(1X,A,A8,1X,I1,' draw ',I4,' has been verified',/,
     *         1X,A,A8,'Changes can not be made unless event ',
     *                 'is UNverified')

99998   FORMAT(1X,A,<CPLDES_LEN/4>A4)

99999	FORMAT(1X,A,A12,Z8)	



	END
