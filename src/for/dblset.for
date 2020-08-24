C  GXSRC:DBLSET.FOR
C
C V10 08-SEP-1999 UXN DDBRWS added.
C V09 24-MAY-1999 UXN Minimum stake added.
C V08 05-JUN-1997 UXN Calculation of REV3 changed.
C V07 14-APR-1996 HXK Putting RXK's (Rita's) changes into archive
C V06 03-APR-1996 RXK Calculation of control revision byte changed
C V05 22-FEB-1996 HXK Fix for blank lines in description
C V04 21-FEB-1996 HXK Fixes for blank descriptions
C V03 18-FEB-1996 HXK Changed method of updating First revision byte such 
C                     that it changes from draw to draw
C V02 05-DEC-1995 HXK Changed WxPnnnn.FIL to DxPnnnn.FIL
C V01 23-NOV-1995 PXB Initial revision.  
C
C SUBROUTINE DBLSET
C
C SUPER DOUBLE GAME SETUP
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
C Copyright 1995 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C=======OPTIONS /CHECK=NOOVERFLOW

	SUBROUTINE DBLSET(GIND)

	IMPLICIT NONE

C---- Include files used.

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:DDBREC.DEF'
	INCLUDE 'INCLIB:RECDAF.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'

C---- Local variables used.

	COMMON SCFREC
	INTEGER*4    DFDB(7),FDB(7),VFDB(7),INPFDB(7),BYTTAB(200)
	INTEGER*2    BDATE(LDATE_LEN),EDATE(LDATE_LEN),DATE(LDATE_LEN)
        INTEGER*2    DDATE(LDATE_LEN)
	INTEGER*4    GNUM,GIND,ST,I,DRW,EXT,LSTAT,WEK,J,YEAR
	INTEGER*4    K,BROW,EROW,CDC,ROW,BUFIND,REV1,REV2,REV3,REV4
	INTEGER*4    PREV3,VERR,SALPER,PRICE
	INTEGER*4    CLEARED
	INTEGER*4    DESCRIPT_LINES
	INTEGER*4    DSCIND
	CHARACTER*6  PASS,OKPASS
	CHARACTER*8  DBINT_NAME
	CHARACTER*20 PWORD,CDDBPFN
	EQUIVALENCE  (PASS,PWORD)
	CHARACTER*2  FUN
	INTEGER*4    NAME(DBLENM_LEN/4),ENAME(DBLENM_LEN/4)
	INTEGER*4    DNAME(DBLDES_LEN/4)
	CHARACTER    INPFIL,ANS
	CHARACTER    CENAME(DBLENM_LEN),CNAME(DBLENM_LEN)
	CHARACTER    CDNAME(DBLDES_LEN)
	BYTE         BENAME(DBLENM_LEN),BNAME(DBLENM_LEN)
	BYTE         BDNAME(DBLDES_LEN)
	INTEGER*4    TEMP		!used to restore to old values
	EQUIVALENCE  (DDBPFN,CDDBPFN)
	EQUIVALENCE  (NAME,CNAME,BNAME)
	EQUIVALENCE  (DNAME,CDNAME,BDNAME)
	EQUIVALENCE  (ENAME,CENAME,BENAME)
	CHARACTER*12 GSTAT(11)
	INTEGER*4    FLAG

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

	WRITE(DBINT_NAME,990) GTNAMES(TDBL)

	DESCRIPT_LINES = DBLDES_LEN/30		!30 Bytes / line

C---- Check if super double game is active

	GNUM = SCFGTN(TDBL,GIND)

	IF (GNUM .LT. 1) THEN
	  WRITE(5,991) IAM(),GTNAMES(TDBL),GIND
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
     *              DDBSEC*256)

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
     *              DDBSEC*256)

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

	IF (FUN .NE. 'LI' .AND. FUN .NE. 'MO' .AND.
     *	    FUN .NE. 'VE' .AND. FUN .NE. 'UN' .AND.
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
	  IF(INPFIL.EQ.'E') GOTO 10
	  IF(INPFIL.EQ.'V') THEN
	     CALL FASTMOV(VFDB,INPFDB,7)
	  ELSE
	     IF(INPFIL.NE.'P')THEN
	       TYPE*,'OPTIONS ARE P, V, AND E ONLY...RETRY.'
	       GOTO 15
	     ENDIF
	  ENDIF
	ENDIF


20	CONTINUE

	IF (FUN .NE. 'LI') CALL CLRSCR(5)

	CALL INPNUM('Enter event number(E-exit) ',DRW,-9999,9999,EXT)

	IF (EXT .LT. 0 .OR. DRW .EQ. 0) GOTO 10

	IF (DRW .LT. 1) TSTFLG = .TRUE.

	DRW = ABS(DRW)
C
C	Get previous text checksum
C	
	PREV3 = 0
	IF(DRW.GT.1) THEN
	  CALL READW(INPFDB,DRW-1,DDBREC,ST)

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
	  CALL ILBYTE(PREV3,DDBREV,2)          !GET PREVIOUS TEXT REV #
       ENDIF

C
C
C
	CALL CLRSCR(5)

	CALL READW(INPFDB,
     *             DRW,
     *             DDBREC,
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

	LSTAT = DDBSTS + 1
	DO 30 I = 1,7
	  BDATE(I+6) = DAY(I)
	  EDATE(I+6) = DAY(I)
	  DDATE(I+6) = DAY(I)
30	CONTINUE

	BDATE(5) = DDBBSD
	IF (BDATE(5) .NE. 0) CALL LCDATE(BDATE)

	EDATE(5) = DDBESD
	IF (EDATE(5) .NE. 0) CALL LCDATE(EDATE)

	DDATE(5) = DDBDAT
	IF (DDATE(5) .NE. 0) CALL LCDATE(DDATE)

	WEK = DDBWEK
	SALPER = DDBSPR
	PRICE = DDBPRC

	IF (FUN .EQ. 'MO' .AND. DDBSTS .GE. GAMOPN) THEN
	  CALL CLRSCR(5) 
	  WRITE(5,992) IAM(),GTNAMES(TDBL),GIND,DRW,IAM()
	  CALL WIMG(5,'Hit return to continue')
	  READ(5,901) ANS
	  GOTO 10
	END IF
 
	DO 80 J = 1,MAXDBLRW
	  DO 70 K = 1,DBLNMS_LEN/4
	    IF (DDBNMS(K,J) .EQ. 0) DDBNMS(K,J) = '    '
70	  CONTINUE
80	CONTINUE


	DO 90 I = 1,DBLENM_LEN/4
	  IF (DDBENM(I) .EQ. 0) DDBENM(I) = '    '
90	CONTINUE


	IF (FUN .EQ. 'MO') GOTO 120
	IF (FUN .EQ. 'VE') GOTO 1000
	IF (FUN .EQ. 'UN') GOTO 2000
	IF (FUN .EQ. 'CH') GOTO 3000

C---- Display game data.

	BROW = 1
	EROW = 19 - DESCRIPT_LINES

105	CONTINUE

	CALL CLRSCR(5)

	WRITE(5,903) GTNAMES(TDBL),GIND,DRW,WEK,GSTAT(LSTAT),
     *		     (DDBENM(K),K=1,DBLENM_LEN/4)

	DSCIND = 0

	CALL FASTMOV(DDBDES,DNAME,DBLDES_LEN/4)

	DO I = 1,DESCRIPT_LINES
	   WRITE(5,914) (CDNAME(K),K=DSCIND+1,DSCIND+30)
	   DSCIND = DSCIND + 30
	END DO

        WRITE(5,904) 
     *	(BDATE(I),I=7,13),BDATE(5),(EDATE(I),I=7,13),EDATE(5)

	WRITE(5,905) DISTIM(DDBTIM),(DDATE(K),K=7,13),DDATE(5)
        WRITE(5,945) CMONY(DDBPRC,10,BETUNIT)

	WRITE(5,910) (DDBPFN(K),K=1,5), (DDBTVC(K),K=1,DBLTVC_LEN/4)

	IF(DDBPCC.EQ.1) THEN
	    TYPE*,'*** NB! XX-17 and 18-XX combinations are NOT playable ***'
	ENDIF

	DO 110 I=BROW,EROW
	  WRITE(5,906) I,(DDBNMS(J,I),J=1,DBLNMS_LEN/4)
110	CONTINUE

	IF (EROW .NE. MAXDBLRW) THEN
	  CALL WIMG(5,'Hit return to see remaining rows')
	  READ(5,901) ANS
	  BROW = EROW+1
	  EROW = EROW+19-DESCRIPT_LINES    
	  IF (EROW .GT. MAXDBLRW) EROW = MAXDBLRW
	  GOTO 105
	END IF
	IF (TSTFLG) THEN
	  WRITE(5,99999) IAM(),'REVISION:   ',DDBREV
	  WRITE(5,99998) IAM(),'DESCRIPT:   ',(DDBDES(K),K=1,DBLDES_LEN/4)
	END IF

	GOTO 20

C---- Modify game date

120	CONTINUE

	WRITE(5,917) IAM()

	WRITE(5,918) IAM(),(BDATE(I),I=7,13) 

	CALL INPDAT(CDC,EXT)

	IF (EXT .NE. 0) CDC = DDBBSD

	BDATE(5) = CDC

	CALL LCDATE(BDATE)

	WRITE(5,919) IAM()

	WRITE(5,918) IAM(),(EDATE(I),I=7,13) 

	CALL INPDAT(CDC,EXT)

	IF (EXT .NE. 0) CDC = DDBESD

	EDATE(5)=CDC

	CALL LCDATE(EDATE)

	CALL FIGWEK(CDC-WEEK_OFFSET,WEK,YEAR)

	WRITE(5,923) IAM()

	WRITE(5,924) IAM(),(DDATE(I),I=7,13)

	CALL INPDAT(CDC,EXT)

	IF (EXT .NE. 0) CDC = DDBDAT

	DDATE(5) = CDC

	CALL LCDATE(DDATE)

	WRITE(5,921) IAM()

	WRITE(5,922) IAM(),DISTIM(DDBTIM)

	TEMP = DDBTIM

	CALL INPTIM('Enter time HH:MM:SS....................',DDBTIM,EXT)

	IF (EXT .NE. 0) DDBTIM = TEMP 
        WRITE(5,944) IAM(), CMONY(PRICE,10,BETUNIT)
        CALL INPMONY('Enter minimum stake.........',PRICE,BETUNIT,EXT)
        IF(EXT.NE.0) DDBPRC=PRICE

	WRITE(5,929) IAM()

	WRITE(5,930) IAM(),DDBPFN(1)

	WRITE (CDDBPFN,911) GIND,DRW

	CALL WIMG(5,'Enter pool file volume name............')

	READ(5,907) NAME(1)

	IF (NAME(1) .NE. '    ') DDBPFN(1)=NAME(1)

	WRITE(5,931) IAM()

	WRITE(5,932) IAM(),(DDBENM(K),K=1,DBLENM_LEN/4)

	CALL WIMG(5,'Enter event name.......................')

	CLEARED = 0

	READ(5,933) ENAME

C---- Did he/she hit return?

	DO I = 1,DBLENM_LEN/4
	  IF(ENAME(I).EQ.'    ') CLEARED = CLEARED + 1
	END DO

	IF (CLEARED .EQ. DBLENM_LEN/4) GOTO 140

C---- Does he want us to clear it?

	IF (CENAME(1) .EQ. '+') THEN
	   DO I = 1,DBLENM_LEN/4
	     ENAME(I) = '    '
	   END DO
	END IF

C---- Replace nulls with spaces

        DO I = 1,DBLENM_LEN
           IF(BENAME(I) .EQ. 0) BENAME(I) = ' '
        ENDDO

C---- Move the input over.

	DO I=1,DBLENM_LEN/4
	  DDBENM(I) = ENAME(I)
	END DO

140	CONTINUE

	WRITE(5,934) IAM()

	DSCIND = 0

	CALL FASTMOV(DDBDES,DNAME,DBLDES_LEN/4)

	DO I = 1,DESCRIPT_LINES
	  WRITE(5,935) IAM(),I,(CDNAME(K),K=DSCIND+1,DSCIND+30)
	  DSCIND = DSCIND + 30
	END DO

	TYPE*,IAM()

	DSCIND = 0

	CALL WIMG(5,'Description Length<                             ')

	DO I = 1,DESCRIPT_LINES
	  CALL WIMG(5,'Enter description')
	  READ(5,936) (CDNAME(K),K=DSCIND+1,DSCIND+30)
	  DSCIND = DSCIND + 30
	END DO

C---- Did he/she hit return?

        CLEARED = 0
        DO I = 1,DBLDES_LEN/4
           IF (DNAME(I) .EQ. '    ') CLEARED = CLEARED + 1
        END DO
        IF (CLEARED .EQ. DBLDES_LEN/4) THEN
           CLEARED = 0
           DO I = 1,DBLDES_LEN/4
              IF (DDBDES(I) .EQ. 0) CLEARED = CLEARED + 1
           END DO
           IF (CLEARED .EQ. DBLDES_LEN/4) THEN
              DO I = 1,DBLDES_LEN/4
                 DDBDES(I) = '    '
              ENDDO
           ENDIF
           GOTO 165
        ENDIF

C---- Does he want us to clear it?

	IF (CDNAME(1) .EQ. '+') THEN
	  DO I = 1,DBLDES_LEN/4
	    DNAME(I) = '    '
	  END DO
	END IF

C---- Replace nulls with spaces

        DO I = 1,DBLDES_LEN
           IF(BDNAME(I) .EQ. 0) BDNAME(I) = ' '
        ENDDO

C---- Move the description over.

	DO I = 1,DBLDES_LEN/4
	  DDBDES(I) = DNAME(I)
	END DO


165	CONTINUE

	WRITE(5,937) IAM()

	WRITE(5,938) IAM(),(DDBTVC(K),K=1,DBLTVC_LEN/4)

	CALL WIMG(5,'Enter TV-Chanel Name...................')

	READ(5,939) (NAME(K),K=1,DBLTVC_LEN/4) 

C---- Did he/she hit return?

	CLEARED = 0

	DO I = 1,DBLTVC_LEN/4
	  IF (NAME(I) .EQ. '    ') CLEARED = CLEARED + 1
	END DO

	IF (CLEARED .EQ. DBLTVC_LEN/4) GOTO 300

C---- Does he want us to clear it?

	IF (CNAME(1) .EQ. '+') THEN
	  DO I = 1,DBLTVC_LEN/4
	    NAME(I) = '    '
	  END DO
	END IF

C---- Move the name over.

	DO I = 1,DBLTVC_LEN/4
	  DDBTVC(I) = NAME(I)
	END DO

300	CONTINUE

	TYPE*,IAM()

	CALL INPNUM('Enter row number to be modified (E-Exit)',
     *	      ROW,1,MAXDBLRW,EXT)

	IF (EXT .LT. 0) GOTO 500

	WRITE(5,940) IAM()

	WRITE(5,941) IAM(),(DDBNMS(K,ROW),K=1,DBLNMS_LEN/4)

	CALL WIMG(5,'Enter Team Name........................')

	READ(5,942) (NAME(K),K=1,DBLNMS_LEN/4)

C---- Did he/she hit return?

	CLEARED = 0

	DO I = 1,DBLNMS_LEN/4
	   IF (NAME(I) .EQ. '    ') CLEARED = CLEARED + 1
	END DO

	IF (CLEARED .EQ. DBLNMS_LEN/4) GOTO 210

C---- Does he want us to clear it?

	IF (CNAME(1) .EQ. '+') THEN
	   DO I = 1,DBLNMS_LEN/4
	     NAME(I) = '    '
	   END DO
	END IF

C---- Replace nulls with spaces

        DO I = 1,DBLNMS_LEN
           IF(BNAME(I) .EQ. 0) BNAME(I) = ' '
        ENDDO

C---- Move the name over.

	DO I = 1,DBLNMS_LEN/4
	  DDBNMS(I,ROW) = NAME(I)
	END DO

210	CONTINUE

	GOTO 300

500	CONTINUE
C
	DDBBSD = BDATE(5)
	DDBESD = EDATE(5)
	DDBDAT = DDATE(5)
	DDBWEK = WEK
	DDBSTS = GAMINF
	DDBDRW = DRW
	DDBSPR = SALPER
	DDBPRC = PRICE

	TEMP = 0
	DO 510 I = 1,MAXDBLRW
	  DDBSTA(I) = 0
	  IF (DDBNMS(1,I) .NE. '    ') THEN
	    DDBSTA(I) = GAMINF
	    TEMP = TEMP + 1
	  ENDIF
510	CONTINUE
	DDBRWS = TEMP
C
C Ask for 17/18 i.e. first other/second other rule.
C
	IF(TEMP.EQ.18) THEN
	  IF(DDBPCC.EQ.0) THEN
	    TYPE*,IAM(),'XX-17 and 18-XX combinations are playable'
	  ELSEIF(DDBPCC.EQ.1) THEN
	    TYPE*,IAM(),'XX-17 and 18-XX combinations are NOT playable'
	  ENDIF
	  CALL INPYESNO('Do you want to disable cmb. XX-17 and 18-XX [Y/N] ?',
     *                   FLAG)
	  IF(FLAG.EQ.1) THEN
	    DDBPCC = 1
	  ELSE
	    DDBPCC = 0
	  ENDIF
	ELSE
	  DDBPCC = 0
	ENDIF
C---- Create table of text message to checksum for rev.

	BUFIND = 1

C---- Load up event name and event descriptions

	CALL MOVBYT(DDBENM(1),
     *              1,
     *              BYTTAB,
     *              BUFIND,
     *              DBLENM_LEN)

	BUFIND = BUFIND + DBLENM_LEN

	CALL MOVBYT(DDBDES(1),
     *              1,
     *              BYTTAB,
     *              BUFIND,
     *              DBLDES_LEN)

	BUFIND = BUFIND + DBLDES_LEN

C---- Load up player names.

	DO I = 1,MAXDBLRW                     !FOR ALL ROWS
	   IF (DDBNMS(1,I) .NE. '    ') THEN
	     CALL MOVBYT(DDBNMS(1,I),
     *			 1,
     *			 BYTTAB,
     *			 BUFIND,
     *			 DBLNMS_LEN-2)
	     BUFIND = BUFIND + (DBLNMS_LEN-2)
	   END IF
	END DO

	BUFIND = BUFIND - 1

	CALL CHECKSUM(BYTTAB,1,BUFIND,REV4)

	CALL ILBYTE(REV1,DDBREV,0)	    

        IF(DDBDRW-1.EQ.M251) THEN
           REV1 = MOD(REV1+DDBDRW,(M251-10)) + 1
        ELSE
           REV1 = MOD(REV1+DDBDRW,M251) + 1
        ENDIF
	REV2 = MOD(DDBDRW,255)

	CALL ILBYTE(REV3,DDBREV,2)          !GET PREVIOUS TEXT REV #

	REV3 = MOD(PREV3 + REV3,255) + 1

	CALL ISBYTE(REV1,DDBREV,0)          !CONTROL REV BYTE (SEQUENCE#)
	CALL ISBYTE(REV2,DDBREV,1)          !DRAW REV BYTE
	CALL ISBYTE(REV3,DDBREV,2)          !TEXT REV # BYTE  (SEQUENCE#)
	CALL ISBYTE(REV4,DDBREV,3)          !TEXT CHECKSUM BYTE

	CALL WRITEW(INPFDB,
     *		    DRW,
     *              DDBREC,
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

	IF (DDBSTS .EQ. GAMOPN) THEN
	  TYPE*,IAM(),DBINT_NAME,GIND,' event ',DRW,' already verified'
	  CALL WIMG(5,'Hit return to continue')
	  READ(5,901) ANS
	  GOTO 10
	END IF

	IF (DDBSTS .NE. GAMINF) THEN
	  TYPE*,IAM(),DBINT_NAME,GIND,' event ',DRW,' invalid status ',
     *          DDBSTS
	  CALL WIMG(5,'Hit return to continue')
	  READ(5,901) ANS
	  GOTO 10
	END IF

	IF (DDBBSD .GT. DDBESD) THEN
	  TYPE*,IAM(),'Begining sales date greater then ending sales date'
	  VERR = VERR + 1
	END IF

	IF (DDBBSD .EQ. 0) THEN
	  TYPE*,IAM(),'Begining sales date not set '
	  VERR = VERR + 1
	END IF

	IF (DDBDAT .LT. DDBESD) THEN
	  TYPE*,IAM(),'Event date is before last sales date'
	  VERR = VERR + 1
	END IF

	IF (DDBESD .EQ. 0) THEN
	  TYPE*,IAM(),'Ending sales date not set'
	  VERR = VERR + 1
	END IF

	IF (DDBPRC .EQ. 0) THEN
	  TYPE*,IAM(),'Base price not set'
	  VERR = VERR + 1
	END IF

	IF (DDBSPR .EQ. 0) THEN
	  TYPE*,IAM(),'Pool percentage not set'
	  VERR = VERR + 1
	END IF

	IF (VERR .NE. 0) THEN
          TYPE*,IAM(),VERR,' data entry errors for ',DBINT_NAME,
     *        GIND,' event ',DRW
	  TYPE*,IAM(),'Event has not been verified'
	  CALL WIMG(5,'Hit return to continue')
	  READ(5,901) ANS
	  GOTO 10
	END IF

	DO 1020 I = DDBBSD,DDBESD
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
	  TYPE*,IAM(),VERR,' game date errors for ',DBINT_NAME,
     *          GIND,' event ',DRW
	  TYPE*,IAM(),'Event has not been verified'
	  CALL WIMG(5,'Hit return to continue')
	  READ(5,901) ANS
	  GOTO 10
	END IF

	DDBSTS = GAMOPN
	DO 1030 I = 1,MAXDBLRW
	  IF (DDBSTA(I) .EQ. GAMINF) DDBSTA(I) = GAMOPN
1030	CONTINUE

	DO 1040 I = DDBBSD,DDBESD
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
     *               DDBREC,
     *               ST)

	 IF (ST .NE. 0) THEN
	   CALL FILERR(SCFGFN(1,GNUM),3,ST,DRW)
	   GOTO 10
	 END IF

	 TYPE*,IAM(),DBINT_NAME,GIND,' event ',DRW,' verify complete'

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

 	IF (DDBSTS .LE. GAMINF) THEN
	  TYPE*,IAM(),DBINT_NAME,GIND,' event ',DRW,' has not been verified '
	  CALL WIMG(5,'Hit return to continue')
	  READ(5,901) ANS
	  GOTO 10
	END IF

	IF (DDBSTS .GT. GAMOPN) THEN
	  TYPE*,IAM(),DBINT_NAME,GIND,' event ',DRW,' has already ended'
	  CALL WIMG(5,'Hit return to continue')
	  READ(5,901) ANS
	  GOTO 10
	END IF

	DO 2010 I = DDBBSD,DDBESD
	  CALL READW(DFDB,I,DAFREC,ST)
	  IF (DAFSTS .GT. GAMOPN) THEN
	    TYPE*,IAM(),DBINT_NAME,GIND,' currently active, cannot be changed'
	    CALL WIMG(5,'Hit return to continue')
	    READ(5,901) ANS
	    GOTO 10
	  ENDIF
2010	CONTINUE

	DDBSTS = GAMINF
	DO 2020 I = 1,MAXDBLRW
	  IF (DDBSTA(I) .EQ. GAMOPN) DDBSTA(I) = GAMINF
2020	CONTINUE

	DO 2030 I = DDBBSD,DDBESD
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

	CALL WRITEW(FDB,DRW,DDBREC,ST)

	IF (ST .NE. 0) THEN
	  CALL FILERR(SCFGFN(1,GNUM),3,ST,DRW)
	  GOTO 10
	END IF

	TYPE*,IAM(),DBINT_NAME,GIND,' event ',DRW,
     *	     'is un-verified and can be modified'

	CALL WIMG(5,'Hit return to continue')

	READ(5,901) ANS

	GOTO 10

C---- Check and validate data file.

3000	CONTINUE

	IF ((FUN .EQ. 'CH') .AND. (DDBSTS .GE. GAMOPN)) THEN
	  CALL CLRSCR(5)
          TYPE*,IAM(),DBINT_NAME,GIND,' EVENT ',DRW,
     *        ' DATA HAS BEEN VERIFIED.'
	  TYPE*,IAM(),' CHANGES CANNOT BE MADE UNLESS EVENT IS UNVERIFIED.'
	  CALL WIMG(5,'HIT RETURN TO CONTINUE.')
	  READ(5,901) ANS
	  GOTO 10
	END IF

	CALL DBLCOMP(FDB,VFDB,GIND,DRW,GNUM,ST)

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
     *	       1X,'*',A14,'*',' Name ',<DBLENM_LEN/4>A4)

904	FORMAT(' Sales dates  ',7A2,'  Cdc - ',I4,
     *	               '  < to >  ',7A2,'  Cdc - ',I4)

905	FORMAT(' Row      Name',12X,'Time ',A8,1X,
     *	 ' Event date ',7A2,'  Cdc - ',I4)

906	FORMAT(1X,I2.2,1X,<DBLNMS_LEN/4>A4)

907	FORMAT(A4)

908	FORMAT(1X,7A2,' has an invalid day status ')

909	FORMAT(1X,7A2,' is already active for Super Double',
     *	       I1,' event ',I4)

910	FORMAT(1X,'Pool file name ',5A4,' TV-Chanel Name:',
     *	       <DBLTVC_LEN/4>A4)

911	FORMAT(4X,':D',I1,'P',I4.4,'.FIL    ')

912	FORMAT(A)

913	FORMAT(5A4)

914	FORMAT(1X,'( ',30A1' )')

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

931	FORMAT(1X,A,'Event name')

932	FORMAT(1X,A,'(RETURN no change /+ clear)',<DBLENM_LEN/4>A4)

933	FORMAT(<DBLENM_LEN>A4)

934	FORMAT(1X,A,'Discription lines (RETURN no change /+ clear)')

935	FORMAT(1X,A,'Description Line<',I1,'>',30A1)

936	FORMAT(30A1)

937	FORMAT(1X,A,'TV-Chanel Name')

938	FORMAT(1X,A,'Current is, (RETURN no change /+ clear)',
     *         <DBLTVC_LEN/4>A4)

939	FORMAT(<DBLTVC_LEN/4>A4)

940	FORMAT(1X,A,'Team name')

941	FORMAT(1X,A,'Current is, (RETURN no change /+ clear)',
     *         <DBLNMS_LEN/4>A4)

942	FORMAT(<DBLNMS_LEN/4>A4)

990	FORMAT(A8)

991	FORMAT(1X,A,'Sorry, ',A8,1X,I1,' game not active')

992	FORMAT(1X,A,A8,1X,I1,' draw ',I4,' has been verified',/,
     *         1X,A,A8,'Changes can not be made unless event ',
     *                 'is UNverified')
944     FORMAT(1X,A,'Current stake .....................',A10)
945     FORMAT(1X,'Minimum stake   ',A10)
99998   FORMAT(1X,A,<DBLDES_LEN/4>A4)

99999	FORMAT(1X,A,A12,Z8)	



	END
