C
C SUBROUTINE TRPSET
C
C V02 10-MAY-1999 UXN TRIPLE changed to TRIO.  
C V01 12-JAN-1998 RXK Initial release.
C
C TODAYS TRIO GAME SETUP
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
C Copyright 1997 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE TRPSET(GIND)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:DTRREC.DEF'
	INCLUDE 'INCLIB:RECDAF.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
C
	COMMON SCFREC
	INTEGER*4    DFDB(7),FDB(7),VFDB(7),INPFDB(7),BYTTAB(1000)
	INTEGER*2    BDATE(LDATE_LEN),EDATE(LDATE_LEN),DATE(LDATE_LEN)
        INTEGER*2    DDATE(LDATE_LEN)
        INTEGER*2    EVDATE(LDATE_LEN,3)
	INTEGER*4    GNUM,GIND,ST,I,DRW,EXT,LSTAT,WEK,J,L
	INTEGER*4    K,CDC,ROW,BUFIND,REV1,REV2,REV3,REV4
	INTEGER*4    WHICH_EVENT,EVTIME,YEAR
	INTEGER*4    PREV3,VERR,SALPER,PRICE
	INTEGER*4    CLEARED
	INTEGER*4    DESCRIPT_LINES
	INTEGER*4    DSCIND
	CHARACTER*6  PASS,OKPASS
	CHARACTER*8  TRINT_NAME
	CHARACTER*20 PWORD,CDTRPFN
	EQUIVALENCE  (PASS,PWORD)
	CHARACTER*2  FUN
	INTEGER*4    NAME(TRPENM_LEN/4),ENAME(TRPENM_LEN/4)
	INTEGER*4    DNAME(TRPDES_LEN/4,3)
	CHARACTER    INPFIL,ANS
	CHARACTER    CENAME(TRPENM_LEN),CNAME(TRPENM_LEN)
	CHARACTER    CDNAME(TRPDES_LEN,3)
	BYTE         BENAME(TRPENM_LEN),BNAME(TRPENM_LEN)
	BYTE         BDNAME(TRPDES_LEN,3)
	INTEGER*4    TEMP		!used to restore to old values
	EQUIVALENCE  (DTRPFN,CDTRPFN)
	EQUIVALENCE  (NAME,CNAME,BNAME)
	EQUIVALENCE  (DNAME,CDNAME,BDNAME)
	EQUIVALENCE  (ENAME,CENAME,BENAME)
	CHARACTER*12 GSTAT(11)
C
	DATA GSTAT   /'Not set     ','Game closed ',
     *	              'Info entered','Game open   ',
     *	            3*'End of game ',
     *	              'Drawing done','Game final  ',
     *	              'Cancelled   ','Refund      '/
C
	CHARACTER*14 CDAY
	INTEGER*2    DAY(7)
	LOGICAL	     TSTFLG
	DATA	     CDAY/'---not set--- '/
	DATA	     OKPASS/'PERKIN'/
	EQUIVALENCE  (CDAY,DAY)

        INTEGER*4 M251
        PARAMETER(M251=251)
C
C Initialize / set variables 
C
	WRITE(TRINT_NAME,990) GTNAMES(TTRP)
C
	DESCRIPT_LINES = TRPDES_LEN/28		!28 Bytes / line
C
C Check if super double game is active
C
	GNUM = SCFGTN(TTRP,GIND)
C
	IF (GNUM .LT. 1) THEN
	  WRITE(5,991) IAM(),GTNAMES(TTRP),GIND
	  CALL XWAIT(2,2,ST)
	  RETURN
	END IF
C
C Open required files.
C
	CALL OPENW(1,SCFSFN(1,DAF),4,0,0,ST)
C
	CALL IOINIT(DFDB,1,DAFSEC*256)
C
	IF (ST .NE. 0) THEN
	  CALL FILERR(SCFSFN(1,DAF),1,ST,0)
	  RETURN
	END IF
C
	CALL OPENW(2,SCFGFN(1,GNUM),4,0,0,ST)
C
	CALL IOINIT(FDB,2,DTRSEC*256)
C
	IF (ST .NE. 0) THEN
	  CALL FILERR(SCFGFN(1,GNUM),1,ST,0)
	  CALL CLOSEFIL(DFDB)
	  RETURN
	END IF
C
	CALL OPENW(3,SCFGVN(1,GNUM),4,0,0,ST)
C
	CALL IOINIT(VFDB,3,DTRSEC*256)
C
	IF (ST .NE. 0) THEN
	  CALL FILERR(SCFGVN(1,GNUM),1,ST,0)
	  CALL CLOSEFIL(DFDB)
	  CALL CLOSEFIL(FDB)
	  RETURN
	END IF
C
10	CONTINUE
C
	TSTFLG = .FALSE.
C
	CALL CLRSCR(5)
C
	WRITE(5,900)
C
	CALL WIMG(5,'Enter function: ')
C
	READ(5,901) FUN
C
	IF (FUN .EQ. 'EX') THEN
	  CALL CLOSEFIL(DFDB)
	  CALL CLOSEFIL(FDB)
	  CALL CLOSEFIL(VFDB)
	  RETURN
	END IF
C
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
C
	CALL FASTMOV(FDB,INPFDB,7)
C
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
C
20	CONTINUE
C
	IF (FUN .NE. 'LI') CALL CLRSCR(5)
	CALL INPNUM('Enter event number(E-exit) ',DRW,-9999,9999,EXT)
	IF (EXT .LT. 0 .OR. DRW .EQ. 0) GOTO 10
	IF (DRW .LT. 1) TSTFLG = .TRUE.
	DRW = ABS(DRW)
C
        DO I=1,3
          DO J=1,LDATE_LEN
            EVDATE(J,I)=0
          ENDDO
        ENDDO
C
C Read previous text checksum
C
	PREV3 = 0
	IF(DRW.GT.1) THEN
	  CALL READW(INPFDB,DRW-1,DTRREC,ST)
C
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
	  CALL ILBYTE(PREV3,DTRREV,2)          !GET PREVIOUS TEXT REV #
        ENDIF
C
	CALL CLRSCR(5)
C
	CALL READW(INPFDB,DRW,DTRREC,ST)
C
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
C
	LSTAT = DTRSTS + 1
C
	DO 30 I = 1,7
	  BDATE(I+6) = DAY(I)
	  EDATE(I+6) = DAY(I)
	  DDATE(I+6) = DAY(I)
30	CONTINUE
C
	BDATE(5) = DTRBSD
	IF (BDATE(5) .NE. 0) CALL LCDATE(BDATE)
C
	EDATE(5) = DTRESD
	IF (EDATE(5) .NE. 0) CALL LCDATE(EDATE)
C
	DDATE(5) = DTRDAT
	IF (DDATE(5) .NE. 0) CALL LCDATE(DDATE)
C
	WEK = DTRWEK
	SALPER = DTRSPR
	PRICE = DTRPRC
C
        DO I=1,3
           EVDATE(5,I)=DTREVD(I)
           IF(EVDATE(5,I).NE.0) CALL LCDATE(EVDATE(1,I))
        ENDDO
C
	IF (FUN .EQ. 'MO' .AND. DTRSTS .GE. GAMOPN) THEN
	  CALL CLRSCR(5) 
	  WRITE(5,992) IAM(),GTNAMES(TTRP),GIND,DRW,IAM()
	  CALL WIMG(5,'Hit return to continue')
	  READ(5,901) ANS
	  GOTO 10
	END IF
C
	DO 85 L = 1,3
	  DO 80 J = 1,MAXTRPRW
	    DO 70 K = 1,TRPNMS_LEN/4
	      IF (DTRNMS(K,J,L) .EQ. 0) DTRNMS(K,J,L) = '    '
70	    CONTINUE
80	  CONTINUE
85	CONTINUE
C
	DO 91 J = 1,3
	  DO 90 I = 1,TRPENM_LEN/4
	    IF (DTRENM(I,J) .EQ. 0) DTRENM(I,J) = '    '
90	  CONTINUE
91	CONTINUE
C
	IF (FUN .EQ. 'MO') GOTO 120
	IF (FUN .EQ. 'VE') GOTO 1000
	IF (FUN .EQ. 'UN') GOTO 2000
	IF (FUN .EQ. 'CH') GOTO 3000
C
C Display game data.
C
105	CONTINUE
C
	CALL CLRSCR(5)
C
	WRITE(5,903) GTNAMES(TTRP),GIND,DRW,WEK,GSTAT(LSTAT)
C
	WRITE(5,9030) (DTRMNM(K),K=1,TRPENM_LEN/4)
C
	WRITE(5,9031) 1,(DTRENM(K,1),K=1,TRPENM_LEN/4),DTRRWS(1),
     *                2,(DTRENM(K,2),K=1,TRPENM_LEN/4),DTRRWS(2),
     *                3,(DTRENM(K,3),K=1,TRPENM_LEN/4),DTRRWS(3)
C
	CALL FASTMOV(DTRDES,DNAME,(TRPDES_LEN/4)*3)
C
	DO J=1,3       
          WRITE(5,9142) J
  	  DSCIND = 0
	  DO I = 1,DESCRIPT_LINES
	    WRITE(5,9141)(CDNAME(K,J),K=DSCIND+1,DSCIND+28)
	    DSCIND = DSCIND + 28
          ENDDO
	ENDDO
C
        WRITE(5,904) 
     *	(BDATE(I),I=7,13),BDATE(5),(EDATE(I),I=7,13),EDATE(5)
C
	WRITE(5,910) (DTRPFN(K),K=1,5)
	WRITE(5,945) CMONY(DTRPRC,10,BETUNIT)
C
	WRITE(5,905) (DDATE(K),K=7,13),DDATE(5),DISTIM(DTRTIM)
        DO I=1,3
	   WRITE(5,9050) I,(EVDATE(K,I),K=7,13),DISTIM(DTREVT(I))
        ENDDO
C
	WRITE(5,9051) 
C
	DO 110 I=1,MAXTRPRW
	  WRITE(5,906) I,
     *		       (DTRNMS(J,I,1),J=1,TRPNMS_LEN/4),
     *		       (DTRNMS(J,I,2),J=1,TRPNMS_LEN/4),
     *		       (DTRNMS(J,I,3),J=1,TRPNMS_LEN/4)
110	CONTINUE
C
	IF (TSTFLG) THEN
	  WRITE(5,99999) IAM(),'REVISION:   ',DTRREV
	  WRITE(5,99998) IAM(),'DESCRIPT:   ',(DTRDES(K,1),K=1,TRPDES_LEN/4)
	  WRITE(5,99998) IAM(),'DESCRIPT:   ',(DTRDES(K,2),K=1,TRPDES_LEN/4)
	  WRITE(5,99998) IAM(),'DESCRIPT:   ',(DTRDES(K,3),K=1,TRPDES_LEN/4)
	END IF
C
	GOTO 20
C
C Modify game date
C
120	CONTINUE
C
	WRITE(5,917) IAM()
C
	WRITE(5,918) IAM(),(BDATE(I),I=7,13) 
C
	CALL INPDAT(CDC,EXT)
C
	IF (EXT .NE. 0) CDC = DTRBSD
C
	BDATE(5) = CDC
C
	CALL LCDATE(BDATE)
C
	WRITE(5,919) IAM()
C
	WRITE(5,918) IAM(),(EDATE(I),I=7,13) 
C
	CALL INPDAT(CDC,EXT)
C
	IF (EXT .NE. 0) CDC = DTRESD
C
	EDATE(5)=CDC
C
	CALL LCDATE(EDATE)
C
	CALL FIGWEK(CDC-WEEK_OFFSET,WEK,YEAR)
C
	WRITE(5,923) IAM()
C
	WRITE(5,924) IAM(),(DDATE(I),I=7,13)
C
	CALL INPDAT(CDC,EXT)
C
	IF (EXT .NE. 0) CDC = DTRDAT
C
	DDATE(5) = CDC
C
	CALL LCDATE(DDATE)
C
	WRITE(5,921) IAM()
C
	WRITE(5,922) IAM(),DISTIM(DTRTIM)
C
	TEMP = DTRTIM
C
	CALL INPTIM('Enter time HH:MM:SS....................',DTRTIM,EXT)
C
	IF (EXT .NE. 0) DTRTIM = TEMP 
C
        DO I=1,3
	   WRITE(5,946) IAM(),I
	   WRITE(5,924) IAM(),(EVDATE(K,I),K=7,13)
	   CALL INPDAT(CDC,EXT)
	   IF (EXT .NE. 0) CDC = DTREVD(I)
	   EVDATE(5,I) = CDC
	   CALL LCDATE(EVDATE(1,I))
	   WRITE(5,947) IAM(),I
	   WRITE(5,922) IAM(),DISTIM(DTREVT(I))
	   CALL INPTIM('Enter time HH:MM:SS....................',EVTIME,EXT)
	   IF (EXT .EQ. 0) DTREVT(I) = EVTIME
        ENDDO
C
        WRITE(5,943) IAM()
        WRITE(5,944) IAM(), CMONY(PRICE,10,BETUNIT)
        CALL INPMONY('Enter minimum stake.........',PRICE,BETUNIT,EXT)
        IF(EXT.NE.0) DTRPRC=PRICE
C
	WRITE(5,929) IAM()
C
	WRITE(5,930) IAM(),DTRPFN(1)
C
	WRITE (CDTRPFN,911) GIND,DRW
C
	CALL WIMG(5,'Enter pool file volume name............')
C
	READ(5,907) NAME(1)
C
	IF (NAME(1) .NE. '    ') DTRPFN(1)=NAME(1)
C
C Replace nulls with spaces
C
	DO I = 1,TRPENM_LEN
	   IF(BENAME(I) .EQ. 0) BENAME(I) = ' '
	ENDDO
C
C Master Event name
C
	WRITE(5,9311) IAM()
	WRITE(5,932) IAM(),(DTRMNM(K),K=1,TRPENM_LEN/4)
	CALL WIMG(5,'Enter event name.......................')
	CLEARED = 0
	READ(5,933) ENAME
C
C Did he/she hit return?
C
	DO I = 1,TRPENM_LEN/4
	  IF(ENAME(I).EQ.'    ') CLEARED = CLEARED + 1
	END DO
	IF (CLEARED .EQ. TRPENM_LEN/4) GOTO 130
C
C Does he want us to clear it?
C
	IF (CENAME(1) .EQ. '+') THEN
	   DO I = 1,TRPENM_LEN/4
	     ENAME(I) = '    '
	   END DO
	END IF
C
C Replace nulls with spaces
C
	DO I = 1,TRPENM_LEN
	   IF(BENAME(I) .EQ. 0) BENAME(I) = ' '
	ENDDO
C
C Move the input over.
C
	DO I=1,TRPENM_LEN/4
	   DTRMNM(I) = ENAME(I)
	END DO
C
130	CONTINUE
C
C Event names
C
	DO 135 J=1,3        
	   WRITE(5,931) IAM(),J
	   WRITE(5,932) IAM(),(DTRENM(K,J),K=1,TRPENM_LEN/4)
	   CALL WIMG(5,'Enter contest name.....................')
	   CLEARED = 0
	   READ(5,933) ENAME
C
C Did he/she hit return?
C
	   DO I = 1,TRPENM_LEN/4
	     IF(ENAME(I).EQ.'    ') CLEARED = CLEARED + 1
	   END DO
	   IF (CLEARED .EQ. TRPENM_LEN/4) GOTO 135
C
C Does he want us to clear it?
C
   	   IF (CENAME(1) .EQ. '+') THEN
	      DO I = 1,TRPENM_LEN/4
	        ENAME(I) = '    '
	      END DO
	   END IF
C
C Replace nulls with spaces
C
	   DO I = 1,TRPENM_LEN
	      IF(BENAME(I) .EQ. 0) BENAME(I) = ' '
	   ENDDO
C
C Move the input over.
C
	   DO I=1,TRPENM_LEN/4
	      DTRENM(I,J) = ENAME(I)
	   END DO
C
135	CONTINUE
C
C Descriptions of events 
C
140	CONTINUE
        DO 141 J=1,3
	   WRITE(5,934) IAM(), J 
	   DSCIND = 0
	   CALL FASTMOV(DTRDES(1,J),DNAME(1,J),(TRPDES_LEN/4))
	   DO I = 1,DESCRIPT_LINES
	      WRITE(5,935) IAM(),I,(CDNAME(K,J),K=DSCIND+1,DSCIND+28)
	      DSCIND = DSCIND + 28
	   END DO
	   TYPE*,IAM()
	   DSCIND = 0
	   DO I = 1,DESCRIPT_LINES
	      CALL WIMG(5,'Enter contest description')
	      READ(5,936) (CDNAME(K,J),K=DSCIND+1,DSCIND+28)
	      DSCIND = DSCIND + 28
	   END DO
C
C Did he/she hit return?
C
	   CLEARED = 0
	   DO I = 1,TRPDES_LEN/4
	      IF (DNAME(I,J) .EQ. '    ') CLEARED = CLEARED + 1
	   END DO
	   IF (CLEARED .EQ. TRPDES_LEN/4) THEN
	      CLEARED = 0 
	      DO I = 1,TRPDES_LEN/4
	         IF (DTRDES(I,J) .EQ. 0) CLEARED = CLEARED + 1
	      END DO
	      IF (CLEARED .EQ. TRPDES_LEN/4) THEN
	         DO I = 1,TRPDES_LEN/4
	            DTRDES(I,J) = '    '
	         ENDDO
              ENDIF
              GOTO 141
	   ENDIF
C
C Does he want us to clear it?
C
	   IF (CDNAME(1,J) .EQ. '+') THEN
	      DO I = 1,TRPDES_LEN/4
	        DNAME(I,J) = '    '
	      END DO
	   ENDIF
C
C Move the description over.
C
	   DO I = 1,TRPDES_LEN/4
	      DTRDES(I,J) = DNAME(I,J)
	   END DO
C
141	CONTINUE
        TYPE*,IAM(),'Modification of row names'
C
299	CONTINUE
	CALL INPNUM('Enter contest number (1-3, E-Exit)',
     *	      WHICH_EVENT,1,3,EXT)
	IF (EXT .LT. 0) GOTO 500
C
300	CONTINUE
	TYPE*,IAM()
	CALL INPNUM('Enter row number to be modified (E-Exit)',
     *	     ROW,1,MAXTRPRW,EXT)
	IF (EXT .LT. 0) GOTO 299
C
	WRITE(5,940) IAM()
	WRITE(5,941) IAM(),(DTRNMS(K,ROW,WHICH_EVENT),K=1,TRPNMS_LEN/4)
	CALL WIMG(5,'Enter Row Name........................')
	READ(5,942) (NAME(K),K=1,TRPNMS_LEN/4)
C
C Did he/she hit return?
C
	CLEARED = 0
C
	DO I = 1,TRPNMS_LEN/4
	   IF (NAME(I) .EQ. '    ') CLEARED = CLEARED + 1
	END DO
C
	IF (CLEARED .EQ. TRPNMS_LEN/4) GOTO 210
C
C Does he want us to clear it?
C
	IF (CNAME(1) .EQ. '+') THEN
	   DO I = 1,TRPNMS_LEN/4
	     NAME(I) = '    '
	   END DO
	END IF
C
C Replace nulls with spaces
C
	DO I = 1,TRPNMS_LEN
	  IF(BNAME(I) .EQ. 0) BNAME(I) = ' '
	ENDDO
C
C Move the name over.
C
	DO I = 1,TRPNMS_LEN/4
	    DTRNMS(I,ROW,WHICH_EVENT) = NAME(I)
	END DO
C 
210	CONTINUE
C
	GOTO 299
C
500	CONTINUE
C
        DO J=1,3
           DTRRWS(J) = 0
           DO I=1,MAXTRPRW
              IF(DTRNMS(1,I,J).NE.'    ') 
     *	         DTRRWS(J) = DTRRWS(J) +1
          ENDDO
        ENDDO  
C
	DTRBSD = BDATE(5)
	DTRESD = EDATE(5)
	DTRDAT = DDATE(5)
	DTRWEK = WEK
	DTRSTS = GAMINF
	DTRDRW = DRW
	DTRSPR = SALPER
	DTRPRC = PRICE
        DO I=1,3
           DTREVD(I)=EVDATE(5,I)
        ENDDO
C
        DO 510,J=1,3
	   DTREST(J) = GAMNUL
	DO 510 I = 1,MAXTRPRW
	   DTRSTA(I,J) = 0
	   IF (DTRNMS(1,I,J) .NE. '    ') THEN
		DTRSTA(I,J) = GAMINF
		DTREST(J)   = GAMINF
	   ENDIF
510	CONTINUE
C
C Create table of text message to checksum for rev.
C
	BUFIND = 1
C
C Load up event name and event descriptions
C
	CALL MOVBYT(DTRENM(1,1),1,BYTTAB,BUFIND,3*TRPENM_LEN)
C
	BUFIND = BUFIND + 3*TRPENM_LEN
C
	CALL MOVBYT(DTRDES(1,1),1,BYTTAB,BUFIND,3*TRPDES_LEN)
C
	BUFIND = BUFIND + 3*TRPDES_LEN
C
C Load up player names.
C
        DO J=1,3
	  DO I = 1,MAXTRPRW                     !FOR ALL ROWS
	    IF (DTRNMS(1,I,J) .NE. '    ') THEN
	      CALL MOVBYT(DTRNMS(1,I,J),1,BYTTAB,BUFIND,TRPNMS_LEN)
	      BUFIND = BUFIND + (TRPNMS_LEN)
	    END IF
	  END DO
	ENDDO
C
	BUFIND = BUFIND - 1
C
	CALL CHECKSUM(BYTTAB,1,BUFIND,REV4)
C
	CALL ILBYTE(REV1,DTRREV,0)	    
C
        IF(DTRDRW-1.EQ.M251) THEN
           REV1 = MOD(REV1+DTRDRW,(M251-10)) + 1
        ELSE
           REV1 = MOD(REV1+DTRDRW,M251) + 1
        ENDIF
	REV2 = MOD(DTRDRW,255)
C
	CALL ILBYTE(REV3,DTRREV,2)          !GET PREVIOUS TEXT REV #
C
	REV3 = MOD(PREV3 + REV3,255) + 1
C
	CALL ISBYTE(REV1,DTRREV,0)          !CONTROL REV BYTE (SEQUENCE#)
	CALL ISBYTE(REV2,DTRREV,1)          !DRAW REV BYTE
	CALL ISBYTE(REV3,DTRREV,2)          !TEXT REV # BYTE  (SEQUENCE#)
	CALL ISBYTE(REV4,DTRREV,3)          !TEXT CHECKSUM BYTE
C
	CALL WRITEW(INPFDB,DRW,DTRREC,ST)
C
	IF (ST .NE. 0) THEN
	  CALL CLRSCR(5)
	  IF (INPFIL .EQ. 'V')THEN
	      CALL FILERR(SCFGVN(1,GNUM),3,ST,DRW)
	  ELSE
	      CALL FILERR(SCFGFN(1,GNUM),3,ST,DRW)
	  END IF
	  GOTO 10
	END IF
C
	GOTO 20
C
C Verification
C
1000	CONTINUE
C
	CALL CLRSCR(5)
	CALL PASSWORD(5,PWORD)
C
	IF (PASS .NE. OKPASS) THEN
	  TYPE*,'Invalid password entered'
	  CALL XWAIT(2,2,ST)
	  GOTO 10
	END IF
C
	VERR = 0
C
	IF (DTRSTS .EQ. GAMOPN) THEN
	  TYPE*,IAM(),TRINT_NAME,GIND,' event ',DRW,' already verified'
	  CALL WIMG(5,'Hit return to continue')
	  READ(5,901) ANS
	  GOTO 10
	END IF
C
	IF (DTRSTS .NE. GAMINF) THEN
	  TYPE*,IAM(),TRINT_NAME,GIND,' event ',DRW,' invalid status ',
     *          DTRSTS
	  CALL WIMG(5,'Hit return to continue')
	  READ(5,901) ANS
	  GOTO 10
	END IF
C
	IF (DTRBSD .GT. DTRESD) THEN
	  TYPE*,IAM(),'Begining sales date greater then ending sales date'
	  VERR = VERR + 1
	END IF
C
	IF (DTRBSD .EQ. 0) THEN
	  TYPE*,IAM(),'Begining sales date not set '
	  VERR = VERR + 1
	END IF
C
	IF (DTRDAT .LT. DTRESD) THEN
	  TYPE*,IAM(),'Event date is before last sales date'
	  VERR = VERR + 1
	END IF
C
	IF (DTRESD .EQ. 0) THEN
	  TYPE*,IAM(),'Ending sales date not set'
	  VERR = VERR + 1
	END IF
C
	IF (DTRPRC .EQ. 0) THEN
	  TYPE*,IAM(),'Base price not set'
	  VERR = VERR + 1
	END IF
C
	IF (DTRSPR .EQ. 0) THEN
	  TYPE*,IAM(),'Pool percentage not set'
	  VERR = VERR + 1
	END IF
C
	IF (VERR .NE. 0) THEN
          TYPE*,IAM(),VERR,' data entry errors for ',TRINT_NAME,
     *        GIND,' event ',DRW
	  TYPE*,IAM(),'Event has not been verified'
	  CALL WIMG(5,'Hit return to continue')
	  READ(5,901) ANS
	  GOTO 10
	END IF
C
	DO 1020 I = DTRBSD,DTRESD
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
C
	IF (VERR .NE. 0) THEN
	  TYPE*,IAM(),VERR,' game date errors for ',TRINT_NAME,
     *          GIND,' event ',DRW
	  TYPE*,IAM(),'Event has not been verified'
	  CALL WIMG(5,'Hit return to continue')
	  READ(5,901) ANS
	  GOTO 10
	END IF
C
	DTRSTS = GAMOPN
	DO 1030 J = 1,3
	  DTREST(J) = GAMNUL
	DO 1030 I = 1,MAXTRPRW
	  IF (DTRSTA(I,J) .EQ. GAMINF) THEN
	    DTRSTA(I,J) = GAMOPN
	    DTREST(J)   = GAMOPN
	  ENDIF
1030	CONTINUE
C
	DO 1040 I = DTRBSD,DTRESD
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
C
	 CALL WRITEW(FDB,DRW,DTRREC,ST)
C
	 IF (ST .NE. 0) THEN
	   CALL FILERR(SCFGFN(1,GNUM),3,ST,DRW)
	   GOTO 10
	 END IF
C
	 TYPE*,IAM(),TRINT_NAME,GIND,' event ',DRW,' verify complete'
C
	 CALL WIMG(5,'Hit return to continue')
C
	 READ(5,901) ANS
C
	 GOTO 10
C
2000	CONTINUE
C
	CALL CLRSCR(5)
C
	CALL PASSWORD(5,PWORD)
C
	IF (PASS .NE. OKPASS) THEN
	  TYPE*,IAM(),'Invalid password entered'
	  CALL XWAIT(2,2,ST)
	  GOTO 10
	END IF
C
 	IF (DTRSTS .LE. GAMINF) THEN
	  TYPE*,IAM(),TRINT_NAME,GIND,' event ',DRW,' has not been verified '
	  CALL WIMG(5,'Hit return to continue')
	  READ(5,901) ANS
	  GOTO 10
	END IF
C
	IF (DTRSTS .GT. GAMOPN) THEN
	  TYPE*,IAM(),TRINT_NAME,GIND,' event ',DRW,' has already ended'
	  CALL WIMG(5,'Hit return to continue')
	  READ(5,901) ANS
	  GOTO 10
	END IF
C
	DO 2010 I = DTRBSD,DTRESD
	  CALL READW(DFDB,I,DAFREC,ST)
	  IF (DAFSTS .GT. GAMOPN) THEN
	    TYPE*,IAM(),TRINT_NAME,GIND,' currently active, cannot be changed'
	    CALL WIMG(5,'Hit return to continue')
	    READ(5,901) ANS
	    GOTO 10
	  ENDIF
2010	CONTINUE
C
	DTRSTS = GAMINF
	DO 2020 J = 1,3
	  DTREST(J) = GAMNUL
	DO 2020 I = 1,MAXTRPRW
	  IF (DTRSTA(I,J) .EQ. GAMOPN) THEN
	    DTRSTA(I,J) = GAMINF
	    DTREST(J)   = GAMINF
	  ENDIF
2020	CONTINUE
C
	DO 2030 I = DTRBSD,DTRESD
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
C
	CALL WRITEW(FDB,DRW,DTRREC,ST)
C
	IF (ST .NE. 0) THEN
	  CALL FILERR(SCFGFN(1,GNUM),3,ST,DRW)
	  GOTO 10
	END IF
C
	TYPE*,IAM(),TRINT_NAME,GIND,' event ',DRW,
     *	     'is un-verified and can be modified'
C
	CALL WIMG(5,'Hit return to continue')
C
	READ(5,901) ANS
C
	GOTO 10
C
C Check and validate data file.
C
3000	CONTINUE
C
	IF ((FUN .EQ. 'CH') .AND. (DTRSTS .GE. GAMOPN)) THEN
	  CALL CLRSCR(5)
          TYPE*,IAM(),TRINT_NAME,GIND,' EVENT ',DRW,
     *        ' DATA HAS BEEN VERIFIED.'
	  TYPE*,IAM(),' CHANGES CANNOT BE MADE UNLESS EVENT IS UNVERIFIED.'
	  CALL WIMG(5,'HIT RETURN TO CONTINUE.')
	  READ(5,901) ANS
	  GOTO 10
	END IF
C
	CALL TRPCOMP(FDB,VFDB,GIND,DRW,GNUM,ST)
C
	IF (ST .NE. 0) TYPE*,IAM(),' ERROR IN WICOMP, ST IS: ',ST
C
	GOTO 20
C
C------------------  Format statements. ----------------------------------
C
900	FORMAT(' LI - List event data',/,
     *	       ' MO - Modify event data',/,
     *	       ' VE - Verify event data',/,
     *	       ' UN - Unverify event data',/,
     *	       ' CH - Check event data',/,
     *	       ' EX - Return to main menu',/)

901	FORMAT(A2)

902	FORMAT(3A4)

903	FORMAT(1X,A8,I1,1X,'Draw ',I4.4,1X,'Week ',I2.2,
     *	       1X,'*',A14,'*',' Name ',<TRPENM_LEN/4>A4)

9030	FORMAT(1X,'Master Event Name',8X,<TRPENM_LEN/4>A4)

9031	FORMAT(3(1X,'Contest ',I1,' Name',12X,<TRPENM_LEN/4>A4,1X,I2,
     *           ' competitors'/))

904	FORMAT(' Sales dates  ',7A2,'  Cdc - ',I4,
     *	               '  < to >  ',7A2,'  Cdc - ',I4)

905	FORMAT(1X,' Master Event date ',7A2,'  Cdc - ',I4,2X,'Time ',A8)
9050	FORMAT(10X,'Contest ',I1,' on ',7A2,2X,A8)

9051	FORMAT(/,' Row',T8,'Contest1',T28,'Contest2',T48,'Contest3')         

906	FORMAT(1X,I2.2,3(4X,<TRPNMS_LEN/4>A4))

907	FORMAT(A4)

908	FORMAT(1X,7A2,' has an invalid day status ')

909	FORMAT(1X,7A2,' is already active for Today''s Trio',
     *	       I1,' event ',I4)

910	FORMAT(1X,'Pool file name ',5A4)

911	FORMAT(4X,':TR',I1,'P',I4.4,'.FIL   ')

912	FORMAT(A)

913	FORMAT(5A4)

914	FORMAT(1X,'( ',28A1' )')

9141	FORMAT(12X,'( ',28A1' )')
9142	FORMAT(12X,'Contest ',I1,' description')

915     FORMAT(1X,'Base price ',A10)

916     FORMAT(1X,'Pool percentage ',F8.3)

917	FORMAT(1X,A,'Begining sales date')

918	FORMAT(1X,A,'Current is, (E-no change)...............',7A2)

919	FORMAT(1X,A,'Ending sales date')

921	FORMAT(1X,A,'Master Close time')

922	FORMAT(1X,A,'Current is, (E-no change)...............',A8)

923	FORMAT(1X,A,'Master Event date')

924	FORMAT(1X,A,'Current is, (E-no change)...............',7A2)

925	FORMAT(1X,A,'Base price')

926	FORMAT(1X,A,'Current: ',A10,' (E-no change)')

927	FORMAT(1X,A,'Pool percentage')

928	FORMAT(1X,A,'Current: ',F8.3,' (E-no change)')

929	FORMAT(1X,A,'Pool file volume name')

930	FORMAT(1X,A,'Current is, (RETURN for System Volume)..',3A4)

931	FORMAT(1X,A,'Contest ',I1,' name')

9311	FORMAT(1X,A,'Master Event name')

932	FORMAT(1X,A,'(RETURN no change /+ clear)',<TRPENM_LEN/4>A4)

933	FORMAT(<TRPENM_LEN>A4)

934	FORMAT(/,1X,A,'Contest ',i1,' Description lines ',
     *         '(RETURN no change /+ clear)')

935	FORMAT(1X,A,'Description Line<',I1,'>',28A1)

936	FORMAT(28A1)

937	FORMAT(1X,A,'TV-Chanel Name')

938	FORMAT(1X,A,'Current is, (RETURN no change /+ clear)',
     *         <TRPTVC_LEN/4>A4)

939	FORMAT(<TRPTVC_LEN/4>A4)

940	FORMAT(1X,A,'Competitor name')

941	FORMAT(1X,A,'Current is, (RETURN no change /+ clear)',
     *         <TRPNMS_LEN/4>A4)

942	FORMAT(<TRPNMS_LEN/4>A4)

943     FORMAT(1X,A,'Event ')
944     FORMAT(1X,A,'Current stake .....................',A10)
945     FORMAT(1X,'Minimum stake   ',A10)
946	FORMAT(1X,A,'Contest ',I1,' date')
947	FORMAT(1X,A,'Contest ',I1,' time')

990	FORMAT(A8)

991	FORMAT(1X,A,'Sorry, ',A8,1X,I1,' game not active')

992	FORMAT(1X,A,A8,1X,I1,' draw ',I4,' has been verified',/,
     *         1X,A,A8,'Changes can not be made unless event ',
     *                 'is UNverified')

99998   FORMAT(1X,A,<TRPDES_LEN/4>A4)

99999	FORMAT(1X,A,A12,Z8)	


	END
