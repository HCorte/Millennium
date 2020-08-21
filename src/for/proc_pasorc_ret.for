C V02 19-APR-2010 FRP  ePassive
C V01 01-MAR-2001 ANG  INITIAL RELEASE FOR PORTUGAL
C
C PROGRAM TO GENERATE FILE TO SCML ORACLE SYSTEM.
C
C ORACLE FILE: PASORC_RET.ASC - CONTAINS PASSIVE RETURNED TICKETS.
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
C Copyright 1998 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        OPTIONS /CHECK=NOOVERFLOW
        PROGRAM  PSORCRET
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:DESLOG.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'
        INCLUDE 'INCLIB:STANDARD.DEF'

C------- DRAW FILE STUFF ----------
	INTEGER*4    DVOL            !DRAW VOLUME
        INTEGER*4    IDRWFIL(5)      !INT*4 FILE NAME FOR OPEN
	INTEGER*4    FDB(7)          !FILE DESCRIPTION BUFFER FOR DRAW FILE
	INTEGER*4    DLUN            !LOGICAL UNIT TO OPEN DRAW FILE
        INTEGER*4    LOGREC(LREC*3)  !BUFFER TO READ FILE        

	CHARACTER*4  CXDVOL          !CHARACTER DRAW VOLUME NAME
	CHARACTER*20 DRWFIL          !CHARACTER DRAW FILE NAME       

C------- DRAW FILE READ CONTROL ---------
	LOGICAL EOD                  !TRUE, IF EOF
	INTEGER*4 BLOCK              !BLOCK # INTO SAVDRW FILE
	INTEGER*4 IND                !WORD INDEX INTO TMFBUF
	INTEGER*4 EOTCNT             !COUNT OF CONSECUTIVE EMPTY RECORDS

C------- PROGRAM CONTROLS ------------
	INTEGER*4    INDPAS          !PASSIVE INDEX
	INTEGER*4    GNUM	     !PASSIVE GAME NUMBER
	INTEGER*4    OLUN            !LOGICAL UNIT FOR PASORC_RET
	INTEGER*4    ST              !ERROR STATUS

C------- EQUIVALENCES --------
        EQUIVALENCE (DVOL,CXDVOL)   
	EQUIVALENCE (DRWFIL,IDRWFIL)

C
C CODE STARTS HERE
C

C
C OPEN PASORC.ASC
C
	CALL OPEN_PASORC(OLUN)            
C
C WRITE HEADER
C
	CALL WRITE_PASORC('HP',TRABUF,OLUN)
C
C GET DRAW FILE VOLUME
C
	DVOL=P(REG_DRWPCK)            
C
C LOOP THROUGH ALL PASSIVE GAME
C
	DO INDPAS=1,NUMPAS
	    GNUM = GTNTAB(TPAS,INDPAS)
	    IF (GNUM.NE.0.AND.DAYDRW(GNUM).GT.0) THEN
		WRITE (DRWFIL,900) DVOL,GSNAMES(GNUM),DAYCDC            !BUILD DRAW FILE NAME
		WRITE(6,100) IAM(),DRWFIL
		CALL FIND_AVAILABLE_LUN(DLUN,ST)                        
		IF (ST.NE.0) THEN
		    TYPE*,IAM(),'ERROR GETTING LUN FOR FILE: ',DRWFIL
		    CALL GPAUSE()
		ELSE
		    CALL OPENW(DLUN,IDRWFIL,5,0,0,ST)                   !OPEN DRAW FILE 
      		    CALL IOINIT(FDB,DLUN,128*256)
		    IF (ST.EQ.0) THEN
			EOD    = .FALSE.
			IND    = 8192
			BLOCK  = 0
			EOTCNT = 0
			CALL MY_READDRW(LOGREC,FDB,EOD,IND,BLOCK,EOTCNT)
			DO WHILE(.NOT.EOD)
      			   CALL LOGTRA(TRABUF,LOGREC)                   !TRANSLATE FROM INTERNAL RECORD TO TRABUF
                           IF(TRABUF(TTYP).EQ.TRET .AND. TRABUF(TWEPOP).EQ.PPASRET)
     *                       CALL WRITE_PASORC('02',TRABUF,OLUN)
			   CALL MY_READDRW(LOGREC,FDB,EOD,IND,BLOCK,EOTCNT)
			ENDDO
			CALL CLOSEFIL(FDB)
		    ELSE
			CALL FILERR(IDRWFIL,1,ST,0)
			CALL CLOSEFIL(FDB)
		    ENDIF
	        ENDIF
	    ENDIF
	ENDDO

	CALL WRITE_PASORC('TP',TRABUF,OLUN)

	CLOSE (OLUN)
	CALL GSTOP(GEXIT_SUCCESS)

100	FORMAT(1X,A18,'Scaning ',A20)
900     FORMAT(A4,':',A4,I4.4,'.FIL')
	END

C*********************************************************************
C
C SUBROUTINE TO OPEN PASORC_RET.ASC
C
C*********************************************************************

        OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE OPEN_PASORC(OLUN)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INTEGER*4 OLUN                 !PASORC_RET LOGICAL UNIT
	INTEGER*4 ST                   !STATUS

	CHARACTER*20 FILNAM            !PASORC FILE NAME

	FILNAM = 'FILE:PASORC_RET.ASC'

	CALL FIND_AVAILABLE_LUN(OLUN,ST)                        
	IF (ST.NE.0) THEN
	    TYPE*,IAM(),'ERROR GETTING LUN FOR FILE: ',FILNAM
	    CALL GPAUSE()
	ENDIF

	OPEN ( UNIT            = OLUN,
     *         FILE            = FILNAM,
     *         STATUS          = 'NEW',
     *         CARRIAGECONTROL = 'LIST',
CC     *         FORM            = 'FORMATTED',
CC     *         ORGANIZATION    = 'SEQUENTIAL',
CC     *         ACCESS          = 'SEQUENTIAL',
CC     *         RECORDTYPE      = 'FIXED',
CC     *         RECL            = 32,
     *         IOSTAT          = ST
     *       )
C
	IF (ST.NE.0) THEN
	    TYPE*,IAM(),'ERROR OPPENING FILE: ',FILNAM
	    CALL GPAUSE()
	ENDIF

	RETURN
	END


C*********************************************************************
C
C SUBROUTINE TO WRITE ON PASORC_RET
C
C*********************************************************************
C
C PROJECT LOTARIA CLASSICA EM SERIES (PLCS)
C ADD NEW FIELD ON FILE

        OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE WRITE_PASORC(TPREC,TRABUF,OLUN)
        IMPLICIT NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:STANDARD.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'

	INTEGER*2   DATE(DATLEN)
	INTEGER*4   CNTREC, GIND, INDEMI, TEMP, EMIS, TICK
        INTEGER*4   CDAGT, SAPNUM, TER, GNUM, TCKS, HALF
	INTEGER*4   WEEK, YEAR, ST, PTYP, NUMFRACS, OLUN
        INTEGER*4   EXTSER, CHKDIG

	LOGICAL     SECTIC
	CHARACTER*2 TPREC
        CHARACTER*6 HHMMSS,GET_HHMMSS_TIME

	IF (TPREC.EQ.'HP') THEN

	    DATE(VCDC) = DAYCDC
	    CALL CDATE(DATE)

	    CNTREC = CNTREC + 1
	    WRITE(OLUN,100) TPREC,2000+DATE(VYEAR),DATE(VMON),DATE(VDAY),' '

	ELSEIF (TPREC.EQ.'02') THEN
	    IF (TRABUF(TPOFFTER).NE.0) THEN
	       TER    = TRABUF(TPOFFTER)
	    ELSE
	       TER    = TRABUF(TTER)
	    ENDIF

	    CDAGT  = AGTTAB(AGTNUM,TER)
	    SAPNUM = AGTSAP(TER)
	    GNUM   = TRABUF(TGAM) 	       
	    GIND   = TRABUF(TGAMIND) 
            DATE(VCDC) = TRABUF(TCDC)
            CALL CDATE(DATE)
            HHMMSS = GET_HHMMSS_TIME(TRABUF(TTIM))
 
	    DO TCKS=1,TRABUF(TPTCK)
	       IF (TRABUF(TPSTS1 + OFFTRA*(TCKS-1)).EQ.RETURND .OR.
     *             TRABUF(TPSTS1 + OFFTRA*(TCKS-1)).EQ.RETAFDR      ) THEN
	          EMIS = TRABUF(TPEMIS1 + OFFTRA*(TCKS-1))     !GET INTERNAL EMISSION NUMBER

	          CALL GETWEK(EMIS,GNUM,WEEK,YEAR,ST)	    !GET SCML EMISSION NUMBER
	          IF (ST.NE.0) THEN
		     TYPE*,IAM(),'ERROR GETTING SCML EMISSION NUMBER'
		     CALL GPAUSE()
	          ENDIF

	          INDEMI = -1
                  DO TEMP=1,PAGEMI
                     IF (PASEMIS(TEMP,GIND).EQ.EMIS) THEN
		        INDEMI = TEMP
			EXIT
		     ENDIF
                  ENDDO

	          IF (INDEMI.LT.0) THEN
		     TYPE*,IAM(),'ERROR GETTING MEMORY OFFSET'
		     CALL GPAUSE()
	          ENDIF

	          SECTIC = .FALSE.
	          IF  (TRABUF(TPRETYP).EQ.ALLTCK) THEN
                      NUMFRACS = PASNOFFRA(INDEMI,GIND)
		      IF (GIND.EQ.PSBPOP) THEN
			 SECTIC = .TRUE.
			 HALF = PASNUMTCK(INDEMI,GIND)/2
                         IF (TRABUF(TPNUM1+OFFTRA*(TCKS-1)).GE.HALF) THEN
                            TICK = TRABUF(TPNUM1+OFFTRA*(TCKS-1)) - HALF
                         ELSE
                            TICK = TRABUF(TPNUM1+OFFTRA*(TCKS-1)) + HALF
                         ENDIF
		      ENDIF
                  ELSEIF(TRABUF(TPRETYP).EQ.BYFRAC) THEN
                      NUMFRACS = 1
                  ELSEIF(TRABUF(TPRETYP).EQ.HALFTCK) THEN
                      IF (GIND.EQ.PSBPOP) THEN
                         NUMFRACS = PASNOFFRA(INDEMI,GIND)
                      ELSE
                         NUMFRACS = PASNOFFRA(INDEMI,GIND)/2
                      ENDIF
                  ELSEIF(TRABUF(TPRETYP).EQ.QUARTCK) THEN
                      NUMFRACS = PASNOFFRA(INDEMI,GIND)/4
                  ENDIF

	          PTYP = -1
	          IF (TRABUF(TGAMIND).EQ.PSBCLA) THEN
		     IF (PASEMIS(INDEMI,GIND).EQ.EMIS) THEN
		     	IF (WEEK.EQ.50 .AND. YEAR.LE.2008) THEN ! (PLCS)
C		        IF (PASNUMSER(INDEMI,GIND).GT.1) THEN !(PLCS)
			   PTYP = 2     !NATAL !(PLCS)
		        ELSE !(PLCS)
			   PTYP = 0     
		        ENDIF !(PLCS)
		     ENDIF
	          ELSE
		     PTYP = 5
	          ENDIF

	          IF (PTYP.LT.0) THEN
                     TYPE*,IAM(),'ERROR GETTING PASSIVE TYPE (WRITE_PASORC)'
	             CALL GPAUSE()
	          ENDIF

                  CALL OUTGEN(TRABUF(TCDC),TRABUF(TPSER1 + OFFTRA*(TCKS-1)),EXTSER,CHKDIG)

	          CNTREC = CNTREC + 1
	          WRITE(OLUN,110) TPREC,                                              !RECORD TYPE
     *                            PTYP,					              !PASSIVE TYPE
     *                            WEEK,					              !WEEK
     *                            YEAR,					              !YEAR
     *                            TRABUF(TPNUM1 + OFFTRA*(TCKS-1)),                   !TICKET NUMBER
     *                            TRABUF(TPSER1 + OFFTRA*(TCKS-1)),                   !SERIE
     *			          TRABUF(TPTEN1 + OFFTRA*(TCKS-1)),                   !FRACTION NUMBER
     *                            SAPNUM,                                             !SAP CODE
     *                            CDAGT,                                              !AGENT NUMBER
     *                            DATE(VJUL),                                         !JULIAN
     *                            EXTSER,                                             !EXTERNAL SERIAL
     *                            CHKDIG,                                             !CHECK DIGITS
     *                            2000+DATE(VYEAR),DATE(VMON),DATE(VDAY),             !DATE
     *                            HHMMSS,                                             !TIME
     *                            TRABUF(TPEMIS1 + OFFTRA*(TCKS-1)),                  !HOST DRAW NUMBER
     *                            ' '                                                 !FILLER

	          IF (SECTIC) THEN							   !PRINT SECOND TICKET FOR POPULAR

	             CNTREC = CNTREC + 1
	             WRITE(OLUN,110) TPREC,                                              !RECORD TYPE
     *                               PTYP,					         !PASSIVE TYPE
     *                               WEEK,					         !WEEK
     *                               YEAR,					         !YEAR
     *                               TICK,                                               !TICKET NUMBER
     *                               TRABUF(TPSER1 + OFFTRA*(TCKS-1)),                   !SERIE
     *			             TRABUF(TPTEN1 + OFFTRA*(TCKS-1)),                   !FRACTION NUMBER
     *                               SAPNUM,                                             !SAP CODE
     *                               CDAGT,                                              !AGENT NUMBER
     *                               DATE(VJUL),                                         !JULIAN
     *                               EXTSER,                                             !EXTERNAL SERIAL
     *                               CHKDIG,                                             !CHECK DIGITS
     *                               2000+DATE(VYEAR),DATE(VMON),DATE(VDAY),             !DATE
     *                               HHMMSS,                                             !TIME
     *                               TRABUF(TPEMIS1 + OFFTRA*(TCKS-1)),                  !HOST DRAW NUMBER
     *                               ' '                                                 !FILLER
	          ENDIF
	       ENDIF
	    ENDDO
	ELSEIF (TPREC.EQ.'TP') THEN
	    CNTREC = CNTREC + 1
	    WRITE(OLUN,120) TPREC,CNTREC,' '
	ENDIF

	RETURN

C100	FORMAT(A2,I4.4,I2.2,I2.2,22(A1))
100	FORMAT(A2,I4.4,I2.2,I2.2,55(A1)) !(PLCS)
C110	FORMAT(A2,I1,I2,I4.4,I1,I5.5,I2.2,I2.2,I6.6,I7.7)
110	FORMAT(A2,I1,I2,I4.4,I5.5,I2.2,I2.2,I6.6,I7.7,I3.3,I8.8,I3.3,I4.4,I2.2,I2.2,A6,I4.4,2(A1))
C120	FORMAT(A2,I8.8,22(A1))
120	FORMAT(A2,I8.8,55(A1)) !(PLCS)
	END


        OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE MY_READDRW(LOGREC,FDB,EOT,IND,BLOCK,EOTCNT)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:DESLOG.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
C
        INTEGER*4 LOGREC(*)         !LOG FORMAT OF ONE SINGLE SERIAL #
        INTEGER*4 FDB(7)            !FILE DESC BLOCK OF SAVDRW FILE
        INTEGER*4 ST                !STATUS
        INTEGER*4 EOTCNT            !COUNT OF CONSECUTIVE EMPTY RECORDS
        INTEGER*4 BLOCK             !BLOCK # INTO SAVDRW FILE
        INTEGER*4 LENGTH            !LENGTH OF SINGLE SERIAL #
        INTEGER*4 TMFBUF(8192)      !BUFFER FOR 1 DRW RECORD
        INTEGER*4 IND               !WORD INDEX INTO TMFBUF
        INTEGER*4 RTYPE             !RTYPE OF SINGLE SERIAL #
        LOGICAL   EOT               !SET TO TRUE WHEN 5000 CONS EMPTY RECS
C
C
1000    CONTINUE
        IF(IND.GE.8157) THEN
          BLOCK=BLOCK+1
          IND=1
          CALL READW(FDB,BLOCK,TMFBUF,ST)
          IF(ST.EQ.144) THEN
             EOT = .TRUE.
             GOTO 10000
          ENDIF
          
	  IF(ST.NE.0) THEN
            TYPE *,'FILE READ ERROR =',ST,'  BLOCK=',BLOCK
            CALL GPAUSE
            EOT=.TRUE.
            GOTO 10000
          ENDIF

        ENDIF

        IF(EOTCNT.GT.1000) THEN
          EOT=.TRUE.
          GOTO 10000
        ENDIF
C
        IF(TMFBUF(IND).EQ.0) THEN
          EOTCNT=EOTCNT+1
          IND=IND+LREC
          GOTO 1000
        ENDIF
C
C
        EOTCNT=0
        CALL ILBYTE(RTYPE,TMFBUF(IND),LREC1-1)
        IF(RTYPE.NE.LONE.AND.RTYPE.NE.LREG) THEN
          TYPE*,'Bad record type > ',RTYPE,' index > ',IND
          IND=IND+LREC
          GOTO 1000
        ENDIF
C
C
        LENGTH=LREC
        IF(RTYPE.EQ.LONE) THEN
          CALL ILBYTE(RTYPE,TMFBUF(IND),LREC2-1)
          IF(RTYPE.EQ.LEND) LENGTH=LREC*2
          IF(RTYPE.EQ.LTWO) LENGTH=LREC*3
        ENDIF
C
        CALL FASTMOV(TMFBUF(IND),LOGREC,LENGTH)
        IND=IND+LENGTH
C
C
10000   CONTINUE
C
        RETURN
        END

C
C GET TIME IN HHMMSS FORMAT STARTING FROM TIME IN SECONDS
C
      OPTIONS /CHECK = NOOVERFLOW /EXT
      CHARACTER * 6 FUNCTION GET_HHMMSS_TIME(TIME)
      IMPLICIT NONE
C
      INTEGER * 4 TIME             ! IMPUT TIME IN SECONDS
      CHARACTER * 6 OUTTIME        ! OUTPUT TIME IN HHMMSS FORMAT
C
      INTEGER * 4 SECONDS           ! SECONDS TIME
      INTEGER * 4 MINUTES           ! MINUTES TIME
      INTEGER * 4 HOURS             ! HOURS TIME
C
      HOURS = TIME / 3600
      MINUTES = MOD(TIME, 3600) / 60
      SECONDS = MOD(MOD(TIME, 3600), 60)
C
      WRITE(OUTTIME, 100) HOURS, MINUTES, SECONDS
C
      GET_HHMMSS_TIME = OUTTIME
C
100   FORMAT(I2.2, I2.2, I2.2)
C
      END
