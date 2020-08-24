C BNGRES.FOR
C
C V07 05-JAN-1999 OXK WINYES set as status (=removed WINPRV)
C V06 22-DEC-1999 OXK Multiwin changes.
C V05 20-DEC-1999 PXO Added a call to a report subroutine
C V04 11-MAR-1999 UXN Fix for displaying date.
C V03 01-SEP-1997 UXN BINGOFTP.DEF removed, redesign of the program.
C		      Changes for new BINGO, old BINGO AB removed.
C V02 12-JAN-1995 PXB Add screen displays and fix checksum errors.
C V01 16-DEC-1994 HXK Changed around for prognosis, display results.
C
C
C Program to process BINGO results that have been transfered from a PC.
C
C 1. BINGO FullHouse results and update game status
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

C=======OPTIONS /CHECK=NOOVERFLOW/EXT

	PROGRAM BINGRES
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:BNGCOM.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:STOPCOM.DEF'
C
	INTEGER*4	FDB(7),GNUM,GIND,FLAG,EXT
	INTEGER*4	I,J,XLEN,ST,LUN,FULL_COUNT
	INTEGER*4	DRAW,OPTION,PC_CHECK,TEMPSTS
	INTEGER*4	I_CHECKSUM,RECTYPE
	CHARACTER*20	CFILNAM 
	CHARACTER*8	TEMP_ROUND,TEMP_DRAW
	CHARACTER*12	TEMP_DATE,TEMP_TIME,TEMP_OK
	CHARACTER*80	OUTTXT,INBUF

	BYTE		B_CHECKSUM(80)
	EQUIVALENCE (B_CHECKSUM,INBUF)

        CHARACTER*(*)   I8FMT
        PARAMETER       (I8FMT='(I8)')
        CHARACTER*(*)   I4FMT
        PARAMETER       (I4FMT='(I4)')

        LOGICAL*4   OK
C
	CALL COPYRITE
C
C Initialize variables.
C
	LUN = 7			
	ST = 0
	FULL_COUNT = 1
	PC_CHECK=0
	GIND=1

        WRITE(6,900)
        CALL INPNUM('Enter option ',OPTION,1,1,EXT)
        IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
C	
C Open SCF file and read record.
C
        CALL GETSCONF(SCFREC,ST)
        IF (ST.NE.0) CALL GSTOP(GEXIT_FATAL)
C
C Open Bingo record.
C
        GNUM = GTNTAB(TBNG,1)
	PROGNOSIS(GNUM) = 0

        CALL OPENW (1,SCFGFN(1,GNUM),4,0,0,ST)
        IF (ST .NE. 0) CALL FILERR (SCFGFN(1,GNUM),1,ST,0)

        CALL IOINIT (FDB,1,DBNSEC*256)


	CFILNAM = 'FILE:BFHRESU.FIL'

	OPEN (UNIT = LUN,
     *	      FILE = CFILNAM,
     *	      IOSTAT = ST,
     *	      STATUS = 'OLD',
     *	      ORGANIZATION = 'SEQUENTIAL',
     *	      ACCESS = 'SEQUENTIAL')

        IF (ST .NE. 0) THEN
	  TYPE*,IAM(),'Error opening ',CFILNAM,' status=',ST
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
C Read records from input file.
C
200	CONTINUE

	READ (LUN,IOSTAT = ST,FMT = '(76A)') INBUF
	
        IF (ST .NE. 0) THEN
	  TYPE*,IAM(),'Error reading ',CFILNAM,' status=',ST
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
C
	RECTYPE = CTOI(INBUF(1:4),XLEN)		!Record type.
C
C Header record ( RECTYPE = 10 )
C
	IF (RECTYPE.EQ.10) THEN

          DRAW = CTOI(INBUF(25:32),XLEN)
	  TEMP_ROUND = '   ' // INBUF(23:24) // '/' // INBUF(15:16)
	  TEMP_DRAW = INBUF(25:32)
	  TEMP_DATE = INBUF(33:44)
	  TEMP_TIME = INBUF(45:56)

          CALL READW (FDB,DRAW,DBNREC,ST)
          IF (ST. NE. 0) THEN
	    CALL FILERR (SCFGFN(1,GNUM),2,ST,DRAW)
            CALL GPAUSE
	  ENDIF

	  TEMPSTS=DBNSTS
	  IF(DAYSTS.NE.DSOPEN.OR.DRAW.NE.DAYDRW(GNUM)) TEMPSTS=BNGSTS(GIND)
	
	  CALL BINGO_CHECKSUM(B_CHECKSUM,I_CHECKSUM,56)

          IF(TEMPSTS.NE.GAMBFD) THEN
              WRITE(6,904) IAM(),GTNAMES(TBNG),GIND,DRAW,DBNSTS
              CALL GPAUSE
          ENDIF
 
	  GOTO 200
	ENDIF
C
C End record (RECTYPE = 99)
C
	IF (RECTYPE.EQ.99) THEN

	  READ (INBUF(5:12),I8FMT) PC_CHECK

	  IF (PC_CHECK .NE. I_CHECKSUM) THEN
	    TEMP_OK = '*** NOT OK'
	  ELSE
	    TEMP_OK = 'OK'
	  END IF
	  GOTO 300
	ENDIF
C
C Winning numbers for fullhouse game.
C
	IF (RECTYPE.GE.20.AND.RECTYPE.LE.27) THEN

	  CALL BINGO_CHECKSUM(B_CHECKSUM,I_CHECKSUM,44)

	  DO I = 0,9
	    READ (INBUF(5+I*4:8+I*4),I4FMT) DBNWIN(FULL_COUNT)
	    IF(FULL_COUNT.EQ.BGONBR) GOTO 200
	    FULL_COUNT = FULL_COUNT + 1
	  END DO
	  GOTO 200	  
	ENDIF


300	CONTINUE

	CALL CLOSEFIL(LUN)
C
C Check duplicates.
C
	CALL BINGO_CHKDUP(ST)

	IF(ST.NE.0) THEN
      	     TYPE*,IAM(),' Duplicate numbers in file, please check ',CFILNAM
	     CALL GSTOP(GEXIT_FATAL)
	ENDIF

 	TYPE*,IAM(),' '
	TYPE*,IAM(),'------------------------------------------------------'
 	TYPE*,IAM(),' '
	TYPE*,IAM(),'    Read file:       ',CFILNAM(6:20)
	TYPE*,IAM(),' '
	TYPE*,IAM(),'    Round:           ',TEMP_ROUND
	TYPE*,IAM(),'    Draw:            ',TEMP_DRAW
	TYPE*,IAM(),'    Time:            ',TEMP_DATE(3:12),' ',
     *				            TEMP_TIME(5:12)
	TYPE*,IAM(),' '
	TYPE*,IAM(),'    Winning numbers  Bingo Fullhouse'
	TYPE*,IAM(),' '
	DO I = 0,7
	  WRITE(OUTTXT,'(21X)')
	  DO J=1,10
	    IF(J+I*10.LE.BGONBR) THEN
	      IF(DBNWIN(J+I*10).EQ.0) THEN
	        IF(J.GT.1) TYPE*,OUTTXT
	        GOTO 500
              ENDIF
              WRITE(OUTTXT(22+(J-1)*3:),908) DBNWIN(J+I*10)
            ENDIF
	  ENDDO
	  TYPE*,OUTTXT  
	ENDDO
C
500	CONTINUE
	TYPE*,IAM(),' '
	TYPE*,IAM(),'    Checksum:        ',TEMP_OK
	TYPE*,IAM(),'------------------------------------------------------'

	CALL WIMG(6,'Is this correct [Y/N] > ')
      	CALL YESNO(FLAG)
C
C Write away record.
C
	IF (FLAG .EQ. 1) THEN
	   DBNSTS=GAMENV
      	   CALL WRITEW (FDB,DRAW,DBNREC,ST)
      	   IF (ST .NE. 0) THEN
	      CALL FILERR(SCFGFN(1,GNUM),3,ST,DRAW)
	   ELSE
	      TYPE*,IAM(),' *** Bingo Winning Numbers updated'
      	      DO I=1,MAX_WINSEL
      	         IF ((DRWGAM(I,GNUM).EQ.DRAW).OR.(DRWGAM(I,GNUM).EQ.0)) THEN
      		    DRWGAM(I,GNUM)=DRAW
    		    DRWSTS(I,GNUM)=WINYES
      		    GOTO 820
      		 ENDIF
      	      ENDDO
820    	      CONTINUE
      	      OK = .TRUE.
      	      IF (I.GT.MAX_WINSEL) OK=.FALSE.
      	      IF (I.EQ.MAX_WINSEL) THEN
      		 IF (DRWGAM(I,GNUM).NE.DRAW) OK=.FALSE.
      	      ENDIF
      	      IF (.NOT.OK) THEN
      		 TYPE*,IAM(),'Too many winner selections for the same game.'
      		 TYPE*,IAM(),'This may result some probelms in MULTIWIN.'
      	      ENDIF
	      CALL BNRESULT(GIND,DRAW)
	   ENDIF
	 ELSE
	   TYPE*,IAM(),' *** WINNING NUMBERS ***NOT*** UPDATED ***'
	 ENDIF

C------------------------ Format Statements --------------------------------

900     FORMAT (///,T5,' Bingo FTP Results Entry',
     *  ///,T5,'1',5X,'- Bingolotto results to online.',
     *   //,T5,'E',5X,'- Exit',//)
901	FORMAT (A1)
904     FORMAT (1X,A,1X,A8,I1,' draw ',I4,' invalid game status> ',I4)
905     FORMAT (1X,A,1X,A8,I1,' draw ',I4)
9081	FORMAT (4X,'Draw results:    ')
908	FORMAT (I2.2,1X)
9091	FORMAT (4X,'Draw results:    ',5(I3,1X))
909	FORMAT (21X,5(I3,1X))
	END

 
