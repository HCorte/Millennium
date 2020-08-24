C GUICMD_019.FOR
C
C V01 07-FEB-2001 HXK INITIAL RELEASE
C
C JOKER WINNING NUMBERS ENTRY COMMAND
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
C Copyright 2001 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C This subroutine returns GUI FUNCTION.
C
C Input parameters:
C	NONE               
C
C Output parameters:
C
C	BYTE		OUTBUF(*)    OUTPUT MESSAGE
C	INTEGER*4	MES_LEN	     MESSAGE LENGTH
C	INTEGER*4	RET_CODE:
C		0		-  no error, message accepted;
C		value >= 11	-  error number to be sent to Client.
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GUICMD_019(OUTBUF,MES_LEN,RET_CODE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GUIMPRM.DEF'
	INCLUDE 'INCLIB:GUIARGS.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:KIKCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'	
        INCLUDE 'INCLIB:GUIFIL.DEF'	        	
C
C
	BYTE		OUTBUF(*)
	INTEGER*4	MES_LEN,RET_CODE
C
	INTEGER*4 ST
	INTEGER*4 NUM_COLS, NUM_ROWS
	INTEGER*4 PAR,GIND,DRW
	INTEGER*4 WINNING_NUM
	INTEGER*4 BUF(CDLEN)
	INTEGER*4 STATUS
	CHARACTER*40 STATUS_STR
	INTEGER*4 GNUM
C
C
	RET_CODE = 0
	STATUS   = 0
	STATUS_STR = ' '
C
C
	CALL GUI_GETPARAMS(OUTBUF,ST)
	IF(ST.NE.0) THEN
	   RET_CODE = 11
	   RETURN
	ENDIF

	PAR = GUI_ARGVAL(1)
	IF(PAR.LT.1 .OR. PAR.GT.2) THEN
	    RET_CODE = 11
	    RETURN
	ENDIF

	GIND = GUI_ARGVAL(2)
	IF(GIND.LT.1 .OR. GIND.GT.MAXIND) THEN
	    STATUS = 2
	    WRITE(STATUS_STR,902) 1,MAXIND
	    GOTO 100
	ENDIF

	DRW = GUI_ARGVAL(3)
        IF(DRW.LE.0 .OR. DRW.GT.KIKDRW(GIND)) THEN
	    STATUS = 3
	    WRITE(STATUS_STR,903) DRW
	    GOTO 100
	ENDIF
	
        GNUM = GTNTAB(TKIK,GIND)	
	
	IF(PAR.EQ.1) THEN
	 IF(DRW.EQ.KIKDRW(GIND)) THEN	
	   CALL GAMLOG(TKIK,GIND,DKKREC,KIKBLK)
	 ELSE	     	
	   CALL READW(GAMFDB(1,GNUM),DRW,DKKREC,ST)
  	   IF(ST.NE.0) THEN
	     CALL OPS('Failed to read '//CGFNAMES(GNUM),ST,DRW)
             WRITE(STATUS_STR,914) DRW
	     RET_CODE = 14
	     GOTO 100
   	   ENDIF
         ENDIF    	 
	ENDIF	

	WINNING_NUM = GUI_ARGVAL(4)
	IF(WINNING_NUM.LT.0 .OR. 
     *     WINNING_NUM.GT.DKKMAX) THEN
	    STATUS = 4
	    WRITE(STATUS_STR,904) WINNING_NUM
	    GOTO 100
	ENDIF

	IF(PAR.EQ.1 .AND. DKKSTS.EQ.GAMBFD) THEN
	    DKKWIN = WINNING_NUM
	    DKKSTS = GAMEN1
	    CUR_GIND = GIND
	    CUR_GTYP = TKIK
	    CUR_DRAW = DRW
	ELSEIF(PAR.EQ.2 .AND. DKKSTS.AND.GAMEN1) THEN
	    DKKHLD = WINNING_NUM
	    DKKSTS = GAMENV

            IF(DKKWIN.NE.DKKHLD) THEN
                DKKSTS=GAMBFD
                STATUS = 10
	        WRITE(STATUS_STR,910)
	        GOTO 100
            ENDIF
            
            CALL MLTWIN(GNUM,DRW,ST)
            IF(ST.NE.0) THEN
               DKKSTS=GAMBFD
	       RET_CODE = 11
	      RETURN
	    ENDIF
            

            IF(DRW.EQ.KIKDRW(GIND)) THEN
              CALL FASTSET(0,BUF,CDLEN)
              BUF(1) = 2
              BUF(2) = DKKWIN
              BUF(3) = TCKIK
              BUF(6) = 'GUI '
              BUF(8) = GIND
              CALL QUECMD(BUF,ST)
	      IF(ST.NE.0) THEN
                 DKKSTS=GAMBFD
	         RET_CODE = 11
	         RETURN
	      ENDIF
 
              CALL FASTSET(0,BUF,CDLEN)
              BUF(1) = 1
              BUF(2) = DKKSTS
              BUF(3) = TCKIK
              BUF(6) = 'GUI '
              BUF(8) = GIND
              CALL QUECMD(BUF,ST)
	      IF(ST.NE.0) THEN
                 DKKSTS=GAMBFD
	         RET_CODE = 11
	         RETURN
	      ENDIF
	    ELSE
	      CALL WRITEW(GAMFDB(1,GNUM),DRW,DKKREC,ST)
              IF(ST.NE.0) THEN
                STATUS = 15
         	CALL OPS('Failed to write '//CGFNAMES(GNUM),ST,DRW)
                WRITE(STATUS_STR,915) DRW
                GOTO 100
              ENDIF 
      	      CALL JORESULT(GIND,DRW)	    	  
	    ENDIF
	ELSEIF(DKKSTS.EQ.GAMENV) THEN
	    STATUS = 12
	    WRITE(STATUS_STR,912) 
	    GOTO 100
	ELSE
	    STATUS = 13
	    WRITE(STATUS_STR,913) PAR,DKKSTS
	    GOTO 100
	ENDIF
C
C SEND DATA TO GUI
C
100	CONTINUE
	CALL GUIARG_INIT()
C
	NUM_COLS = 2
	NUM_ROWS = 1
	CALL GUIARG_NEXT_SET(OUTBUF,NUM_COLS)
C
C STATUS BACK
C
	CALL GUIARG_INT4(OUTBUF,STATUS)	
	CALL GUIARG_CHAR(OUTBUF,%REF(STATUS_STR),40)	
C
	CALL GUIARG_SET_MESLEN(MES_LEN)
C
	RETURN
902	FORMAT('Invalid game index: <',I8,' or >',I8)
903	FORMAT('Invalid Draw#: ',I8)
904	FORMAT('Invalid Number of winning numbers: ',I8)
905	FORMAT('Invalid Number of bonus numbers: ',I8)
906	FORMAT('Invalid number < 1 or > ',I2,' entered')
907	FORMAT('Duplicate numbers entered')
908	FORMAT('Invalid verification number < 1 or > ',I2,' entered')
909	FORMAT('Duplicate verification numbers entered')
910	FORMAT('Winning number verification error.')
911	FORMAT('Bonus numbers verification error.')
912	FORMAT('Winning number already entered and verified.')
913	FORMAT('Invalid - req mode:',I2,' game status:',I2)
914	FORMAT('Error reading draw file for draw:',I5)
915	FORMAT('Error writting draw file for draw:',I5)
	END
