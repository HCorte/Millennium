C GUICMD_018.FOR
C
C V02 13-FEB-2001 HXK ALLOW WINNING NUMBERS TO BE ENTERED
C                     FOR PREVIOUS DRAWS
C V01 06-FEB-2001 HXK INITIAL RELEASE
C
C TOTOGOLO WINNING NUMBERS ENTRY COMMAND
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
	SUBROUTINE GUICMD_018(OUTBUF,MES_LEN,RET_CODE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GUIMPRM.DEF'
	INCLUDE 'INCLIB:GUIARGS.DEF'
	INCLUDE 'INCLIB:RESCOM.DEF'
	INCLUDE 'INCLIB:TGLCOM.DEF'
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
	INTEGER*4 N_WINNING_NUMS
	INTEGER*4 BUF(CDLEN)
	INTEGER*4 STATUS
	CHARACTER*40 STATUS_STR
	INTEGER*4 IND
	INTEGER*4 I,J
	INTEGER*4 OFF
	INTEGER*4 TEMP
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
	IF(DRW.LT.0 .OR. DRW.GT.TGLDRW(GIND)) THEN
	    STATUS = 3
	    WRITE(STATUS_STR,903) DRW
	    GOTO 100
	ENDIF
        GNUM = GTNTAB(TTGL,GIND)

	IF(PAR.EQ.1) THEN
	 IF(DRW.EQ.TGLDRW(GIND)) THEN	
	   CALL GAMLOG(TTGL,GIND,DTGREC,TGLBLK)
	 ELSE
	   CALL READW(GAMFDB(1,GNUM),DRW,DTGREC,ST)
  	   IF(ST.NE.0) THEN
	     CALL OPS('Failed to read '//CGFNAMES(GNUM),ST,DRW)
             WRITE(STATUS_STR,914) DRW
	     RET_CODE = 14
	     GOTO 100
   	   ENDIF	 		 	
	 ENDIF	
	ENDIF 	  

	N_WINNING_NUMS = GUI_ARGVAL(4)
	IF(N_WINNING_NUMS.LT.1 .OR. 
     *     N_WINNING_NUMS.GT.DTGMAX) THEN
	    STATUS = 4
	    WRITE(STATUS_STR,904) N_WINNING_NUMS
	    GOTO 100
	ENDIF

	IND = 5

	IF(PAR.EQ.1 .AND. DTGSTS.EQ.GAMBFD) THEN
	    DO I=1,N_WINNING_NUMS
	       DO J=1,2
	          IF(GUI_ARGVAL(IND).EQ.0 .OR.       
     *               GUI_ARGVAL(IND).EQ.1 .OR.      
     *               GUI_ARGVAL(IND).EQ.2 .OR.
     *               GUI_ARGVAL(IND).EQ.3) THEN  
	              DTGWIN(J,I) = GUI_ARGVAL(IND)
	              IND = IND + 1
	          ELSE
	              STATUS = 6
	              WRITE(STATUS_STR,906) GUI_ARGVAL(IND)
	              GOTO 100
	          ENDIF
	       ENDDO
	    ENDDO
	    DTGSTS = GAMEN1
	    CUR_GIND = GIND
	    CUR_GTYP = TTGL
	    CUR_DRAW = DRW
	ELSEIF(PAR.EQ.2 .AND. DTGSTS.AND.GAMEN1) THEN
	    DO I=1,N_WINNING_NUMS
	       DO J=1,2
	          IF(GUI_ARGVAL(IND).EQ.0 .OR.   
     *               GUI_ARGVAL(IND).EQ.1 .OR.     
     *               GUI_ARGVAL(IND).EQ.2 .OR.    
     *               GUI_ARGVAL(IND).EQ.3) THEN    
	              DTGHLD(J,I) = GUI_ARGVAL(IND)
	              IND = IND + 1
	          ELSE
	              STATUS = 8
	              WRITE(STATUS_STR,908) GUI_ARGVAL(IND)
	              GOTO 100
	          ENDIF
	       ENDDO
	    ENDDO
	    DTGSTS = GAMENV
            DO I=1,N_WINNING_NUMS
	       DO J=1,2
                  IF(DTGWIN(J,I).NE.DTGHLD(J,I)) THEN
                     DTGSTS = GAMBFD
                     STATUS = 10
	             WRITE(STATUS_STR,910)
	             GOTO 100
                  ENDIF
	       ENDDO
            END DO
            	
            CALL MLTWIN(GNUM,DRW,ST)	
            IF(ST.NE.0) THEN
               DTGSTS=GAMBFD
	       RET_CODE = 11
	      RETURN
	    ENDIF
            

            IF(DRW.EQ.TGLDRW(GIND)) THEN
              CALL FASTSET(0,BUF,CDLEN)
              BUF(1) = 2
              BUF(2) = DRW
	      IF(DRW.NE.TGLDRW(GIND) .AND. DRW.NE.0) THEN
	         DTGSTS = GAMBFD !because current draw is not being updated
	      ENDIF
              BUF(3) = TCTGL
              BUF(6) = 'GUI '
              BUF(8) = GIND
              OFF = 0
	      TEMP = 0
              DO I=1,DTGMAX
	         TEMP = IOR(ISHFT(DTGWIN(1,I),4),DTGWIN(2,I))
                 CALL ISBYTE(TEMP,BUF(9),OFF)
                 OFF=OFF+1
              ENDDO
              CALL QUECMD(BUF,ST)
	      IF(ST.NE.0) THEN
                 DTGSTS=GAMBFD
	         RET_CODE = 11
	         RETURN
	      ENDIF
	      IF(DRW.EQ.TGLDRW(GIND) .OR. DRW.EQ.0) THEN
                 CALL FASTSET(0,BUF,CDLEN)
                 BUF(1) = 1
                 BUF(2) = GAMENV
                 BUF(3) = TCTGL
                 BUF(6) = 'GUI '
                 BUF(8) = GIND
                 CALL QUECMD(BUF,ST)
	         IF(ST.NE.0) THEN
                    DTGSTS=GAMBFD
	            RET_CODE = 11
	            RETURN
	         ENDIF
	      ENDIF
	    ELSE
              CALL WRITEW(GAMFDB(1,GNUM),DRW,DTGREC,ST)
              IF(ST.NE.0) THEN
                STATUS = 15
         	CALL OPS('Failed to write '//CGFNAMES(GNUM),ST,DRW)
                WRITE(STATUS_STR,915) DRW
                GOTO 100
              ENDIF 
      	      CALL TGRESULT(GIND,DRW)    	    	  	    	  
	    ENDIF  
	ELSEIF(DTGSTS.EQ.GAMENV) THEN
	    STATUS = 12
	    WRITE(STATUS_STR,912) 
	    GOTO 100
	ELSE
	    STATUS_STR = 'Bad request combination'
	    STATUS = 13
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
906	FORMAT('Invalid number ',I2,' entered')
908	FORMAT('Invalid verification number ',I2,' entered')
910	FORMAT('Winning numbers verification error.')
912	FORMAT('Winning results already entered and verified.')
914	FORMAT('Error reading draw file for draw:',I5)
915	FORMAT('Error writting draw file for draw:',I5)
	END
