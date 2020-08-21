C
C SUBROUTINE X2SEGSNP
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2GTXSNP.FOV                                 $
C  $Date::   17 Apr 1996 16:20:22                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C ** Source - x2gtxsnp.for **
C
C X2DLYSNP.FOR
C
C
C X2X Upgrade: 22-FEB-96 wsm Added AGTINF.DEF, moved VISCOM.DEF for Finland.
C
C V01 13-DEC-94 GPR RELEASED FOR UK
C
C This routine will display the number of terminals requesting a 
C segment, for each segment
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE X2GTXSNP(CLINE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:REQCOM.DEF'
C
	INTEGER*4   CLINE(*)
	INTEGER*4   KEY_BEG_SAP,		      !Display Selection
     *		    KEY_INTERVAL,
     *	            KEY_NUM,
     *              KEY_LOOP
	PARAMETER  ( KEY_BEG_SAP=1,
     *		    KEY_INTERVAL=2  ,
     *	            KEY_LOOP=3)
C
        INTEGER*4 NUM_KEYS, POS, SAP
	INTEGER*4 BEG_SAP /0/
	INTEGER*4 BEG_PORT /0/	! ONLY USED IN GTXDATA() 
C				! GTXDATA CHANGES THIS AND NEEDS THIS
C				! ONLY CLEARED IF BEG_SAP IS SET
	INTEGER*4 INTERVAL /0/ 
	INTEGER*4 SAP_INTERVAL
	PARAMETER (NUM_KEYS=7)                !NUMBER OF KEYS
	REAL*8 KEY_NAME(0:NUM_KEYS)
	DATA   KEY_NAME /'BSAP    ','INTerval',
     *	      'FORW    ','BACK    ','CLEAR   ',
     *	      'GTXData ','GTXTrans','GTXMsgs '/
	INTEGER*4 COUNT_TRASAP(0:X2X_SAP+1)
	INTEGER*4 NUM_BACK,NUM_FORW
	INTEGER*4 TOTAL_TRANS
	INTEGER*4 UPPER_BOUND
C
	LOGICAL	  IN_GTXDATA/.FALSE./
	LOGICAL	  IN_GTXTRANS/.TRUE./
	LOGICAL	  IN_GTXMSGS/.FALSE./
	COMMON /COM_PAR/ BEG_SAP,BEG_PORT,INTERVAL,IN_GTXDATA,
     *	      IN_GTXTRANS,IN_GTXMSGS	  ! GLOBAL VARIABLES
C
	INTEGER*4    ROW1,ROW2,NROWS   ! actual VISION row range
	 parameter ( ROW1 = 4,
     *	             ROW2 = 22,
     *	             NROWS= ROW2-ROW1+1 )   ! here: appr 20 rows
C
	NUM_BACK = 0
	NUM_FORW = 0
C
C ====== PROCESS KEYBOARD INPUT
C
	POS=1
C
	IF (BEG_SAP.EQ.0) BEG_SAP = 10
	IF (BEG_PORT.EQ.0) BEG_PORT = 1
	IF (INTERVAL.EQ.0) INTERVAL = 10
C
	IF (IN_GTXMSGS) THEN
		CALL X2GTXMSG(CLINE,IN_GTXTRANS,IN_GTXDATA)
C		! IF 'GTXTrans' OR 'GTXData' IS KEYED IN, WE SHOULD SET
C		! THE SWITCHES ACCORDINGLY. THEREFORE, WE NEED TO PASS
C		! IN_GTXTRANS, IN_GTXDATA
		IF (IN_GTXTRANS) GOTO 800
C		    ! GTX Transactions chosen
		IF (IN_GTXDATA) GOTO 700
C		    ! GTX Data chosen
		RETURN
	ENDIF
C
	CALL KEY(CLINE,KEY_NAME,NUM_KEYS+1,POS,KEY_NUM)
	GOTO (100,300,400,500,600,700,800,900) KEY_NUM
C
	GOTO 2000

C
100	CONTINUE
	CALL NUMB(CLINE,POS,BEG_SAP)
	BEG_PORT = 1
	IF (BEG_SAP.LT.1) BEG_SAP = 1
	IF (BEG_SAP.GT.X2X_SAP) BEG_SAP = X2X_SAP
	GOTO 2000
C
300	CONTINUE
	CALL NUMB(CLINE,POS,INTERVAL)
	IF (INTERVAL.EQ.0) INTERVAL = 10
	GOTO 2000
C
400	CONTINUE
	CALL NUMB(CLINE,POS,NUM_FORW)
	IF (NUM_FORW.LE.0) NUM_FORW = 1
	IF (.NOT.IN_GTXDATA)
     *	  BEG_SAP = MIN0(BEG_SAP + NUM_FORW*NROWS*INTERVAL,X2X_SAP)
	NUM_BACK = 0
	GOTO 2000
C
500	CONTINUE
	CALL NUMB(CLINE,POS,NUM_BACK)
	IF (NUM_BACK.LE.0) NUM_BACK = 1
	IF (.NOT.IN_GTXDATA)
     *	  BEG_SAP = MAX0(BEG_SAP - NUM_BACK*NROWS*INTERVAL,1)
	NUM_FORW = 0
	GOTO 2000
C
600	CONTINUE
	IF (IN_GTXTRANS) THEN
	   DO SAP=1,X2X_SAP
	       X2XE_STATION_MSG_CNT(SAP) = 0
	   END DO
	ENDIF
	GOTO 2000
C
700	CONTINUE
	IN_GTXDATA=.TRUE.
	IN_GTXTRANS=.FALSE.
	IN_GTXMSGS=.FALSE.
	CALL X2GTXDATA(BEG_SAP,BEG_PORT,0,0)
	GOTO 3000
C
800	CONTINUE
	IN_GTXTRANS=.TRUE.
	IN_GTXDATA = .FALSE.
	IN_GTXMSGS = .FALSE.
	GOTO 2000
C
900	CONTINUE
	IN_GTXMSGS = .TRUE.
	IN_GTXDATA = .FALSE.
	IN_GTXTRANS = .FALSE.
	CALL X2GTXMSG(CLINE,IN_GTXTRANS,IN_GTXDATA)
	RETURN
C
C====   HISTOGRAM CALCULATIONS START FROM HERE
C
2000	CONTINUE
C
	IF (IN_GTXDATA) THEN
		CALL X2GTXDATA(BEG_SAP,BEG_PORT,NUM_FORW,NUM_BACK)
		GOTO 3000
	ENDIF
C
	DO SAP=0,X2X_SAP
	    COUNT_TRASAP(SAP) = 0
	END DO
C
	TOTAL_TRANS = 0
	DO SAP=1,X2X_SAP
	    SAP_INTERVAL = (SAP-1)/INTERVAL
	    COUNT_TRASAP(SAP_INTERVAL) = 
     *	       COUNT_TRASAP(SAP_INTERVAL) + X2XE_STATION_MSG_CNT(SAP)
	    TOTAL_TRANS = TOTAL_TRANS + X2XE_STATION_MSG_CNT(SAP)
	END DO
C
	WRITE(CLIN2,9010) BEG_SAP,X2XE_STATION_MSG_CNT(BEG_SAP),
     *	  TOTAL_TRANS
C
	WRITE(CLIN1,9000)
	WRITE(CLIN23,9023)'BSAP <sapnr>, ',
     *	  'INTerval <nr>, ',
     *	  ' BACKward <nr>,','FORWard <nr>, CLEAR'
C
	UPPER_BOUND = (((X2X_SAP+INTERVAL-1)/INTERVAL)*INTERVAL 
     *		       - ((BEG_SAP+INTERVAL-1)/INTERVAL)*INTERVAL)
     *			  /INTERVAL + 1
C
	CALL X2HISTD(COUNT_TRASAP(0),(BEG_SAP+INTERVAL-1)/INTERVAL,
     *	  UPPER_BOUND,
     *	  INTERVAL,'Saps')
C
3000	CONTINUE
	WRITE(CLIN22,9023)'BSAP <sapno>, ',
     *	  'INTerval <no>, ',
     *	  ' BACKward <no>,','FORWard <no>, CLEAR, '
C
	WRITE(CLIN23,9023)'GTXData, ','GTXTrans, ','GTXMsg '
C
C PROGRAM EXIT
C
	RETURN
C
C     ================== Format Statements =====================
C
9000	FORMAT('GTX Transactions Volume - ',' Init',I4.2,':',I2.2,':',I2.2)
9010	FORMAT('Beginning SAP: ',I3,' Nr of transactions: ',I6,4X,
     *	  'Total nr. of trans. ',I6)
9023	FORMAT(8(A,1X))
	END
