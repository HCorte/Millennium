C
C SUBROUTINE X2GTXMSG
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2GTXMSG.FOV                                 $
C  $Date::   17 Apr 1996 16:20:16                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C ** Source - x2gtxsnp.for **
C
C X2GTXMSG.FOR
C
C
C X2X Upgrade: 22-FEB-96 wsm Added AGTINF.DEF, moved VISCOM.DEF for Finland.
C
C V01 13-DEC-94 GPR RELEASED FOR UK
C
C This routine displays a histogram of number of messages in one frame
C Displays are per SAP or for all SAP's
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
	SUBROUTINE X2GTXMSG(CLINE,IN_GTXTRANS,IN_GTXDATA)
	IMPLICIT NONE

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
        LOGICAL   IN_GTXDATA
        LOGICAL   IN_GTXTRANS
C
	INTEGER*4    ROW1,ROW2,NROWS   ! actual VISION row range
	 parameter ( ROW1 = 4,
     *	             ROW2 = 22,
     *	             NROWS= ROW2-ROW1 )   ! here: appr 20 rows
C
        INTEGER*4 NUM_KEYS, I, POS
	INTEGER*4 SAPNR /10/
	INTEGER*4 INTERVAL /0/ 
	INTEGER*4 MSG_INTERVAL
	PARAMETER (NUM_KEYS=7)                !NUMBER OF KEYS
	REAL*8 KEY_NAME(0:NUM_KEYS)
	DATA   KEY_NAME /'SAPNR   ','INTerval',
     *	      'FORW    ','BACK    ','CLEAR   ',
     *	      'GTXData ','GTXTrans','BMSG    '/
C
	INTEGER*4 COUNT_MSGSAP(0:X2X_MAX_MSGS_PER_FRAME_INTERVAL)
	INTEGER*4 NUM_BACK,NUM_FORW
	INTEGER*4 TOTAL_MSGS
	INTEGER*4 UPPER_BOUND
	INTEGER*4 BMSG/1/
	INTEGER*4 START,END,SAPCNT
C
	COMMON /COM_GTXMSG/ SAPNR,INTERVAL

	NUM_BACK = 0
	NUM_FORW = 0
C
C ====== PROCESS KEYBOARD INPUT
C
	POS=1
	CALL KEY(CLINE,KEY_NAME,NUM_KEYS+1,POS,KEY_NUM)
	GOTO (100,300,400,500,600,700,800,900) KEY_NUM
C
	IF (INTERVAL.LE.0 .OR. INTERVAL.GT.63) INTERVAL = 2
	GOTO 2000

C
100	CONTINUE
	CALL NUMB(CLINE,POS,SAPNR)
	IF (SAPNR.LT.1) SAPNR = 1
	IF (SAPNR.GT.X2X_SAP.AND.SAPNR.NE.255) SAPNR = X2X_SAP
	GOTO 2000
C
300	CONTINUE
	CALL NUMB(CLINE,POS,INTERVAL)
	IF (INTERVAL.LE.0 .OR. INTERVAL.GT.63) INTERVAL = 2
	GOTO 2000
C
400	CONTINUE
	CALL NUMB(CLINE,POS,NUM_FORW)
	IF (NUM_FORW.LE.0) NUM_FORW = 1
     	BMSG = MIN0(BMSG + NUM_FORW*NROWS*INTERVAL,
     *	      X2X_MAX_MSGS_PER_FRAME_INTERVAL)
	NUM_BACK = 0
	GOTO 2000
C
500	CONTINUE
	CALL NUMB(CLINE,POS,NUM_BACK)
	IF (NUM_BACK.LE.0) NUM_BACK = 1
     	BMSG = MAX0(BMSG - NUM_BACK*NROWS*INTERVAL,1)
	NUM_FORW = 0
	GOTO 2000
C
600	CONTINUE
	IF (SAPNR.EQ.255) THEN
	    START = 1
	    END = X2X_SAP
	ELSE
	    START = SAPNR
	    END = SAPNR
	ENDIF
C
	DO SAPCNT = START,END
	   DO I=1,X2X_MAX_MSGS_PER_FRAME_INTERVAL
	         X2XE_MSGS_PER_FRAME(I,SAPCNT) = 0
           END DO
	END DO
	GOTO 2000
C
700	CONTINUE
	IN_GTXDATA=.TRUE.
	IN_GTXTRANS=.FALSE.
	RETURN
C
800	CONTINUE
	IN_GTXTRANS=.TRUE.
	IN_GTXDATA = .FALSE.
	RETURN
C
900	CONTINUE
	CALL NUMB(CLINE,POS,BMSG)
	IF (BMSG.LT.1) BMSG = 1
	GOTO 2000
C
C
C====   HISTOGRAM CALCULATIONS START FROM HERE
C
2000	CONTINUE
C
	DO I=0,X2X_MAX_MSGS_PER_FRAME_INTERVAL
	    COUNT_MSGSAP(I) = 0
	END DO
C
	TOTAL_MSGS = 0
	IF (SAPNR.EQ.255) THEN
	    START = 1
	    END = X2X_SAP
	ELSE
	    START = SAPNR
	    END = SAPNR
	ENDIF
C
	DO SAPCNT=START,END
	    DO I=1,X2X_MAX_MSGS_PER_FRAME_INTERVAL
		MSG_INTERVAL = (I-1)/INTERVAL
		COUNT_MSGSAP(MSG_INTERVAL) = 
     *	     COUNT_MSGSAP(MSG_INTERVAL) + X2XE_MSGS_PER_FRAME(I,SAPCNT)
	        TOTAL_MSGS = TOTAL_MSGS + X2XE_MSGS_PER_FRAME(I,SAPCNT)
	    END DO
	END DO
C
	WRITE(CLIN2,9010) SAPNR,TOTAL_MSGS
C
	WRITE(CLIN1,9000)
	WRITE(CLIN23,9023)'BSAP <sapnr>, ',
     *	  'INTerval <nr>, ',
     *	  ' BACKward <nr>,','FORWard <nr>, CLEAR'
C
	UPPER_BOUND = 
     *   (((X2X_MAX_MSGS_PER_FRAME_INTERVAL+INTERVAL-1)/
     *     INTERVAL)*INTERVAL
     *	  - ((BMSG+INTERVAL-1)/INTERVAL)*INTERVAL)/INTERVAL + 1
C
	CALL X2HISTD(COUNT_MSGSAP(0),(BMSG+INTERVAL-1)/INTERVAL,
     *	  UPPER_BOUND,
     *	  INTERVAL,'Msgs')
C
3000	CONTINUE
	WRITE(CLIN22,9023)'SAPNR <sapno>, BMSG <msg interv>',
     *	  'INTerval <no>, '
C
	WRITE(CLIN23,9023) ' BACKward <no>,','FORWard <no>, CLEAR, ',
     *    'GTXData, ','GTXTrans '
C
C PROGRAM EXIT
C
	RETURN
C
C     ================== Format Statements =====================
C
9000	FORMAT('GTX Messages Per Frame - ',I4.2,':',I2.2,':',I2.2)
9010	FORMAT('SAP Number: ',I3,'   Total no. of Messages: ',I6)
9023	FORMAT(8(A,1X))
	END
