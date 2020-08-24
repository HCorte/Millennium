C GUISNP.FOR
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode Island,
C and contains confidential and trade secret information. It may not be
C transferred from the custody or control of GTECH except as authorized in
C writing by an officer of GTECH. Neither this item nor the information it
C contains may be used, transferred, reproduced, published, or disclosed,
C in whole or in part, and directly or indirectly, except as expressly
C authorized by an officer of GTECH, pursuant to written agreement.
C
C Copyright 1994 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GUISNP(CLINE)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:GUIMCOM.DEF'
	INCLUDE 'INCLIB:GUILCOM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
C
	INTEGER*4 CLINE(4), VALUE
	INTEGER*4 ST, IND1, IND2, POS, KEYNUM
	INTEGER*4 START_CONN/1/, CONX, AUTHX, TOT_CLIENTS
	INTEGER*4 NOFTLSIG
	EXTERNAL  NOFTLSIG
C
	RECORD /SOCKADR_BYT/ REM_ADR
C
	CHARACTER*4 STAT1(0:1)
	DATA STAT1/'actv','supr'/
C
	REAL*8      K(5)
C
	DATA     K/'SUPPth  ','*No_use','DISPconn','PORTnr  ','TIMOUcln'/
C
	CALL LIB$ESTABLISH(NOFTLSIG)
C
	IF (START_CONN.LE.0) START_CONN = 1
C
C GET INPUT
C
	POS=1
	CALL KEY(CLINE,K,5,POS,KEYNUM)
	IF(POS.GT.40) GOTO 3000                    !NO INPUT
	IF(KEYNUM.EQ.0)GOTO 10                     !INPUT ERROR
	IF(KEYNUM.NE.2) THEN
	  CALL NUMB(CLINE,POS,VALUE)                 !GET VALUE
	  IF(VALUE.LT.0)  GOTO 20
	ENDIF
C
	GOTO ( 100, 200, 300, 400, 500 ) KEYNUM
C
10	CONTINUE
	WRITE(CLIN23,9010)
9010	FORMAT('Input error')
	GOTO 3000
C
20	CONTINUE
	WRITE(CLIN23,9020)
9020	FORMAT('Value error')
	GOTO 3000
C
C
C SUPRESS Server FLAG
C
100	CONTINUE
	IF(VALUE.NE.0.AND.VALUE.NE.1) GO TO 20
	CON_PATH_THRU_SUPPRESS = VALUE
	IF(VALUE.EQ.1) THEN
	  CLIN23(1)='Path-Thru Server Suppressed'
	ELSE
	  CLIN23(1)='Path-Thru Server Enabled'
	ENDIF
	GOTO 3000
C
C Start new version of GLITSMSG.FIL
C
200	CONTINUE
	GUI_REOPEN_GTRK = .TRUE.
	CALL RELSE (TSKNAM(SPEF), ST)
	WRITE(CLIN23,9030)
9030	FORMAT('Print file GLITSMSG.FIL;-1 to see GLITS messages.')
	GOTO 3000
C
C Starting connection change
C
300	CONTINUE
	IF(VALUE.LT.1.OR.VALUE.GT.GUI_MAX_CONN) GO TO 20
	START_CONN = VALUE
C
	GOTO 3000
C
C Port number change
C
400	CONTINUE
	IF(VALUE.LT.1000.OR.VALUE.GT.9999) GO TO 20
	GUI_PORT = VALUE
C
	GOTO 3000
C
C Client timout value change
C
500	CONTINUE
	IF(VALUE.LT.0.OR.VALUE.GT.3600-1) GO TO 20	! less than 1 hour
	GUI_WATCH_TIME = VALUE*1000
C
	GOTO 3000
C
C Encodde the shapshor
C                      
3000	CONTINUE
C
	IND1=1
	WRITE(XNEW(IND1), 9040)
9040	FORMAT('GUIMGR/GUILINK Snapshot')
	IND1=IND1+1
C
	IND1=IND1+1
C
	IND2 = CON_PATH_THRU_SUPPRESS

C	WRITE(XNEW(IND1),9050) K(1),IND2,STAT1(IND2),
C     *			  K(2)
C9050	FORMAT(1X,'*',A8,I1,' Path-Thru GUIer........',A4,
C     *         1X,'*',A8,1X,' Start new GLITSMSG.FIL..',4X)

	WRITE(XNEW(IND1),9050) K(1),IND2,STAT1(IND2),
     *                        K(5), GUI_WATCH_TIME/1000
9050	FORMAT(1X,'*',A8,I1,' Path-Thru GUIer........',A4,
     *         1X,'*',A8,1X,' Time-out Client Scnds...',I4)

	IND1=IND1+1

	WRITE(XNEW(IND1),9060) K(3), START_CONN,
     *			  K(4), GUI_PORT
9060	FORMAT(1X,'*',A8,1X,' Display Connection......',I4,
     *	       1X,'*',A8,1X,' GUIice Port nr.........',I4)

	IND1=IND1+1

C
C	WRITE(XNEW(IND1),9070) K(5), GUI_WATCH_TIME/1000
C9070	FORMAT(1X,'*',A8,1X,' Time-out Client Scnds...',I4)
C	IND1=IND1+1
C
	XNEW(IND1) = ' '
	IND1=IND1+1
C
	WRITE(XNEW(IND1), 9100)
     *		'............USER NAME...........',
     *		'MSGS-IN', 'MSG-OUT', ' ERRORS',
     *		'DATA-CLS','CONN','TCP ADR'
9100	FORMAT(1X,T2,A32,
     *		 T35,A7,   T43,A7,    T51,A7,
     *		 T59,A8,   T68,A4,    T73,A7)
	IND1=IND1+1
C
	XNEW(IND1) = ' '
	IND1=IND1+1
C
	DO 3200 CONX = START_CONN, GUI_MAX_CONN
C
	  IF( .NOT. GUI_CONN_SIGNED_ON(CONX) ) GOTO 3200
C
	  IF(IND1 .GT. 21) GOTO 4000
C
	  CALL MOVBYT(GUI_CONN_RHOST_ADD(1,CONX),1,REM_ADR,1,16)
	  AUTHX = GUI_CONN_AUTH_INX(CONX)
C
	  WRITE(XNEW(IND1), 9120)
     *		GUI_AUTH_NAME(AUTHX),
     *		GUI_READS(CONX),GUI_WRITES(CONX),
     *		GUI_READERRS(CONX)+GUI_WRITEERRS(CONX),
     *		GUI_CONN_DATA_CLASS_BITS(CONX),
     *		CONX, ZEXT(REM_ADR.ADRS_BYT(3)),
     *		ZEXT(REM_ADR.ADRS_BYT(4))
9120	  FORMAT(1X,T2,A32,
     *		 T35,I7,   T43,I7,    T51,I7,
     *		 T59,Z8.8, T68,I4,    T73,I3.3,'.',I3.3)
	  IND1 = IND1 + 1
C
3200	CONTINUE
C
	DO 3300 IND2 = IND1, 21
	  XNEW(IND2) = ' '
3300	CONTINUE
C
C
4000	CONTINUE
C
	TOT_CLIENTS = 0
	DO 4200 CONX = 1, GUI_MAX_CONN
	  IF( .NOT. GUI_CONN_SIGNED_ON(CONX) ) GOTO 4200
	  TOT_CLIENTS = TOT_CLIENTS + 1
4200	CONTINUE
C
	WRITE(XNEW(22), 9080), TOT_CLIENTS
9080	FORMAT(1X,
     *  '************* TOTAL',I4,' CLIENTS SIGNED ON **************')
C
	RETURN
C
	END
