C SUBROUTINE MSCSNP
C
C V05 15-JUN-2000 OXK CLEANUP W/ WARNINGS=ALL
C V04 22-FEB-1996 wsm X2X Upgrade: Added PRMAGT.DEF, AGTINF.DEF for Finland.
C V03 04-AUG-1995 SCD REMOVE "*" BEFORE MSCSTs FIELD SINCE WE CANNOT
C			MODIFY MSC STATUS FROM THIS SNAPSHOT
C V02 13-DEC-1993 PJS MODIFIED FOR NEW MSC STUFF.
C V01 18-MAR-1991 RRB INITIAL RELEASE
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Purpose:
C	THIS SUBROUTINE WILL DISPLAY THE MATRIX SWITCH
C	CONTROL INFORMATION AND PORT CONFIGURATION.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE MSCSNP(CMDLIN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:MSCCOM.DEF'
	INCLUDE 'INCLIB:MSCCMDS.DEF'
	INCLUDE 'INCLIB:MSCMSGS.DEF'
C
	INTEGER*4   DISDIM                          !Dimension of table
	PARAMETER  (DISDIM  = 5)
C
	INTEGER*4   CMDLIN(20)                      !Command Line
        INTEGER*4   CBUF(CDLEN)                     !Command buffer
 	INTEGER*4   DISCNT                          !Display counter
	INTEGER*4   DISTBL(DISDIM,MSC_MAX_PORTS)    !Display table
	INTEGER*4   NETIDX,LOCIDX,SAPIDX            !Indices into DISTBL
        INTEGER*4   TYPIDX,STSIDX
	INTEGER*4   LOCAL_PORT			    !Selected local port
	INTEGER*4   NET_PORT                        !Network port selected
	INTEGER*4   OLD_PORT			    !Old local port for switch
	INTEGER*4   NEW_PORT			    !New local port for switch
	INTEGER*4   PVC_CIRCUIT, STN
	INTEGER*4   CURTIM                          !Current time
	INTEGER*4   ELAPSED                         !Time since last update
	INTEGER*4   MAXDELAY                        !Max time to wait to update
	INTEGER*4   MAXDISP                         !Max lines displayed
	INTEGER*4   BEGIDX                          !Display indices
	INTEGER*4   NETSRT,LOCSRT,SAPSRT            !Sort keys
	INTEGER*4   SRTIDX			    !Sort Index			
        INTEGER*4   TYPSRT,STSSRT
	INTEGER*4   KEY1,KEY2,KEY3                  !Sort keys
	INTEGER*4   LINIDX                          !Print line index
	INTEGER*4   SEC,MIN,HR                      !Time display
	INTEGER*4   LSTTIM, LSTSRT, LSTIDX
	CHARACTER*7  PRTSTATE(0:1)                  !Port State from MSC
	CHARACTER*7  PRTTYPE(0:4)                   !Port type
	CHARACTER*10 SUPSTATE(0:1)		    !Suppression state
	CHARACTER*16 MSCSTATE(0:10)                 !MSC connect status
	CHARACTER*15 MSCCONF(0:4)                   !MSC Configuratn flag
	CHARACTER*10 AUTOSTATE(0:1)	            !Auto Switching flag
	LOGICAL      DISPLAY /.FALSE./              !Display info flag
	LOGICAL      ASSIGNED
	REAL*8       CMDOPT(8)                      !Command Line Options
	INTEGER*4    I, LOC, POS, VALUE, KEYNUM, ST !Other
C
	PARAMETER  (MAXDELAY = 30)
	PARAMETER  (MAXDISP = 10)
	PARAMETER  (LOCIDX  = 1)
	PARAMETER  (NETIDX  = 2)
	PARAMETER  (SAPIDX  = 3)
	PARAMETER  (TYPIDX  = 4)
	PARAMETER  (STSIDX  = 5)
	PARAMETER  (LOCSRT  = 1)
	PARAMETER  (NETSRT  = 2)
	PARAMETER  (SAPSRT  = 3)
	PARAMETER  (TYPSRT  = 4)
	PARAMETER  (STSSRT  = 5)
C
	DATA        PRTSTATE    /'Normal','Alarmed'/
	DATA        PRTTYPE     /'invalid','   X.21','   X.25',
     *	                         ' dialup','  Async'/
	DATA        SUPSTATE    /'Enabled','Supressed'/
 	DATA	    MSCSTATE    /'Down', 'Connect Pending', 'On Line',
     *                           'Request Logon', 'Logged On',
     *                           'Request Page Len', 'Page Len Set',
     *                           'Request Switch', 'Switch Selected',
     *                           'Request Startup', 'Active'/
	DATA        MSCCONF     /'Undefined','Check Pending',
     *                           'Receiving Conf','Conf Received',
     *                           'Conf verified'/
        DATA        AUTOSTATE   /'Disabled','Enabled'/
        DATA        CMDOPT      /'SUPMSC  ','MSCSTs  ','LPort   ',
     *                           'NPort   ','SORT    ','DISable ',
     *                           'ASSign  ','REAssign'/
	DATA        LSTTIM/0/,LSTSRT/0/,LSTIDX/0/
C
	CALL FASTSET(0,CBUF,CDLEN)
        CBUF(6)=IDNUM
	CALL GETTIM(CURTIM)
	ELAPSED = CURTIM - LSTTIM
C
C IF WE HAVE RECENTLY RECEIVED NEW CONFIGURATION
C FROM THE MSC THEN UPDATE DISPLAY TABLE
C
	IF(LSTTIM.NE.MSC_LAST_CONF_TIME.OR.
     *     ELAPSED.GT.MAXDELAY) THEN
	  DISCNT=0
	  DO 100 LOC = 1,MSC_MAX_PORTS
	    NET_PORT = MSC_LOCAL_TO_NETWORK(LOC)
	    IF(NET_PORT.NE.0) THEN
	      DISCNT=DISCNT+1
	      DISTBL(LOCIDX,DISCNT)=LOC
	      DISTBL(NETIDX,DISCNT)=NET_PORT
	      DISTBL(SAPIDX,DISCNT)=X2XPL_SAP(LOC)
	      DISTBL(TYPIDX,DISCNT)=X2XPN_TYPE(NET_PORT)
	      DISTBL(STSIDX,DISCNT)=MSC_PORT_STATUS(LOC)
	    ENDIF
100	  CONTINUE
	  DISPLAY = .TRUE.
	  LSTTIM = MSC_LAST_CONF_TIME
	  HR=LSTTIM/3600
	  MIN=(LSTTIM-HR*3600)/60
	  SEC=LSTTIM-(HR*3600+MIN*60)
	ENDIF
C
C CHECK FOR INPUT COMMAND LINE.
C
	POS=1
        CALL KEY(CMDLIN,CMDOPT,8,POS,KEYNUM)
        IF(KEYNUM.EQ.0) GOTO 300
        CALL NUMB(CMDLIN,POS,VALUE)             !GET VALUE
        IF(VALUE.LT.0) THEN                     !VALUE ERROR
          WRITE(CLIN23,9923)
          GOTO 8000
        ENDIF
        GOTO (210,220,230,240,250,260,270,280) KEYNUM
C
C CHANGE SUPMSC
C
210     CONTINUE
        IF(VALUE.LT.0.OR.VALUE.GT.1) THEN
           WRITE(CLIN23,9923)
           GOTO 8000
        ENDIF
	CBUF(1)=SUPMSC
	CBUF(2)=VALUE
	CBUF(3)=TCPAR
	GOTO 290
C
C CHANGE MSC CONNECTION STATUS
C (Only allow setting MSC to Down or Connect Request)
C
220	CONTINUE
        IF(VALUE.LT.MSC_DOWN.OR.VALUE.GT.MSC_REQ_ONLINE) THEN
           WRITE(CLIN23,9923)
           GOTO 8000
        ENDIF
	CBUF(1)=UPDSTS
	CBUF(2)=VALUE
	CBUF(3)=TCMSC
	GOTO 290
C
C GOTO LOCAL PORT INDEX
C
230 	CONTINUE
	IF(VALUE.GE.1.AND.VALUE.LE.MSC_MAX_PORTS) THEN
	  LOCAL_PORT = VALUE
	  DO 231 I = 1,DISCNT
	     IF(DISTBL(LOCIDX,I).EQ.VALUE) THEN
	        BEGIDX = I
	        GOTO 300
	     ENDIF
231       CONTINUE
	ENDIF
	BEGIDX = LSTIDX
	GOTO 300
C
C GOTO NETWORK PORT INDEX
C
240 	CONTINUE
	IF(VALUE.GE.1.AND.VALUE.LE.MSC_MAX_PORTS) THEN
	  DO 241 I = 1,DISCNT
	     IF(DISTBL(NETIDX,I).EQ.VALUE) THEN
	        BEGIDX = I
	        GOTO 300
	     ENDIF
241       CONTINUE
	ENDIF
	BEGIDX = LSTIDX
	GOTO 300
C
C CHANGE SORT INDEX
C
250 	CONTINUE
	IF(VALUE.GE.1.AND.VALUE.LE.DISDIM) THEN
	   SRTIDX = VALUE
	ELSE
	   SRTIDX = LSTSRT
	ENDIF
	GOTO 300
C
C DISable LINE
C
260	CONTINUE
	IF(LOCAL_PORT.EQ.0) THEN
          WRITE(CLIN23,99232)
          GOTO 8000
        ENDIF
C
C THIS LOCAL PORT MUST HAVE CURRENTLY DEFINED NETWORK PORT
C
	OLD_PORT = LOCAL_PORT
	NEW_PORT = 0
        NET_PORT = X2XPL_LOCAL_TO_NETWORK(LOCAL_PORT)
	IF(NET_PORT.LT.1.OR.NET_PORT.GT.X2X_NETWORK_PORTS) THEN
           WRITE(CLIN23,99233) LOCAL_PORT
           GOTO 8000
	ENDIF
	GOTO 285
C
C ASSign LINE
C
270	CONTINUE
	IF(LOCAL_PORT.EQ.0) THEN
          WRITE(CLIN23,99232)
          GOTO 8000
        ENDIF
	IF(VALUE.LT.1.OR.VALUE.GT.X2X_NETWORK_PORTS) THEN
	   WRITE(CLIN23,99234) VALUE
	   GOTO 8000
	ENDIF
C
C MAKE SURE IT'S NOT ALREADY ASSIGNED
C
	NET_PORT = X2XPL_LOCAL_TO_NETWORK(LOCAL_PORT)
	IF(NET_PORT.NE.0) THEN
	   WRITE(CLIN23,99235) LOCAL_PORT,NET_PORT
	   GOTO 8000
	ENDIF
C
C MAKE SURE THERE IS AT LEAST ONE STATION ASSIGNED TO THIS NETWORK PORT
C
	ASSIGNED = .FALSE.
	DO 275 PVC_CIRCUIT = 0,X2XPN_NUMPVC(VALUE)
           STN = X2XPN_PVC_TO_STATION(VALUE,PVC_CIRCUIT)
	   IF(STN.NE.0) THEN 
	      ASSIGNED = .TRUE.
	      GOTO 277
	   ENDIF
275	CONTINUE
C
277	CONTINUE
	IF(.NOT.ASSIGNED) THEN
	   WRITE(CLIN23,99238) VALUE
	   GOTO 8000
	ELSE
	   NET_PORT = VALUE
	   OLD_PORT = 0	   
	   NEW_PORT = LOCAL_PORT
	ENDIF
        GOTO 285
C
C REAssign LINE
C
280	CONTINUE
	IF(LOCAL_PORT.EQ.0) THEN
          WRITE(CLIN23,99232)
          GOTO 8000
        ENDIF
C
	IF(VALUE.LT.1.OR.VALUE.GT.X2X_LOCAL_PORTS) THEN
	   WRITE(CLIN23,99236) LOCAL_PORT
	   GOTO 8000
	ENDIF
C
C OLD LOCAL PORT MUST HAVE NETWORK PORT ASSIGNMENT
C
	OLD_PORT = LOCAL_PORT
	NEW_PORT = VALUE
        NET_PORT = X2XPL_LOCAL_TO_NETWORK(LOCAL_PORT)
	IF(NET_PORT.LT.1.OR.NET_PORT.GT.X2X_NETWORK_PORTS) THEN
           WRITE(CLIN23,99233) LOCAL_PORT
           GOTO 8000
	ENDIF
C
C NEW LOCAL PORT CANNOT ALREADY BE ASSIGNED
C
	IF(X2XPL_LOCAL_TO_NETWORK(NEW_PORT).NE.0) THEN
	   WRITE(CLIN23,99235) NEW_PORT,X2XPL_LOCAL_TO_NETWORK(NEW_PORT)
	   GOTO 8000
	ENDIF
C
	GOTO 285
C
C BUILD COMMAND
C
285	CONTINUE
	CBUF(1) = CONNECT_PORT
	CBUF(2) = 0
   	CBUF(3) = TCMSC
	CBUF(4) = NET_PORT
	CBUF(8) = OLD_PORT
	CBUF(9) = NEW_PORT
	WRITE(CLIN23,99237) OLD_PORT,NEW_PORT,NET_PORT
C
C QUE COMMAND TO GAME
C
290	CONTINUE
	CALL VISCMD(CBUF,ST)
	CALL XWAIT(2,1,ST)
C
C SORT THE DISPLAY TABLE ACCORDING TO DESIRED INFORMATION.
C DEFAULT TO LOCAL PORT SORT.
C
300	CONTINUE
	IF(SRTIDX.LE.0.OR.SRTIDX.GT.DISDIM) SRTIDX = 1
	IF(DISPLAY.OR.SRTIDX.NE.LSTSRT) THEN
	   IF(SRTIDX.EQ.NETSRT) THEN
	      KEY1=NETIDX
	      KEY2=0
	      KEY3=0
	   ELSE IF(SRTIDX.EQ.SAPSRT) THEN
	      KEY1=SAPIDX
	      KEY2=LOCIDX
	      KEY3=0
	   ELSE IF(SRTIDX.EQ.TYPSRT) THEN
	      KEY1=TYPIDX
	      KEY2=LOCIDX
	      KEY3=0
	   ELSE IF(SRTIDX.EQ.STSSRT) THEN
	      KEY1=STSIDX
	      KEY2=LOCIDX
	      KEY3=0
 	   ELSE
	      KEY1=LOCIDX
	      KEY2=0
	      KEY3=0
	   ENDIF
	   IF(DISCNT.GT.0)
     *        CALL I4XSORT(DISTBL,DISDIM,DISCNT,KEY1,KEY2,KEY3)
	   LSTSRT=SRTIDX
	ENDIF
C
C IF THE DISPLAY FLAG HAS BEEN SET, DISPLAY THE SCREEN
C STARTING AT THE CURRENT BEGINNING POSITION.
C
	WRITE(CLIN1,9010)
	WRITE(CLIN2,9020) HR,MIN,SEC
	WRITE(CLIN3,9030) P(SUPMSC), SUPSTATE(P(SUPMSC)),
     *                    MSCCONF(MSC_CONF_FLAG)
	WRITE(CLIN4,9040) MSCSTS, MSCSTATE(MSCSTS), 
     *                    AUTOSTATE(MSC_AUTO_SWITCH)
	WRITE(CLIN6,9060)
	WRITE(CLIN7,9070)
	WRITE(CLIN8,9080)
C
C OUTPUT INFORMATION.
C
	LINIDX = 10
	IF(BEGIDX.LE.0) BEGIDX = 1
	IF(BEGIDX.NE.LSTIDX) DISPLAY = .TRUE.
	IF(LOCAL_PORT.LE.0) LOCAL_PORT = BEGIDX
C
	IF(DISPLAY) THEN
	   IF(DISCNT.LE.0) THEN
	      WRITE(CLIN10,9900)
	      LINIDX = LINIDX + 1
	      GOTO 1050
	   ENDIF	
	   DO 1000 I = BEGIDX,BEGIDX+(MAXDISP*2),2
	      IF(I.GT.DISCNT) GOTO 1050
	      IF(DISTBL(LOCIDX,I+1).EQ.0) THEN
     	         WRITE(XNEW(LINIDX),9100)
     *                 DISTBL(LOCIDX,I), DISTBL(NETIDX,I),
     *                 DISTBL(SAPIDX,I), PRTTYPE(DISTBL(TYPIDX,I)),
     *                 PRTSTATE(DISTBL(STSIDX,I))
	      ELSE
     	         WRITE(XNEW(LINIDX),9101)
     *                 DISTBL(LOCIDX,I), DISTBL(NETIDX,I),
     *                 DISTBL(SAPIDX,I), PRTTYPE(DISTBL(TYPIDX,I)),
     *                 PRTSTATE(DISTBL(STSIDX,I)),DISTBL(LOCIDX,I+1),
     *                 DISTBL(NETIDX,I+1),DISTBL(SAPIDX,I+1),
     *                 PRTTYPE(DISTBL(TYPIDX,I+1)),
     *                 PRTSTATE(DISTBL(STSIDX,I+1))
	      ENDIF
	      LINIDX = LINIDX + 1
1000       CONTINUE
C
C CLEAR ANY UNUSED SLOTS.
C
1050	   CONTINUE
	   DO 1100 I=LINIDX,22
	      WRITE(XNEW(  I),9800)
1100	   CONTINUE
	   LSTIDX=BEGIDX
	   DISPLAY = .FALSE.
	ENDIF
C
C PROGRAM EXIT.
C
8000    CONTINUE
	RETURN
C
C     ===================== Format Statements ===================
C
9010	FORMAT('Matrix Switch Control Snapshot ')
9020    FORMAT(T41,'Last Configuration Update   ',I2.2,':',I2.2,
     *         ':',I2.2)
9030	FORMAT(T2,'*SUPMSC ',I2,T14,'[',A10,']',T41,
     *         'Configuration status ','[',A15,']')
9040    FORMAT(T2,' MSCSTs ',I2,T14,'[',A16,']',T41,			!V03
     *         'Auto Switching       ','[',A10,']')
9060    FORMAT(T4,'[1]',T13,'[2]',T20,'[3]',T27,'[4]',T35,'[5]')
9070    FORMAT(T3,'Local',T11,'Network',T20,'SAP',T27,'Type',T33,
     *         'Status',T44,'Local',T51,'Network',T59,'SAP',T66,'Type',
     *         T72,'Status')
9080    FORMAT(79('='))
9100 	FORMAT(T4,I3,T12,I3,T20,I3,T24,A7,T33,A7)
9101	FORMAT(T4,I3,T12,I3,T20,I3,T24,A7,T33,A7,T45,I3,T52,I3,T59,I3,
     *	       T63,A7,T72,A7)
9800	FORMAT(80(' '))
9900    FORMAT(T2,'MSC Port Configuration not yet defined.....')
9923    FORMAT('Value error   ')
99232   FORMAT('No local port selected')
99233   FORMAT('No network port assigned for local port ',I3)
99234   FORMAT('Invalid network port specified - ',I3)
99235   FORMAT('Local port ',I3,' Already assigned to net port ',I3)
99236   FORMAT('Invalid local port specified - ',I3)
99237   FORMAT('Local Port changed from ',I3,' to ',I3,
     *         ', Net Port - ',I3)
99238   FORMAT('No Station(s) currently defined for Net Port ',I3)
	END
