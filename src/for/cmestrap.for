C
C SUBROUTINE CMESTRAP
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]CMESTRAP.FOV                                 $
C  $Date::   17 Apr 1996 12:40:10                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C
C ** Source - cmestrap.for **
C
C CMESTRAP.FOR
C
C V04 24-JAN-2011 RXK IF command split
C V03 01-AUG-2000 UXN TYPE* replaced with OPSTXT()
C V02 07-JAN-1991 KWP INITIAL VAX VERSION
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C V01 16-DEC-1988 MK   ORIGINAL RELEASE FOR FINLAND
C
C
C THIS SUBROUTINE WILL INTERCEPT A COMMAND FROM A CONSOLE
C      DECODE AND EXECUTE IT
C
C     OS ENDS MESSAGE WITH 0D
C
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
	SUBROUTINE CMESTRAP
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:LANCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
	INCLUDE 'INCLIB:CTLCOM.DEF'
	INCLUDE 'INCLIB:CTLEVN.DEF'
C
	INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
C
	INTEGER*4  NPAR
	PARAMETER (NPAR=4)
C
	INTEGER*4 MESS(18),LMESS(18)
	INTEGER*4 DUM3, DUM2, DUM1, MYSAP, BUF, BACK, SAP, STATUS, DIG
	INTEGER*4 NUM, CMD, PAR, PNT, OFF, K, FUNCOD
	CHARACTER*64 CH
	CHARACTER*1 C1(64)
	LOGICAL NUMBER
C
	RECORD /CT_IOSSTRUCT/ LOCAL_IOSB
C
	CHARACTER*80 MNEM(NPAR)
	INTEGER*4 CMDL
	INTEGER*4 CMDS(NPAR)
C
	EQUIVALENCE(LMESS(1),CH,C1)
C
	CHARACTER*1 SPACE/' '/
	CHARACTER*1 CR/Z0D/
C
	DATA MNEM(1)(1:1)    /Z01/
	DATA MNEM(1)(2:9)    /'ACTIVATE'/
	DATA MNEM(1)(10:10)  /Z02/
	DATA MNEM(1)(11:20)  /'DEACTIVATE'/
	DATA MNEM(1)(21:21)  /Z02/
	DATA MNEM(1)(22:28)  /'DISPLAY'/
	DATA MNEM(1)(29:29)  /Z02/
	DATA MNEM(1)(30:32)  /'SET'/
	DATA MNEM(1)(33:33)  /Z04/
	DATA MNEM(1)(34:37)  /'KILL'/
	DATA MNEM(1)(38:38)  /Z02/
	DATA MNEM(1)(39:42)  /'HELP'/
	DATA MNEM(1)(43:43)  /Z02/
	DATA MNEM(1)(44:48)  /'RESET'/
	DATA MNEM(1)(49:49)  /Z00/
	DATA MNEM(1)(50:50)  /Z00/
C
	DATA MNEM(2)(1:1)    /Z03/
	DATA MNEM(2)(2:7)    /'ENABLE'/
	DATA MNEM(2)(8:8)    /Z05/
	DATA MNEM(2)(9:14)   /'STATUS'/
	DATA MNEM(2)(15:15)  /Z02/
	DATA MNEM(2)(16:18)  /'ALL'/
	DATA MNEM(2)(19:19)  /Z02/
	DATA MNEM(2)(20:23)  /'TEST'/
	DATA MNEM(2)(24:24)  /Z00/
	DATA MNEM(2)(25:25)  /Z00/
C
        DATA MNEM(3)(1:1)    /Z01/
        DATA MNEM(3)(2:2)    /'A'/
        DATA MNEM(3)(3:3)    /Z01/
        DATA MNEM(3)(4:4)    /'B'/
        DATA MNEM(3)(5:5)    /Z01/
        DATA MNEM(3)(6:6)    /'C'/
        DATA MNEM(3)(7:7)    /Z01/
        DATA MNEM(3)(8:8)    /'D'/
        DATA MNEM(3)(9:9)    /Z01/
        DATA MNEM(3)(10:10)  /'E'/
C
        DATA MNEM(3)(11:11)  /Z02/
        DATA MNEM(3)(12:18)  /'PRIMARY'/
        DATA MNEM(3)(19:19)  /Z02/
        DATA MNEM(3)(20:25)  /'BACKUP'/
        DATA MNEM(3)(26:26)  /Z00/
        DATA MNEM(3)(27:27)  /Z00/
C
	DATA MNEM(4)(1:1)    /Z00/
	DATA MNEM(4)(2:2)    /Z00/
C
	DATA CMDL /8/ !MAX NUMBER OF KEYWORDS +1 (ERROR)
C
C
C READ THE MESSAGE FROM THE MAILBOX.
C
        FUNCOD=IO$_READVBLK
        STATUS=SYS$QIOW(,%VAL(CT_MESCHANNEL),%VAL(FUNCOD),
     *                  LOCAL_IOSB,,,MESS,%VAL(64),,,,)
        IF(.NOT.STATUS) THEN
D         TYPE *,'ERROR READING MESSAGE '
          CALL LIB$SIGNAL(%VAL(STATUS))
        ENDIF
C
	DO 5 K=1,18
	  LMESS(K)=MESS(K)
5	CONTINUE
C
    	IF(LANGO.LT.LANPROUP) THEN
    	   CALL OPSTXT('SYSTEM NOT READY')
    	   GOTO 8000
    	ENDIF
C
	DO 6 OFF=1,NPAR
	  CMDS(OFF)=0
6	CONTINUE
C
	NUMBER=.FALSE.
	PNT=1
	DO 10 PAR=1,NPAR
	IF(NUMBER) GOTO 15
	CALL SCAN(CH,PNT,MNEM(PAR),CMD)
11      CONTINUE
        IF(PNT.LT.64) THEN
          IF(C1(PNT).NE.SPACE) THEN
            PNT=PNT+1
            GOTO 11
          ENDIF
        ENDIF
C
	CMDS(PAR)=CMD
	GOTO (10,10,10,10,10,10,10,10,
     *	      10,10,10,10,20,10,10,10,    !20 MEANS NEXT PAR IS A NUMBER
     *	      10,20,20,20,20,20,20,10,    !20 MEANS NEXT PAR IS A NUMBER
     *	      10,10,10,10,10,10,10,10),
     *	                       (PAR-1)*CMDL+CMD+1
	GOTO 300
15	CONTINUE
C
C     NUMBER FOLLOWS
C     ANALIZE AND PUT IN CMDS(PAR)
C
	DO 100 K=PNT,64
	IF(C1(K).EQ.CR) GOTO 300
	IF(C1(K).NE.SPACE) GOTO 200
100	CONTINUE
	GOTO 300
200	CONTINUE
	PNT=K
	NUM=0
	DIG=0
	DO 110 K=PNT,64
	IF(C1(K).EQ.SPACE.OR.C1(K).EQ.CR) GOTO 210
	CALL ASCBIN(LMESS(1),K,1,DIG,STATUS)
	IF(STATUS.NE.0) GOTO 300
	NUM=NUM*10+DIG
110	CONTINUE
	GOTO 300
210	CONTINUE
	IF(NUM.LT.0) GOTO 300
	CMDS(PAR)=NUM
	PNT=K
	NUMBER=.FALSE.
	GOTO 10
C
20	CONTINUE
	NUMBER=.TRUE.
10	CONTINUE
C
	IF(CTLTEST.NE.0) THEN
	  TYPE*,'**** CMESTRAP ****[',CMDS,']'
	  TYPE*,'STRING [',CH,']'
	ENDIF
C
C CMDS IS ALL SET NOW
C ANALIZE AND EXECUTE
C
	IF(CMDS(1).EQ.1)   THEN !ACTIVATE
C
	   CTLSTATUS=CTLACTIVE
C
	ELSEIF(CMDS(1).EQ.2) THEN !DEACTIVATE
C
	   CTLSTATUS=CTLINACTIVE
C
	ELSEIF(CMDS(1).EQ.3) THEN !DISPLAY
C
	   IF(CMDS(2).EQ.3.OR.CMDS(2).EQ.2) THEN !ALL
C
	   TYPE*,'CTLSAPSYS       ',CTLSAPSYS
	   TYPE*,'CTLSYSSAP       ',CTLSYSSAP
	   TYPE*,'CTLSAPSTA       ',CTLSAPSTA
	   TYPE*,'CTLSAPTOUT      ',CTLSAPTOUT
	   TYPE*,'CTLSAPENA       ',CTLSAPENA
	   TYPE*,'CTLRSEQ         ',CTLRSEQ
	   TYPE*,'CTLSSEQ         ',CTLSSEQ
	   TYPE*,'CTLSTATUS       ',CTLSTATUS
	   TYPE*,'CTLTEST         ',CTLTEST
C
	   ELSE
	      GOTO 300
	   ENDIF
C
	ELSEIF(CMDS(1).EQ.4) THEN !SET
C
	   IF(CMDS(2).EQ.4) THEN
C
	      CTLTEST=CMDS(3)
	      TYPE*,'CTLTEST         ',CTLTEST
C
	   ELSEIF(CMDS(2).EQ.1) THEN
C
	      IF(CMDS(4).EQ.0.OR.CMDS(4).EQ.1) THEN
	         IF(CMDS(3).GE.1.AND.CMDS(3).LE.NETSYS) THEN
C
	            SAP=CTLSAPSYS(CMDS(3))
	            IF(SAP.GT.0) CTLSAPENA(SAP)=CMDS(4)
C
	         ELSEIF(CMDS(3).EQ.6) THEN
C
	            CTLSAPENA(X2X_GAME_SAP)=CMDS(4)
C
	         ELSEIF(CMDS(3).EQ.7) THEN
C
	            BACK=NETBACKUP(WAYINP)
C
	            IF(BACK.GT.0) THEN
	               SAP=CTLSAPSYS(BACK)
C
	               IF(SAP.GT.0) THEN
	                  CTLSAPENA(SAP)=CMDS(4)
	               ENDIF
	            ENDIF
C
	         ELSE
	            GOTO 300
	         ENDIF
	      ELSE
	         GOTO 300
	      ENDIF
	   ELSE
	      GOTO 300
	   ENDIF
	ELSEIF(CMDS(1).EQ.5.OR.CMDS(1).EQ.7) THEN !STOP OR RESET
C
C CLOSE YOUR OWN SAP
C
	   CALL LANGETX(BUF,STATUS)
	   IF(STATUS.NE.2) THEN
C
	      MYSAP=CTLSAPSYS(NODEID)
	      LANBUF(LANBTYP,BUF)   =LTYPCMD
	      LANBUF(LANDATAF,BUF)  =CCLOSE
	      LANBUF(LANDATAF+1,BUF)=CCOMMAND
	      LANBUF(LANDATAF+2,BUF)=MYSAP
	      LANBUF(LANDATAF+3,BUF)=CTLAPLQUE
C
	      DO 250 SAP=1,MAXSAP
	      CTLSAPTOUT(SAP) =0
	      CTLSAPSTA(SAP)  =CTLSTADOWN
	      CTLSSEQ(SAP)    =0
	      CTLRSEQ(SAP)    =0
250	      CONTINUE
C
	      CTLSTATUS=CTLINACTIVE
C
	      CALL SNDLAN(DUM1,DUM2,BUF,DUM3)
	   ELSE
	      CALL OPSTXT('**** BUFFER ALLOCATION ERROR ****')
	   ENDIF
C
	   IF(CMDS(1).EQ.5) THEN !STOP
C
	      CALL OPSTXT('OK')
	      CALL GSTOP(GEXIT_SUCCESS)
C
	   ENDIF
C
	ELSEIF(CMDS(1).EQ.6) THEN ! HELP
C
	   TYPE*,' '
	   TYPE*,'Activate'
	   TYPE*,'DEactivate'
	   TYPE*,'DIsplay ALl/STATUs'
	   TYPE*,'SEt TEst number'
	   TYPE*,'SEt ENAble system_id/PRimary/BAckup number'
	   TYPE*,'REset'
	   TYPE*,'KILL'
	   TYPE*,'HElp'
	   TYPE*,' '
C
 
	ELSE
	   GOTO 300
	ENDIF
C
	GOTO 8000
C
C
300	CONTINUE
	CALL OPSTXT('INVALID COMMAND ')
C
8000	CONTINUE
	CALL CT_START_MESS
C
	RETURN
	END
