C
C *** SUBROUTINE FLWSNP ***
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]FLWSNP.FOV                                   $
C  $Date::   17 Apr 1996 13:11:40                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C *** Pre-Baseline Source - dcnsno.for ***
C
C              31-MAY-2000 PXO Subroutine name from FLOWSNP -> FLWSNP
C X2X Upgrade: 22-FEB-96 wsm Added PRMAGT.DEF, AGTINF.DEF for Finland.
C
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
C Copyright 1993 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
C
	SUBROUTINE FLWSNP(CMDLIN)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
 	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
        INCLUDE 'INCLIB:DN_LINK.DEF'			! DECNET STRUCTURES
        INCLUDE 'INCLIB:DN_BLOCK.DEF'			! DECNET DATA BLOCKS
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
C
C LOCAL DECLARATIONS
C
        REAL*8		CMDOPT(2) /'DUMP    ', '        '/
C
	INTEGER*4	BUFPTR,
     *			CMDLIN(20),
     *			CNT_QUE(4, 2, NETSYS),
     *			CURRENT_OFFSET,
     *			HIGH_CURRENT_DELAY(NETSYS),
     *			HIGH_DELAY(NETSYS),
     *			I4_SPACES /'    '/,
     *			KEYNUM,
     *			NODE,
     *			NUM_READS(NETSYS),
     *			NUM_WRITES(NETSYS),
     *			OFF,
     *			POS,
     *			STATUS,
     *			TIME_STARTED(NETSYS),
     *			VALUE
C
	CHARACTER*9	RELAT(-2:2)
     *			/'dead    ', 'dead    ',
     *			 'not actv',
     *			 'secon-ry', 'primary '/
C
	CHARACTER*8	SNP_DCN_STATE(0:7)
     *			/'initial ', 'down    ',
     *			 'connect ', 'confirm ',
     *			 'disconct', 'abort   ',
     *			 'fail    ', 'running '/,
C
C
     *			SNP_SYSTEM_STATES(0:2)
     *			/'invalid ', 'normal  ', 'recovery'/,
C
     *			SYSTEM_NAME(6)
     *			/'   A    ', '   B    ', '   C    ',
     *			 '   D    ', '   E    ', '   F    '/
C
C EXTERNAL DECLARATIONS
C
	INTEGER*4	QUECNT
	EXTERNAL	QUECNT
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	CALL SPACES
	POS = 1
C
C CHECK FOR INPUT COMMAND LINE.
C
	CALL KEY(CMDLIN, CMDOPT, 2, POS, KEYNUM)
        IF (KEYNUM .EQ. 0) GOTO 200
C
	CALL NUMB(CMDLIN, POS, VALUE)			! GET VALUE
	IF (VALUE .LT. 0) THEN				! VALUE ERROR
	  WRITE(CLIN23, 9030)
	  GOTO 9999
	ENDIF
C
	GOTO (101, 102) KEYNUM
C
C DUMP COMMONS TO FILE
C
101	CONTINUE
	CALL NET_DUMP
	WRITE(CLIN23, 9040)
	GOTO 200
C
C NEXT COMMAND OPTION GOES HERE
C
102	CONTINUE
	GOTO 200
C
C INITIALIZE & DISPLAY SCREEN
C
200	CONTINUE
	CALL FASTSET(I4_SPACES, NEW(1, 1), 22 * 20)
C
	WRITE (CLIN3, 9000) 'System ',
     *        (SYSTEM_NAME(OFF), OFF = 1, NETSYS)

	WRITE (CLIN4, 9000) 'Conn ',
     *        (RELAT(NETSTAT(OFF, 1)), OFF = 1, NETSYS)
C
	WRITE (CLIN5, 9000) 'Network state ',
     *        (SNP_DCN_STATE(DN_LINK(OFF).STATE), OFF = 1, NETSYS)
C
	WRITE (CLIN6, 9000) 'System status ',
     *        (SNP_SYSTEM_STATES(NETMODE(OFF, 1)), OFF = 1, NETSYS)
C
	WRITE (CLIN7, 9010) 'Time since last I/O ',
     *        ((NETTIMER-NETTIM(OFF, 1)) / 1000, OFF = 1, NETSYS)
C
C FIND NUMBER OF OUTSTANDING WRITES
C
	DO 300 NODE = 1, NETSYS
	  CALL QIMAGE(NET_IOSTARTED(1, 1, NODE), CNT_QUE(1, 1, NODE), 3)
	  CALL QIMAGE(NET_IOSTARTED(1, 2, NODE), CNT_QUE(1, 2, NODE), 3)
300	CONTINUE
C
	WRITE (CLIN8, 9010) 'I/O started ',
     *        (CNT_QUE(1, 2, OFF), OFF = 1, NETSYS)
C
	WRITE (CLIN9, 9010) 'I/O waiting ',
     *        (CNT_QUE(1, 1, OFF), OFF = 1, NETSYS)
C
C DISPLAY TIME SINCE STARTED
C
	DO 400 NODE = 1, NETSYS
	  CALL LISTTOP(BUFPTR, NET_IOSTARTED(1, 2, NODE), STATUS)
	  IF (STATUS .EQ. GLIST_STAT_EMPTY)
     *      CALL LISTTOP(BUFPTR, NET_IOSTARTED(1, 1, NODE), STATUS)
	  IF (STATUS .EQ. GLIST_STAT_EMPTY) THEN
	    TIME_STARTED(NODE) = 0
	  ELSE
	    CALL NET_DIFTIM(DN_BUFFER(BUFPTR), TIME_STARTED(NODE))
	  ENDIF
400	CONTINUE
C
	WRITE (CLIN10, 9010) 'time started ',
     *        (TIME_STARTED(OFF), OFF = 1, NETSYS)
C
C FIND LONGEST WRITE
C
	CURRENT_OFFSET = NET_CURRENT_XFER_OFFSET + 1
	IF (CURRENT_OFFSET .GT. 2) CURRENT_OFFSET = 1
C
	DO 600 NODE = 1, NETSYS
	  HIGH_CURRENT_DELAY(NODE) = 0
	  HIGH_DELAY(NODE)         = 0
	  DO 500 OFF = 0, NET_XFER_TIME_MAX
	    IF (NET_XFER_TIME(OFF, NODE) .NE.
     *          NET_XFER_LAST_DELAYS(OFF, NODE, CURRENT_OFFSET)) 
     *        HIGH_CURRENT_DELAY(NODE) = OFF
	    IF (NET_XFER_TIME(OFF, NODE) .NE. 0) HIGH_DELAY(NODE) = OFF
500	  CONTINUE
600	CONTINUE
C	
	WRITE (CLIN11, 9010) 'Longest write (last) ',
     *        (HIGH_CURRENT_DELAY(OFF), OFF = 1, NETSYS)
C
	WRITE (CLIN12, 9010) 'Longest write ',
     *	      (HIGH_DELAY(OFF), OFF = 1, NETSYS)
C
	DO 700 NODE = 1, NETSYS
	  NUM_WRITES(NODE) = DN_LINK(NODE).MSGXMT
     *                     - NET_XFER_MSGS_CURRENT(CURRENT_OFFSET, NODE)
700	CONTINUE
C
	WRITE (CLIN13, 9010) 'no of writes  (last)',
     *        (NUM_WRITES(OFF), OFF = 1, NETSYS)	
C
	WRITE (CLIN14, 9010) 'no of writes ',
     *        (DN_LINK(OFF).MSGXMT, OFF = 1, NETSYS)	
C
	DO 800 NODE = 1, NETSYS
	  NUM_READS(NODE) = DN_LINK(NODE).MSGRCV
     *                    - NET_RECV_MSGS_CURRENT(CURRENT_OFFSET, NODE)
800     CONTINUE
C
        WRITE (CLIN15, 9010) 'no of reads  (last)',
     *        (NUM_READS(OFF), OFF = 1, NETSYS)
C
        WRITE (CLIN16, 9010) 'no of reads ',
     *        (DN_LINK(OFF).MSGRCV, OFF = 1, NETSYS)
C
	WRITE (CLIN17, 9010) 'Last error ',
     *        (DN_LINK(OFF).LAST_ERROR, OFF = 1, NETSYS)
C
	WRITE (CLIN18, 9010) 'Free recovery buffs',
     *        (QUECNT(NETFREE_RECOVERY(1, OFF)), OFF = 1, NETSYS)
C
	WRITE (CLIN19, 9010) 'High serial ',
     *        (NETSER(OFF, 1), OFF = 1, NETSYS)
C
	WRITE (CLIN20, 9010) 'Response time long',
     *        (NET_TIMES_RESPONSE_LONG(OFF), OFF = 1, NETSYS)
C
	WRITE (CLIN21, 9010) 'SNDIOCHK ',
     *        (SNDIOCHK(OFF), OFF = 1, NETSYS)
C
	WRITE (CLIN22, 9020) QUECNT(NETFREE(1, 1)), QUECNT(DCN_FREE),
     *        QUECNT(DCN_QUE), QUECNT(DCN_NETQUE), 
     *	      NXTSER, P(CMDFRZ), NETCMDFRZ
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C FORMAT STATEMENTS.
C
9000	FORMAT(A, T21, A, T31, A, T41, A, T51, A, T61, A, T71, A)
9010	FORMAT(A, T21, I9, T31, I9, T41, I9, T51, I9, T61, I9, T71, I9)
9020	FORMAT('Netfree ', I3, ' Dcnfree ', I3, ' NET ', I3,
     *         ' DCN ', I3, ' NXTSER ', I8, ' CMDFRZ ', I4, ' / ', I4)
9030    FORMAT('Value error  ')
9040    FORMAT('Dump complete')
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C RETURN & END.
C
9999	CONTINUE
C
	RETURN
	END
