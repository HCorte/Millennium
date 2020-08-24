C
C SUBROUTINE CONSNP
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]CONSNP.FOV                                   $
C  $Date::   17 Apr 1996 12:42:00                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - vis_consnp.for **
C
C CONSNP.FOR
C
C
C X2X Upgrade: 22-FEB-96 wsm Added PRMAGT.DEF, AGTINF.DEF for Finland.
C
C V03 28-SEP-94 GPR ALLOW CON SELECTION BY SSAP
C V02 21-FEB-94 GPR USE I4 FOR CONN TYPE OUTS
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 21-NOV-89 MBK ORIGINAL FOR FINLAND
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
	SUBROUTINE CONSNP(CON,CMDLIN)					!V03
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'					!V03
	INCLUDE 'INCLIB:LANCOM.DEF'
C
	INTEGER*4 CMDLIN(20)						!V03

	INTEGER*4 I, FR2, FR1, FRIND, PNT, DSAP, SSAP, CON
C
	CHARACTER*17 AFRAME(0:MAXFRAME)
	CHARACTER*10 ACONN(0:6)
C
	DATA AFRAME /'invalid         ',
     *	             'find request    ',
     *	             'find confirm    ',
     *	             'connect request ',
     *	             'connect confirm ',
     *	             'disconn request ',
     *	             'disconn confirm ',
     *	             'clear           ',
     *	             'data            ',
     *	             'poll request    ',
     *	             'poll confirm    '/
C
	DATA ACONN /'   illegal',
     *	            '    closed',
     *	            'find pennd',
     *	            'conn pennd',
     *	            '      open',
     *	            '      idle',
     *	            'disc pennd'/
C
C	***** Start V03 Changes *****
C
	INTEGER*4   NUM_CMDS			    !Number of valid commands
	PARAMETER   (NUM_CMDS=2)
	REAL*8	    CMDOPT(NUM_CMDS)		    !The commands
	DATA CMDOPT /'SSAP    ','        '/
	INTEGER*4   POS				    !Position of command
	INTEGER*4   KEYNUM			    !Key number for command
	INTEGER*4   VALUE			    !Value for command
C
C	***** End V03 Changes *****
C
	IF(LANGO.NE.LANPROUP) THEN
	   WRITE(CLIN23,990)
990	   FORMAT('Network not initialized yet ... please wait ')
	   RETURN
	ENDIF
C
        POS = 1
C
C	***** Start V03 Changes *****
C
C 	Check for input command line.
C
        CALL KEY(CMDLIN, CMDOPT, NUM_CMDS, POS, KEYNUM)
        IF (KEYNUM .EQ. 0) GOTO 30
C
        CALL NUMB(CMDLIN, POS, VALUE)                   ! GET VALUE
        IF (VALUE .LT. 0) THEN                          ! VALUE ERROR
          WRITE(CLIN23, 9030)
9030	  FORMAT('Value error  ')
          GOTO 9999
        ENDIF
C
        GOTO (10, 20) KEYNUM
C
C	Get the Connection number based on the SSAP
C
10	CONTINUE
	IF((VALUE.LE.0).OR.(VALUE.GT.MAXSAP)) THEN
	  CON=1
	ELSE
	  CON=CONNECTION(X2X_GAME_SAP,VALUE)
	ENDIF
        GOTO 30
C
C 	Next command option goes here
C
20	CONTINUE
	GOTO 30
C
30	CONTINUE
C
C	***** End V03 Changes *****
C
	IF(CON.LE.0.OR.CON.GT.MAXCON) THEN
	  CON=1
	ENDIF
C
	WRITE(CLIN1,901) CON,LANTIMER
901	FORMAT('Connection snapshot ',I4.4,' at ',I9)			! V02
C
	SSAP=0
	DSAP=0
	DO 100 SSAP=1,MAXSAP
	DO 200 DSAP=1,MAXSAP
	IF(CONNECTION(SSAP,DSAP).EQ.CON) GOTO 300
200	CONTINUE
100	CONTINUE
C
	WRITE(CLIN23,991) CON
991	FORMAT('Invalid connection number ',I5)
	RETURN
C
300	CONTINUE
	WRITE(CLIN2,900)
900	FORMAT(80(' '))
C
	WRITE(CLIN3,903) CON,SSAP,DSAP,LANTIMER
903	FORMAT('Connection.(S-D).:  ',I4,'[',I3,'-',I3,']',7X,
     *	       'Lantimer.........:  ',I10)				! V02
C
	WRITE(CLIN4,904) CURLAN(CON),ACONN(LANCONN(CON))
904	FORMAT('Current lan......:  ',I10,10X,
     *	       'Connection state.:  ',A10)
C
	WRITE(CLIN5,905) LANTARR(CON),LANTOUT(CON)
905	FORMAT('Last arrival time:  ',I10,10X,
     *	       'Time-out sched...:  ',I10)
C
	WRITE(CLIN6,900)
C
	WRITE(CLIN7,907)
907	FORMAT('Frame counters:')
C
	WRITE(CLIN8,900)
	WRITE(CLIN9,909)
909	FORMAT(19X,'inbound ','outbound',23X,'inbound ','outbound')
C
	PNT=10
	IF(MAXFRAME.GT.1) THEN
	   DO 400 FRIND=1,MAXFRAME/2
	   FR1=FRIND*2-1
	   FR2=FRIND*2
         WRITE(XNEW(  PNT),910)AFRAME(FR1),(LANFRAMES(FR1,I,CON),I=1,2),
     *	                    AFRAME(FR2),(LANFRAMES(FR2,I,CON),I=1,2)
910	   FORMAT(A17,': ',1X,2I7,5X,A17,': ',1X,2I7)
	   PNT=PNT+1
400	   CONTINUE
	ENDIF
	IF(MOD(MAXFRAME,2).NE.0) THEN
	   FR1=MAXFRAME
         WRITE(XNEW(  PNT),911)AFRAME(FR1),(LANFRAMES(FR1,I,CON),I=1,2)
911	   FORMAT(A17,': ',1X,2I7)
	   PNT=PNT+1
	ENDIF
C
	WRITE(XNEW(  PNT),900)
	PNT=PNT+1
C
9999	CONTINUE							!V03
	RETURN
	END
