C
C SUBROUTINE LTIMTRAP
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]LTIMTRAP.FOV                                 $
C  $Date::   17 Apr 1996 13:57:58                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - ltimtrap.for;1 **
C
C LTIMTRAP.FOR
C
C V02 30-MAR-92 DAS ADDED MUTIPLE TIMERS (PTIMTICK/PREADTIMR)
C V01 30-MAY-89 MBK ORIGINAL RELEASE
C
C TIMER TICK  ROUTINE:
C
C  1. INCREASE LANTIMER BY LANSTEP
C  2. CHECK IF ANY TIMEOUT SCHEDULED FOR NOW IN LANTOUT FOR CONNECTIONS.
C     IF SO RESET TO ZERO AND CALL EVENT ROUTINE WITH A TOUT EVENT AND
C     CURRENT STATE. ALSO CALL THE APPROPRIATE ACTION ROUTINE.
C  3. CHECK CONNECTIONS. IF ANYONE IS IN IDLE MODE POLL THE DSAP.
C  4. CHECK LAST ARRIVAL TABLE. FOR THOSE CONNECTIONS WHICH ARE NOT REFRESHED
C     CALL EVENT AND ACTION ROUTINES. IF CONNECTED TAKE LANDEL1, IF IDLE
C     TAKE LANDEL2 FOR CONSIDERATION. IF NO TRAFIC FOR LANDEL2 TIME GENERATE
C     DISCONNECTION EVENT.
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
	SUBROUTINE LTIMTRAP(PARTIM)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:LANCOM.DEF'
C
	INTEGER*4 BUF   ! IS DUMMY IN THIS ROUTINE
        INTEGER*4 ILAN, ISAP, READTIME
	INTEGER*4 OLDSTATE, XDSAP, XSSAP, CONN, DSAP, SSAP, PARTIM
C
C TEST FOR THE TYPE OF TIMER TRAP:
C CURRENT VALUES ARE:
C     1 - TIMER TICK
C     2 - STATISTICS (NOT IMPLEMETED)
C     3 - READ TIMER (ALL LAN BUFFERS USED - INITIATED ANOTHER READ)
C
	BUF=0
	IF(PARTIM.EQ.PTIMTICK) THEN
	   LANTIMER=LANTIMER+LANSTEP
D	   TYPE*,'**** TIMER TICK ****[',PARTIM,LANTIMER,']'
C
	   DO 100 SSAP=1,MAXSAP-1
              DO 110 DSAP=SSAP+1,MAXSAP
	         CONN=CONNECTION(SSAP,DSAP)
	         IF(LANSAPSTS(SSAP).NE.SAPDOWN) THEN
	            XSSAP=SSAP
	            XDSAP=DSAP
	         ELSE
	            XSSAP=DSAP
	            XDSAP=SSAP
	         ENDIF
C
	        IF(LANTOUT(CONN).NE.0) THEN
	          IF(LANTOUT(CONN).LE.LANTIMER) THEN
D	            OLDSTATE=LANCONN(CONN)
D	            IF(LANTEST.EQ.1) THEN
D                     CALL OPS('**** DLL CONNECTION TIMEOUT ****',
D     *                        CONN,OLDSTATE)
D	            ENDIF
	            CALL LANACT(EVNTOUT,XSSAP,XDSAP,BUF) !ACT ON IT
	          ENDIF
	        ELSE
	          CALL LANACT(EVNTICK,XSSAP,XDSAP,BUF)   !ACT ON IT
	        ENDIF
110	      CONTINUE
100	   CONTINUE
C
	  CALL LN_START_TIME  !START WAITING FOR ANOTHER TIMER TRAP
C
C
C
	ELSEIF(PARTIM.EQ.PTIMSTATS) THEN  !PARTIM = 2            
	   TYPE*,'**** NOT IMPLEMENTED ****'
C
C ALL LAN BUFFERS HAVE BEEN EXHAUSTED. TEST READ STATUS OF EACH LAN/SAP
C AND INITIATE ANOTHER READ. THE TIMER TRAP IS SET BY GOREAD. READTIM CONTAINS
C THE TIME OF THE UNSUCESSFUL READ + LANREAD TIME.  THIS IS TO BE TESTED
C AGAINST THE CURRENT TIME. IF LESS THAN OR EQUAL TO THE CURRENT TIME THEN 
C A READ ON THAT LAN/SAP AND SHOULD BE RETRIED. A SUCESSFUL READ WILL SET 
C THE VALUE TO -1(IN GOREAD). TIME IS IN SECONDS
C
        ELSEIF(PARTIM.EQ.PREADTIMR) THEN  !PARTIM = 3
C
           CALL GETTIM(READTIME)    
           DO 250 ILAN = 1,MAXLAN
              DO 200 ISAP = 1,MAXSAP
                 IF((READTIM(ILAN,ISAP).NE.-1) .AND.
     *              (READTIM(ILAN,ISAP).LE.READTIME)) THEN
                       CALL GOREAD(ILAN,ISAP)
                 ENDIF
200           CONTINUE
250        CONTINUE
C
C ADD ANY ADDITIONAL TIMERS HERE
C
	ELSE
	   TYPE*,'**** ILLEGAL TIMER TRAP **** [ ',PARTIM,' ]'
	ENDIF
C
	RETURN
	END
