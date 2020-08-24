C
C SUBROUTINE X2X32PRT
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2X32PRT.FOV                                 $
C  $Date::   17 Apr 1996 16:41:38                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2x32prt.for;1 **
C
C X2X32PRT.FOR
C
C V02 24-JAN-2011 RXK IF command split
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C THIS ROUTINE WILL ASSIGN NETWORK PORTS FOR DIALUP X.32
C THIS ROUTINE IS BASED ON X2NETPRT.FTN
C
C NOTE: THE ROUTINE X2LODNET HAS BEEN CHANGED TO PROVIDE
C        9 PORTS. THE X32 PORT ASSIGNMENTS WILL BE TAKEN
C        FROM THE LAST TWO ASSIGNED.
C
C This routine will assign network ports to be used when
C attempting a connection to the host system.  NOTE:
C specific network addresses previously configured
C in the Station Configuration file will override any
C default network ports.
C
C NOTE: To provide backward compatability, the assigned ports
C will have a negative value.  This informs X2STCONF to use
C the specific port address, and not the hunt address.
C
C Calling sequence:
C
C     CALL X2X32PRT(STN,PRTCNT,NETPORT)
C
C Input parameters:
C
C     STN         Int*4       Station Number
C
C Output parameters:
C
C     PRTCNT      Int*4                       Number of ports assigned
C     NETPORT     Int*4(X2X_MAXPRT_ASSIGN)    Network ports assigned
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
	SUBROUTINE X2X32PRT(STN,PRTCNT,NETPORT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2NETCOM.DEF'
C
	INTEGER*4   STN                             !Station number
	INTEGER*4   PRTCNT                          !Number of ports assigned
	INTEGER*4   NETPORT(X2X_MAXX32_ASSIGN)      !Ports assigned
	INTEGER*4   PORT                            !Work variable
	INTEGER*4   TOPPRT                          !Last port slot assigned
        INTEGER*4   I, J, SITE
C
C INITIALIZE LOCAL VARIABLES.
C
	PRTCNT=0
	CALL FASTSET(0,NETPORT,X2X_MAXX32_ASSIGN)
C
C ==================== SPECIFIC PORT ASSIGNMENT ====================
C
	TOPPRT=0
	DO 150 I=1,X2XS_MAXX32
	  PORT = X2XS_X32PORT(I,STN)
	  IF(PORT.EQ.0) GOTO 150
C
C SEARCH THE ALREADY ASSIGNED PORTS, AND IF IT IS THE SAME,
C SKIP IT.
C
	  DO 152 J=1,X2X_MAXX32_ASSIGN
	    IF(PORT.EQ.NETPORT(J)) GOTO 150
152	  CONTINUE
C
C IF THE PORT EXISTS, ASSIGN IT FOR OUTCALL.
C
	  IF(PORT.NE.0 .AND.
     *	    (X2XPN_ADRESS(1,PORT).NE.0 .OR.
     *	     X2XPN_ADRESS(2,PORT).NE.0)) THEN
	    TOPPRT=TOPPRT+1
	    NETPORT(TOPPRT)=-PORT
	  ENDIF
150	CONTINUE
C
C ==================== DEFAULT PORT ASSIGNMENT =====================
C
	PRTCNT=TOPPRT
	IF(PRTCNT.EQ.X2XS_MAXX32) RETURN
	DO 100 I=TOPPRT+1,X2X_MAXX32_ASSIGN
	   J = I + X2X_MAXPRT_ASSIGN
	  SITE=X2XC_SITE(J, X2XS_STNCLS(STN) )
	  IF(SITE.NE.0) THEN
            IF(ALL_SETS_OF_PORTS(SITE,STN,J).NE.0) THEN
	      NETPORT(I)=-ALL_SETS_OF_PORTS(SITE,STN,J)
	      PRTCNT=PRTCNT+1
	    ENDIF
	  ENDIF
100	CONTINUE
C
	RETURN
	END
