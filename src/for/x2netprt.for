C
C SUBROUTINE X2NETPRT
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2NETPRT.FOV                                 $
C  $Date::   17 Apr 1996 16:25:10                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2netprt.for;1 **
C
C X2NETPRT.FOR
C
C V04 05-DEC-94 SCD Integrate UK changes into X2X Baseline
C V02 24-OCT-94 XXX CLEAN UP THIS CRAZY LOGIC, USE NET PORT
C		    DISTRIBUTION LOGIC FROM SPAIN WHICH WORKS?,
C		    AND IMPLEMENT OPTIMIZE SITES
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
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
C     CALL X2NETPRT(STN,PRTCNT,NETPORT)
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
	SUBROUTINE X2NETPRT(STN,PRTCNT,NETPORT)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2NETCOM.DEF'
C
	INTEGER*4   STN                             !Station number
	INTEGER*4   PRTCNT                          !Number of ports assigned
	INTEGER*4   NETPORT(X2X_MAXPRT_ASSIGN)      !Ports assigned
	INTEGER*4   PORT                            !Work variable
	INTEGER*4   TOPPRT                          !Last port slot assigned
        INTEGER*4   NDX_DEFAULT(X2X_MAX_SITES)	    !Work variable	    !V02
        INTEGER*4   SITE_PORT_INDEX(X2X_MAX_SITES)  !Index for each site    !V02
	INTEGER*4   SITE, J, I
	INTEGER*4   NEXT_CLASS_PORT, CLASS, LAST_CLASS_PORT
	INTEGER*4   SKIP_CLASS
C
C INITIALIZE LOCAL VARIABLES.
C
	PRTCNT=0
	CALL FASTSET(0,NETPORT,X2X_MAXPRT_ASSIGN)
C
C ==================== SPECIFIC PORT ASSIGNMENT ====================
C
	CLASS=X2XS_STNCLS(STN)
	TOPPRT=0
	LAST_CLASS_PORT=1
	SKIP_CLASS=0
	DO 400 I=1,X2XS_MAXNET
	  PORT = X2XS_NETPORT(I,STN)	
C
C	  ***** Start V02 changes *****
C
	  IF (PORT.NE.0) THEN
	     SKIP_CLASS=-1
	  ENDIF
	  IF(PORT.EQ.0) THEN
	    IF (SKIP_CLASS.EQ.0) THEN
	       IF (TOPPRT.GE.X2X_MAXPRT_ASSIGN) GOTO 400
	       NEXT_CLASS_PORT=LAST_CLASS_PORT
	       IF (NEXT_CLASS_PORT.LE.X2X_MAXPRT_ASSIGN) THEN
	 	  IF (X2XC_NETPORT(NEXT_CLASS_PORT,CLASS).EQ.0) GOTO 400
		  PORT = X2XC_NETPORT(NEXT_CLASS_PORT,CLASS)
		  LAST_CLASS_PORT=NEXT_CLASS_PORT+1
	       ENDIF
	    ELSE
	       GOTO 400
	    ENDIF
	  ENDIF
C
C	  ***** End V02 changes *****
C
C SEARCH THE ALREADY ASSIGNED PORTS, AND IF IT IS THE SAME,
C SKIP IT.
C
	  DO 300 J=1,TOPPRT
	    IF(PORT.EQ.NETPORT(J)) GOTO 400
300	  CONTINUE
C
C IF THE PORT EXISTS, ASSIGN IT FOR OUTCALL.
C
	  IF(PORT.NE.0 .AND.
     *	    (X2XPN_ADRESS(1,PORT).NE.0 .OR.
     *	     X2XPN_ADRESS(2,PORT).NE.0)) THEN
	    TOPPRT=TOPPRT+1
	    NETPORT(TOPPRT)=-PORT
	  ENDIF
400	CONTINUE
C
C ==================== DEFAULT PORT ASSIGNMENT =====================
C
	PRTCNT=TOPPRT
C
C	***** Start V02 changes *****
C
C	Initialize the port indexes for each site
C
	DO I=1,X2X_MAX_SITES
	  SITE_PORT_INDEX(I)=0
	  NDX_DEFAULT(I)=1
	ENDDO
C
	DO 500 I=TOPPRT+1,X2X_MAXPRT_ASSIGN
C
C	 Use the Network port from the Station class
C	 if defined
C
	 IF(X2XC_NETPORT(I,CLASS).NE.0) THEN
	  NETPORT(I)=X2XC_NETPORT(I,CLASS)
	  PRTCNT=PRTCNT+1
C
C	 Otherwise get from all sets of ports
C
	 ELSE
	  SITE=X2XC_SITE(I,X2XS_STNCLS(STN))
C
C	  Set the site to the modulus of max sites if
C	  the optimize flag is set
C
	  IF((SITE.NE.0).AND.(X2XC_OPTIMIZE_SITES(CLASS).EQ.
     *	    X2XC_ENABLE_OPTIMIZE_SITES)) THEN
	    SITE=MOD(STN+SITE,X2X_MAX_SITES)+1
	  ENDIF
C
	  IF(SITE.NE.0) THEN
C
C	    Increment the port index for the site
C
	    SITE_PORT_INDEX(SITE)=SITE_PORT_INDEX(SITE)+1
C
C	    Load the Network port from all sets of ports 
C	    if one is defined for this site's port index
C
	    IF(ALL_SETS_OF_PORTS(SITE,STN,SITE_PORT_INDEX(SITE)).NE.0) THEN
	      NETPORT(I)=-ALL_SETS_OF_PORTS(SITE,STN,SITE_PORT_INDEX(SITE))
	      PRTCNT=PRTCNT+1
C
C	    Otherwise load the Network port from all sets of ports 
C	    using the default port index
C
	    ELSEIF (ALL_SETS_OF_PORTS(SITE,STN,
     *		    SITE_PORT_INDEX(SITE)).EQ.0) THEN
	      IF (ALL_SETS_OF_PORTS(SITE,STN,NDX_DEFAULT(SITE)).EQ.0) THEN
		NDX_DEFAULT(SITE)=1
		NETPORT(I)=-ALL_SETS_OF_PORTS(SITE,STN,NDX_DEFAULT(SITE))
	      ELSE
		NETPORT(I)=-ALL_SETS_OF_PORTS(SITE,STN,NDX_DEFAULT(SITE))
		NDX_DEFAULT(SITE)=NDX_DEFAULT(SITE)+1
	      ENDIF
	      PRTCNT=PRTCNT+1
	    ENDIF
	   ENDIF
	  ENDIF
C
C	  ***** End V02 changes *****
C
500	CONTINUE
C
	RETURN
	END
