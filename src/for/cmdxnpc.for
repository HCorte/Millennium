C
C SUBROUTINE CMDXNPC
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]CMDXNPC.FOV                                  $
C  $Date::   17 Apr 1996 12:39:46                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - cmdxsub.for;1 **
C
C
C =============================================================
C CMDXNPC
C
C This subroutine loads the network port information into
C common.
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
	SUBROUTINE CMDXNPC(FIELD,ALLREC,ADDFLG)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XNPC.DEF'
C
	INTEGER*4   FIELD           !Field number
	INTEGER*4   ALLREC(128)      !Record buffer
	INTEGER*4   ADDFLG          !New network port
	INTEGER*4   PORT            !Network port number
	INTEGER*4   HNTPRT, I
C
C STORE THE INFORMATION INTO COMMON.
C
	CALL FASTMOV(ALLREC,X2XNPC_REC,64)
	PORT=X2XNPC_PORT
        IF(PORT.LE.0 .OR. PORT.GT.X2X_NETWORK_PORTS) GOTO 8000
        IF(FIELD.EQ.2)  THEN                              
          X2XPN_FAST(PORT)   = CHAR(X2XNPC_FAST)          
        ELSEIF(FIELD.EQ.3)  THEN                          
          X2XPN_DDIS(PORT)   = X2XNPC_DDIS                
        ELSEIF(FIELD.EQ.4)  THEN                          
          X2XPN_RETCNT(PORT) = X2XNPC_RETCNT              
        ELSEIF(FIELD.EQ.5)  THEN                          
          X2XPN_RETTIM(PORT) = X2XNPC_RETTIM              
        ELSEIF(FIELD.EQ.6)  THEN                          
          X2XPN_ADDLEN(PORT) = X2XNPC_ADDLEN              
	ELSEIF(FIELD.EQ.7)  THEN
		X2XPN_ADRESS(1,PORT)=X2XNPC_ADDRES(1)
		X2XPN_ADRESS(2,PORT)=X2XNPC_ADDRES(2)
	ELSEIF(FIELD.EQ.9)  THEN
		X2XPN_OUTCALL(PORT)=X2XNPC_ASSIGN
	ELSEIF(FIELD.EQ.10) THEN
		X2XPN_CAPACITY(PORT)=X2XNPC_CAPACITY
	ELSEIF(FIELD.EQ.11) THEN
		X2XPN_STATE(PORT)=X2XNPC_STATE
	ELSEif(FIELD.EQ.14)  THEN				
		X2XPN_REVCHRG(PORT)  = CHAR(X2XNPC_REVCHRG)	
	ELSEIF(FIELD.EQ.15) THEN
		X2XPN_TYPE(PORT)=X2XNPC_TYPE
	ELSEIF(FIELD.EQ.16)  THEN
		X2XPN_NUMPVC(PORT)=X2XNPC_NUMPVC
C        endif
	ELSEIF(FIELD.EQ.12) THEN
	  X2XPN_HUNT_ADR(1,PORT)=X2XNPC_HUNTADR(1)
	  X2XPN_HUNT_ADR(2,PORT)=X2XNPC_HUNTADR(2)
C
C SEARCH THROUGH THE HUNT ADDRESS TO SEE IF WE HAVE A
C NEW ONE.
C
	  DO 200 I=1,X2X_MAX_HUNT
	    HNTPRT=X2XPN_HUNT(I)
	    IF(HNTPRT.EQ.0) GOTO 200
	    IF(X2XNPC_HUNTADR(1).EQ.X2XPN_ADRESS(1,HNTPRT).AND.
     *	       X2XNPC_HUNTADR(2).EQ.X2XPN_ADRESS(2,HNTPRT))
     *	         GOTO 210
	    IF(X2XPN_HUNT(I).EQ.0) GOTO 205
200	  CONTINUE
C
C HUNT ADDRESS IS A NEW ONE, SO STORE IT.
C
205	  CONTINUE
	  IF(I.LE.X2X_MAX_HUNT) THEN
	    X2XPN_HUNT(I)=PORT
	  ENDIF
210	  CONTINUE
	ENDIF
C
C CLEAR OUT STATION CHECKSUMS.
C
        DO 300 I=1,X2X_STATIONS
          X2XS_CALL_CHKSUM(I)=0
300     CONTINUE
C
8000    CONTINUE
	RETURN
	END
