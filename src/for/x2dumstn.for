C
C SUBROUTINE X2DUMSTN
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2DUMSTN.FOV                                 $
C  $Date::   17 Apr 1996 16:15:54                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C X2X Upgrade: 22-FEB-96 wsm Added AGTINF.DEF for Finland.
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     X2DUMSTN                 ;ASSIGN DUMMY STATION AND TERMINAL
C                              ;NUMBERS TO NEW GVTS
C
C     X2DUMSTN(EVSNNUM,CONN_TYPE,DUMMY_STN_NO,DUMMY_TERM_NO,STATUS)
C              
C     IN:
C        EVSNNUM - EVSN (GVT ID) RECEIVED IN THE UPLINE HELP/
C                  DEFAULT CONFIGURATION REQUEST
C        CONN_TYPE - CONNECTION TYPE.  THERE IS AN ASSUMED 1 TO 1 MAPPING
C                    BETWEEN STATION CLASS AND CONNECTION TYPE.
C     OUT:
C        DUMMY_STN_NO  - DUMMY STATION  NUMBER FOR THE GVT
C        DUMMY_TERM_NO - DUMMY TERMINAL NUMBER FOR THE GVT
C        STATUS  - STATUS
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
	SUBROUTINE X2DUMSTN(EVSNNUM,CONN_TYPE,DUMMY_STN_NO,
     *			    DUMMY_TERM_NO,STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:X2BLDNET.DEF'
C
C	INTEGER*4 X2XS_TERMS(X2X_MAXTERMS,X2X_MAXPORT,X2X_STATIONS)

        INTEGER*4 EVSNNUM(X2X_EVSN_MAXLEN)
	INTEGER*4 CONN_TYPE
	INTEGER*4 DUMMY_TERM_NO, DUMMY_STN_NO, STATUS

	INTEGER*4 DUMMY_CLASS,DUMMY_STN
        INTEGER*4 GVT_TERM_NUM,GVT_PORT_NUM
        PARAMETER (GVT_TERM_NUM = 1)	!ALWAYS ASSUME 1 TERMINAL PER
                                        !DUMMY STATION
	PARAMETER (GVT_PORT_NUM =1)	!ALWAYS ASSUME 1 PORT PER
                                        !DUMMY STATION
	INTEGER*4 OFF,OFF1


	STATUS = -1
C 
C       THERE IS AN ASSUMED 1 TO 1 MAPPING BETWEEN STATION CLASS AND 
C	CONNECTION TYPE.  CURRENTLY, GVTS ARE ONLY ALLOWED ON GTECH
C	DIAL OR X28 PAD.THE ONLY VALID (STATION CLASS,CONNECTION TYPE) 
C	PAIRS ARE (GTECH DIAL,GTECH DIAL) AND (X28GVT, X28 PAD).  IF ANY 
C	OTHER MAPPING IS ADDED IN THE FUTURE, THEN THE FOLLOWING LOGIC 
C	WILL REQUIRE MODIFICATION.

	IF (CONN_TYPE.EQ.X2XSCT_GTECH_DIAL) THEN
            DUMMY_CLASS = CLASS_GVT
	ELSEIF (CONN_TYPE.EQ.X2XSCT_X28PAD) THEN
            DUMMY_CLASS = CLASS_X28
	ELSE				!INVALID CLASS
	   STATUS = -1
	   RETURN
	ENDIF

	DUMMY_STN_NO = -1
	DUMMY_STN = X2XC_DUMMY_START_STN(DUMMY_CLASS)	!1ST DUMMY STATION #
	DO 120 OFF = 1,X2XC_DUMMY_STN_COUNT(DUMMY_CLASS)
	   IF (X2XC_DUMMY_FREE_LIST(OFF,DUMMY_CLASS) .EQ. 
     *	       X2XC_DUMMY_AVAILABLE) THEN
               DUMMY_STN_NO = DUMMY_STN
	       X2XC_DUMMY_FREE_LIST(OFF,DUMMY_CLASS) = X2XC_DUMMY_IN_USE
	       GOTO 130
	   ELSE
	       DUMMY_STN = DUMMY_STN + 1
	   ENDIF
  120	CONTINUE

	IF (DUMMY_STN_NO .EQ. -1) THEN		!NO DUMMY STN # AVAILABLE
	    STATUS = -2
	    RETURN
	ENDIF

 130    CONTINUE

C	NOW THAT WE HAVE ASSIGNED A VALID DUMMY STATION NUMBER TO THE GVT,
C	ASSIGN THE DUMMY TERMINAL NUMBER AND ADD THE NEW EVSN (GVT ID)
C	TO THE LIST OF KNOWN EVSNs
	DUMMY_TERM_NO = X2XS_TERMS(GVT_TERM_NUM,GVT_PORT_NUM,DUMMY_STN_NO)

	DO 140, OFF1=1,X2X_EVSN_MAXLEN
	   X2XS_EVSN(OFF1,DUMMY_STN_NO) = EVSNNUM(OFF1)
140     CONTINUE

	STATUS = 0				!0 = DUMMY STN AND TERM ASSIGNED

	RETURN
	END
