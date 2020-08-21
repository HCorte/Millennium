C
C SUBROUTINE X2SEGSNP
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2GTXDATA.FOV                                $
C  $Date::   17 Apr 1996 16:20:12                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C ** Source - x2gtxsnp.for **
C
C X2GTXDATA.FOR
C
C
C X2X Upgrade: 22-FEB-96 wsm Added PRMAGT.DEF, AGTINF.DEF for Finland.
C
C V01 13-DEC-94 GPR RELEASED FOR UK
C
C This routine displays different X.25 stuff for all ports on all GTX's
C that are up. 
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
        SUBROUTINE X2GTXDATA(BEG_SAP,BEG_PORT,NUM_FORW,NUM_BACK)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:X2XCOM.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:VISCOM.DEF'
        INCLUDE 'INCLIB:X2MAINT.DEF'
C
C
	INTEGER*4 BEG_SAP,NUM_FORW,NUM_BACK
C
	INTEGER*4 MAX_DATA
	INTEGER*4 SAP,PORT
	PARAMETER (MAX_DATA=13)
	INTEGER*4   SAP_INDX,
     *		    PORT_INDX,
     *		    ONL_INDX,
     *		    DISC_INDX,
     *		    NDPR_INDX,
     *		    NDPS_INDX,
     *		    DISC00_INDX,
     *		    DISC05_INDX,
     *		    DISC09_INDX,
     *		    DISCOT_INDX,
     *		    MVC_INDX,
     *		    SABMDS_INDX,
     *		    SABMDR_INDX
C
	PARAMETER (SAP_INDX=1,
     *		   PORT_INDX=2,
     *		   ONL_INDX=3,
     *		   DISC_INDX=4,
     *		   NDPR_INDX=5,
     *		   NDPS_INDX=6,
     *		   DISC00_INDX=7,
     *		   DISC05_INDX=8,
     *		   DISC09_INDX=9,
     *		   DISCOT_INDX=10,
     *		   MVC_INDX=11,
     *		   SABMDS_INDX=12,
     *		   SABMDR_INDX=13)
C
	INTEGER*4 DIRECTION,START,I
	INTEGER*4 NEW_SAP,PORT_STN,PORT_CNTR
C	    ! ARRAY WITH LENGTH OF EACH INTEGER TO BE DISPLAYED
	INTEGER*4 GTXDATA(MAX_DATA,X2X_SAP*X2X_SAP_PORTS)
	INTEGER*4 BEG_PORT
C	INTEGER*4 POS(MAX_DATA)
C***	DATA POS /3,3,5,5,6,6,5,5,5,5,5,5,5/
C***	    ! ARRAY WITH LENGTH OF EACH INTEGER TO BE DISPLAYED
C
	IF (NUM_FORW.EQ.0) THEN
	    DIRECTION = - NUM_BACK
	ELSEIF (NUM_BACK.EQ.0) THEN
	    DIRECTION = NUM_FORW
	ENDIF
C
	PORT_STN = 0
	DO 2000,SAP=1,X2X_SAP
	    PORT_CNTR = 0
	    DO 3000,PORT=1,X2X_SAP_PORTS
		IF (X2XE_ACT_STATUS(SAP).EQ.X2XES_NOTUP) GOTO 2000
		IF (X2XE_LOCAL_PORT_STATE(PORT,SAP).EQ.0) GOTO 3000
		PORT_STN = PORT_STN + 1
		PORT_CNTR = PORT_CNTR + 1
		GTXDATA(SAP_INDX,PORT_STN) = SAP
		GTXDATA(PORT_INDX,PORT_STN) = PORT
		GTXDATA(ONL_INDX,PORT_STN) = X2XE_LOCAL_PORT_STATE(PORT,SAP)
		CALL MOV2TOI4(GTXDATA(DISC_INDX,PORT_STN),
     *	          X2XE_LOCAL_PORT_MAINTENANCE(1,1,PORT_CNTR,SAP),
     *		    X2MAINT_T1M_NO_DISCONN - 1)
		CALL MOV2TOI4(GTXDATA(NDPR_INDX,PORT_STN),
     *		    X2XE_LOCAL_PORT_MAINTENANCE(1,1,PORT_CNTR,SAP),
     *		    X2MAINT_T1M_TOT_NO_DAT_PACK_REC - 1)
		CALL MOV2TOI4(GTXDATA(NDPS_INDX,PORT_STN),
     *		    X2XE_LOCAL_PORT_MAINTENANCE(1,1,PORT_CNTR,SAP),
     *		    X2MAINT_T1M_TOT_NO_DAT_PACK_SEN - 1)
		CALL MOV2TOI4(GTXDATA(DISC00_INDX,PORT_STN),
     *		    X2XE_LOCAL_PORT_MAINTENANCE(1,2,PORT_CNTR,SAP),
     *		    X2MAINT_T2M_NO_DISCONN_00 - 1)
		CALL MOV2TOI4(GTXDATA(DISC05_INDX,PORT_STN),
     *		    X2XE_LOCAL_PORT_MAINTENANCE(1,2,PORT_CNTR,SAP),
     *		    X2MAINT_T2M_NO_DISCONN_05 - 1)
		CALL MOV2TOI4(GTXDATA(DISC09_INDX,PORT_STN),
     *		    X2XE_LOCAL_PORT_MAINTENANCE(1,2,PORT_CNTR,SAP),
     *		    X2MAINT_T2M_NO_DISCONN_09 - 1)
		CALL MOV2TOI4(GTXDATA(DISCOT_INDX,PORT_STN),
     *		    X2XE_LOCAL_PORT_MAINTENANCE(1,2,PORT_CNTR,SAP),
     *		    X2MAINT_T2M_NO_DISCONN_OT - 1)
		CALL MOV2TOI4(GTXDATA(MVC_INDX,PORT_STN),
     *		    X2XE_LOCAL_PORT_MAINTENANCE(1,3,PORT_CNTR,SAP),
     *		    X2MAINT_T3M_RUN_MAX_VC_CONN - 1)
		CALL MOV2TOI4(GTXDATA(SABMDS_INDX,PORT_STN),
     *		    X2XE_LOCAL_PORT_MAINTENANCE(1,3,PORT_CNTR,SAP),
     *		    X2MAINT_T3M_NO_SABM_DISC_SENT - 1)
		CALL MOV2TOI4(GTXDATA(SABMDR_INDX,PORT_STN),
     *		    X2XE_LOCAL_PORT_MAINTENANCE(1,3,PORT_CNTR,SAP),
     *		    X2MAINT_T3M_NO_SABM_DISC_REC - 1)
3000	    CONTINUE
2000	CONTINUE
C
	WRITE(CLIN3,9000)
	WRITE(CLIN4,9100)
	WRITE(CLIN5,9200)
C
        DO I=1,PORT_STN
            IF (GTXDATA(SAP_INDX,I).EQ.BEG_SAP .AND.
     *	        GTXDATA(PORT_INDX,I).EQ.BEG_PORT) THEN
                START = I
                GOTO 4000
            ENDIF
        END DO
        START = 1
4000    CONTINUE
C
	CALL X2SHOW_TABLE(GTXDATA,MAX_DATA,PORT_STN,
     *	      START,6,20,1,80,DIRECTION,NEW_SAP)
C
	BEG_SAP = GTXDATA(SAP_INDX,NEW_SAP)
	BEG_PORT = GTXDATA(PORT_INDX,NEW_SAP)
C
	RETURN
C
9000	FORMAT(1X,'SAP|Prt|  On |  No.|  No. |  No. |Code |Code |Code |'
     *    'Code |Max. |SABM |SABM |')
9100	FORMAT(1X,'No.|No.|Line |Disc | Sent | Recv |  00 |  05 |  09 |'
     *	  'other| SVC |DiscS|DiscR|')
9200	FORMAT(' ')
	END

