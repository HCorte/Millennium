C
C SUBROUTINE X2BLD_UPDSTN
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2BLD_UPDSTN.FOV                             $
C  $Date::   17 Apr 1996 16:10:14                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2bldfil.for;1 **
C
C V02 12-DEC-1994 GPR NO LONGER NEED TO LOAD STATION RECORD WITH 
C		      ALL CLASS VALUES
C
C ===========================================================
C X2BLD_UPDSTN
C
C This routine will update the station record from the
C X2XBLD.FIL and the X2XSCL.FIL.
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
	SUBROUTINE X2BLD_UPDSTN
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XSTN.DEF'
	INCLUDE 'INCLIB:X2XSCL.DEF'
	INCLUDE 'INCLIB:X2XBLD.DEF'
C
	INTEGER*4 I
C
C FILL IN ALL STATION FIELDS.
C
	X2XSTN_STN = X2XBLD_STN
	X2XSTN_ADDRES(1) = X2XBLD_ADDRES(1)
	X2XSTN_ADDRES(2) = X2XBLD_ADDRES(2)
	X2XSTN_SERIAL  = X2XBLD_SERIAL
	X2XSTN_GROUP   = 0
	X2XSTN_STNCLS  = X2XBLD_STNCLS
CV02	X2XSTN_REPCLS  = X2XBLD_REPCLS
	X2XSTN_PVC     = X2XBLD_PVC
	X2XSTN_PVCPORT = X2XBLD_PVCPORT
	X2XSTN_ADDLEN  = X2XSCL_ADDLEN
CV02	X2XSTN_PROTO   = X2XSCL_PROTO
CV02	X2XSTN_PRTCNT  = X2XSCL_PRTCNT
	X2XSTN_TYPE    = X2XSCL_TYPE
	X2XSTN_DELACK  = X2XSCL_DELACK
	X2XSTN_ERRREP  = X2XSCL_ERRREP
CV02	X2XSTN_STNDIS  = X2XSCL_STNDIS
CV02	X2XSTN_FEDIS   = X2XSCL_FEDIS
CV02	X2XSTN_NETPT1  = X2XSCL_NETPT1
CV02	X2XSTN_NETPT2  = X2XSCL_NETPT2
CV02	X2XSTN_NETPT3  = X2XSCL_NETPT3
CV02	X2XSTN_NETPT4  = X2XSCL_NETPT4
CV02	X2XSTN_NETPT5  = X2XSCL_NETPT5
CV02	X2XSTN_NETPT6  = X2XSCL_NETPT6
CV02	X2XSTN_NETPT7  = X2XSCL_NETPT7
CV02	X2XSTN_POLTIM  = X2XSCL_POLTIM
	X2XSTN_NETSTAT = X2XSCL_NETSTAT
	X2XSTN_AUTOUPD = X2XSCL_AUTOUPD
CV02	X2XSTN_BAUD    = X2XSCL_BAUD
CV02	X2XSTN_TTN_PORT1 = 0
CV02	X2XSTN_TTN_PORT2 = 0
	X2XSTN_DEF_PORT1 = 0
	X2XSTN_DEF_PORT2 = 0
CV02	X2XSTN_DIALENA   = X2XSCL_DIALENA
CV02    IF(X2XSCL_DIAL_PORT1.EQ.0) THEN
CV02      X2XSTN_DIAL_PORT1=X2XBLD_DIAL_PORT1
CV02    ELSE
CV02      X2XSTN_DIAL_PORT1= X2XSCL_DIAL_PORT1
CV02    ENDIF
CV02    IF(X2XSCL_DIAL_PORT2.EQ.0) THEN
CV02      X2XSTN_DIAL_PORT2=X2XBLD_DIAL_PORT2
CV02    ELSE
CV02      X2XSTN_DIAL_PORT2= X2XSCL_DIAL_PORT2
CV02    ENDIF
        X2XSTN_X32_PORT1 = X2XSCL_X32_PORT1
        X2XSTN_X32_PORT2 = X2XSCL_X32_PORT2
        X2XSTN_VSP = 0
	X2XSTN_PRTFLG  = X2X_NONE_PRINT
	X2XSTN_STATE   = X2XS_IDLE
C
C SET BITMAP INDICATING ALL FIELDS HAVE BEEN MODIFIED, ALLOWING
C X2POST TO UPDATE MEMORY.
C
	DO 100 I=1,X2XSTN_ENTRIES
	  CALL BSET(X2XSTN_BITMAP,I)
100	CONTINUE
	RETURN
	END
