C
C SUBROUTINE CMDXGRP
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]CMDXGRP.FOV                                  $
C  $Date::   17 Apr 1996 12:39:40                                         $
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
C CMDXGRP
C
C This subroutine loads the relay group information into
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
        SUBROUTINE CMDXGRP(FIELD,ALLREC,ADDFLG)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:X2XCOM.DEF'
        INCLUDE 'INCLIB:X2XGRP.DEF'
C
        INTEGER*4   GRP
        INTEGER*4   FIELD
        INTEGER*4   ALLREC(128)     !Record buffer
        INTEGER*4   ADDFLG          !New network port
C
C UPDATE COMMON WITH THE RELAY GROUP CONFIGURATION.
C
        CALL FASTMOV(ALLREC,X2XGRP_REC,128)
        GRP=X2XGRP_GROUP
        IF(GRP.LE.0.OR.GRP.GT.X2X_NUM_GROUPS) GOTO 8000
C
        IF(FIELD.EQ.5) X2XG_STATE(GRP) = X2XGRP_STATE
C
C ROUTINE EXIT.
C
8000    CONTINUE
        RETURN
        END
