C
C SUBROUTINE X2LOAD1
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2LOAD1.FOV                                  $
C  $Date::   17 Apr 1996 16:21:10                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2xrapp.for;1 **
C
C X2LOAD1.FTN
C
C PURPOSE: THIS ROUTINE WILL DETERMINE WHAT THE FIRST LOAD NUMBER IS TO
C          BE BASED ON THE TYPE OF BROADCAST BEING SENT.
C
C V04 11-SEP-95 DAS Modified to handle multiple aplications a.k.a leipzig
C V03 29-DEC-94 GPR Modified to handle multiple MCPs per subnetwork
C V02 13-DEC-94 GPR Integrate UK changes into X2X Baseline
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
C Copyright 1995 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE X2LOAD1(PROCESS,APPLICATION_NO,LOAD_NO)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:MSGCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PRMDLL.DEF'
	INCLUDE 'INCLIB:X2STMES.DEF'
	INCLUDE 'INCLIB:X2FEMES.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XREL.DEF'
        INCLUDE 'INCLIB:X2RLOD.DEF'
C
	INTEGER*4 LOAD_NO
        INTEGER*4 APPLICATION_NO
	INTEGER*4 PROCESS, LOAD
C
C       FULL DOWNLOAD...WITH MCP
C
        IF (X2XR_BRDCST_TYPE(PROCESS).NE.X2RLOD_SOFT_SEL .AND.
     *      X2XR_BRDCST_TYPE(PROCESS).NE.X2RLOD_NO_RESET_SEL .AND.
     *      X2XR_BRDCST_TYPE(PROCESS).NE.X2RLOD_FULL .AND.
     *      X2XR_BRDCST_TYPE(PROCESS).NE.X2RLOD_HARD_FULL .AND.
     *      X2XR_BRDCST_TYPE(PROCESS).NE.X2RLOD_SOFT_FULL .AND.
     *      X2XR_BRDCST_TYPE(PROCESS).NE.X2RLOD_NO_RESET_FULL) THEN
C
C           SELECT LOAD_NO OF MCP TO USE
C
            LOAD_NO = MCP_LOAD_NO
            IF (LOAD_NO.EQ.0) LOAD_NO = 1
            RETURN
        ENDIF
C
C       ALL OTHER TYPES....
C
        DO LOAD = 1,X2XR_LASTLOAD(PROCESS)
           IF (X2DLL_TIMES_TO_SEND(LOAD).LT.0.OR.
     *        SMFDLTAB(LOAD,APPTYPE,APPLICATION_NO).EQ.MCP_TYPE)GOTO 320
           IF (SMFDLNAM(1,LOAD,APPLICATION_NO) .NE. '    ' .AND.
     *         SMFDLNAM(1,LOAD,APPLICATION_NO) .NE. 0) THEN
C
C              SELECTIVE LOADS ...
C
               IF((X2XR_BRDCST_TYPE(PROCESS).EQ.X2RLOD_SOFT_SEL) .OR.
     *           (X2XR_BRDCST_TYPE(PROCESS).EQ.X2RLOD_NO_RESET_SEL))THEN
                 IF (SMFDLTAB(LOAD,SNDFLG,APPLICATION_NO) .EQ. 1) THEN
                     LOAD_NO = LOAD
                     GOTO 350
                ENDIF
C
C              NO MCP LOADS
C
               ELSEIF (X2XR_BRDCST_TYPE(PROCESS).EQ.X2RLOD_FULL   .OR.
     *           X2XR_BRDCST_TYPE(PROCESS).EQ.X2RLOD_HARD_FULL    .OR.
     *           X2XR_BRDCST_TYPE(PROCESS).EQ.X2RLOD_SOFT_FULL    .OR.
     *           X2XR_BRDCST_TYPE(PROCESS).EQ.X2RLOD_NO_RESET_FULL)THEN
                     LOAD_NO = LOAD
                     GOTO 350
               ENDIF
           ENDIF
320     CONTINUE
        END DO
        LOAD_NO = X2XR_LASTLOAD(PROCESS)+1
C
350     CONTINUE
        RETURN
	END
