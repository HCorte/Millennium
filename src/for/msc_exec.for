C
C *** SUBROUTINE MSC_EXEC ***
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]MSC_EXEC.FOV                                 $
C  $Date::   17 Apr 1996 14:07:24                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C *** Pre-Baseline Source - msc_exec.for ***
C
C V01 23-APR-93 RRB INITIAL RELEASE
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode Island,
C and contains confidential and trade secret information. It may not be
C transferred from the custody or control of GTECH except as authorized in
C writing by an officer of GTECH. Neither this item nor the information it
C contains may be used, transferred, reproduced, published, or disclosed,
C in whole or in part, and directly or indirectly, except as expressly
C authorized by an officer of GTECH, pursuant to written agreement.
C
C Copyright 1994 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Purpose:
C	DETERMINE IF INBOUND OR OUTBOUND BUFFER AND PROCESS ACCORDINGLY.
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE MSC_EXEC(PARAM)
C
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:MSCCOM.DEF'
        INCLUDE 'INCLIB:MSCEVN.DEF'
        INCLUDE 'INCLIB:DESNET.DEF'
C
        INCLUDE '($IODEF)'
        INCLUDE '($SSDEF)'
        INCLUDE '($SYSSRVNAM)'
C
	INTEGER*4 PARAM, STATUS, BUF
C
C BEGIN BUFFER PROCESSING
C
1000    CONTINUE
	CALL RTL(BUF,MSCEXEC,STATUS)
	IF(STATUS.EQ.2) THEN      !LIST EMPTY.
D         TYPE*,IAM(),'LIST EMPTY IN MSC_EXEC'
	  GOTO 8000
	ENDIF
C
C DUMP BUFFER IF NOT LIVE SYSTEM OR PROCESSING DISABLED
C
        IF(P(SYSTYP).NE.LIVSYS.OR.P(SUPMSC).NE.0) THEN
           CALL MSCRELB(BUF)
           GOTO 1000
        ENDIF
C
C DETERMINE BUFFER TYPE
C
	IF(MSCBUF(MSCBLEN,BUF).EQ.MSCBTYPOUT) THEN
D          TYPE*,IAM(),'SEND BUFFER IN MSC_EXEC'
	   CALL MSC_SNDBUF(BUF)
	ELSEIF(MSCBUF(MSCBLEN,BUF).EQ.MSCBTYPIN) THEN
D          TYPE*,IAM(),'RECEIVE BUFFER IN MSC_EXEC'
	   CALL MSC_RCVBUF(BUF)
	ELSE
	   TYPE*,IAM(),'Invalid buffer type ',MSCBUF(MSCBLEN,BUF)
           CALL MSCRELB(BUF)
	ENDIF
C
	GOTO 1000  !PROCESS ALL BUFFERS
C
8000    CONTINUE
	RETURN
	END
