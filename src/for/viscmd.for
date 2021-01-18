C
C SUBROUTINE VISCMD
C
C V06 01-JAN-2010 FJG ePassive
C V05 06-OCT-2000 UXN AlphaIPS release. ISBLDCMD() added.
C V04 19-MAY-1996 HXK Wojtek's security stuff added
C V03 13-JUN-1993 HXK added AGTINF.DEF, PRMAGT.DEF
C V02 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C SUBROUTINE TO PROCESS VISION COMMAND REQUESTS
C
C CALLING SEQUENCE:
C     CALL VISCMD(CBUF,STATUS)
C INPUT
C     CBUF   - COMMAND BUFFER (5 I*4 WORDS)
C OUTPUT
C     STATUS - QUEUEING STATUS (0 = GOOD, -1 = ERROR)
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE VISCMD(CBUF,STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
C
	INTEGER*4 CBUF(CDLEN), I
	INTEGER*4 STATUS,CNUMBER
C
C DETERMINE IF COMMAND CAN BE PROCESSED BY THIS VISION TASK
C
C
C Get command number group and number
C
	CNUMBER=0
	CNUMBER =  ((CBUF(3) * 1000) + CBUF(1))
C
	DO 10 I=1,168
	IF(CMDS(I,PASS).EQ.CNUMBER) GOTO 20
C***	IF(CMDS(I,PASS).EQ.-1) GOTO 20
10	CONTINUE
	WRITE(CLIN23,923) CNUMBER,PASS
	RETURN
C
C Call QUECMD to process buffer
C error, then set status tO -1 and return.
C
20	CONTINUE
	CALL OPSTXT('**********************CALL QUECMD***************************')
	CALL QUECMD(CBUF,STATUS)
	CALL OPS('So command status is:',STATUS,STATUS)
C
C IF AGENT PASSNUMBERS OR  AGENT TYPE ARE CHANGED ON THE PRIMARY,        
C SEND AN INTRASYSTEM COMMAND MSG TO LMS                                 
C
C
C REMOVED AGENT GVT UPDATE / AGENT STATUS UPDATE THAT TIMO ASKED FOR
C
C        IF(P(SYSTYP).EQ.LIVSYS) THEN                                    
C         IF(CBUF(3).EQ.TCAGT) THEN                                      
C            IF((CBUF(1).EQ.2).OR.(CBUF(1).EQ.3)) THEN                   
C             CALL ISBLDCMD(CBUF,STATUS)                                 
C             IF(STATUS.NE.0)THEN                                        
C                STATUS=-2                                               
C                RETURN                                                  
C             ENDIF                                                      
C            ENDIF                                                       
C         ENDIF                                                          
C
C        GVTID UPDATES
C
C         IF(CBUF(3).EQ.TCX2X) THEN                                      !V03
C            IF(CBUF(1).EQ.1.AND.CBUF(8).EQ.XSTN.AND.CBUF(9).EQ.38) THEN
C             CALL ISBLDCMD(CBUF,STATUS)
C             IF(STATUS.NE.0)THEN
C                STATUS=-2
C                RETURN
C             ENDIF
C            ENDIF
C         ENDIF
C        ENDIF                                                       !CDH0001
        RETURN
C
923	FORMAT('Incorrect security level',2(' [',I5.5,']'))
	END
