C
C SUBROUTINE X2RSTNTM
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2RSTNTM.FOV                                 $
C  $Date::   17 Apr 1996 16:32:42                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C ** Source - x2xrel.for **
C
C
C X2RSTNTM.FOR 
C
C V03 05-DEC-94 SCD Integrate UK changes into X2X Baseline
C V02 05-APR-94 GPR USE X2X_I4_STATION TO DETERMINE STATION AND TERNUM
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C 
C     SUBROUTINE:
C        X2RSTNTM(STATION,PROCESS,STATUS) 
C
C     PURPOSE:
C        SENDS A STATION TIME OUT                    
C        BUILDS MESSAGE AND SENDS TO RELAY APPLICATION INPUT QUEUE
C
C     INPUT:
C       STATION  -     STATION TO BE TIMED OUT
C       PROCESS  -     PROCESS NUMBER ASSIGNED
C
C     OUTPUT:
C       STATUS   -     0 IF OKAY
C                     -1 IF ERROR (NO BUFFER)
C 
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
C
	SUBROUTINE X2RSTNTM(STATION,PROCESS,STATUS)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:X2XREL.DEF'
	INCLUDE 'INCLIB:X2STMES.DEF'
C 
	INTEGER*4 PROBUF, STATUS, PROCESS, STATION
C
	IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0) THEN
           TYPE *,'X2RSTNTM:  STATION,PROCESS '
           TYPE *,'         ',STATION,PROCESS
        ENDIF
C
	CALL GETBUF(PROBUF)
	IF (PROBUF.LE.0) THEN
           STATUS=-1
           RETURN   
        ENDIF
C 
C       BUILD THE RELAY MESSAGE FOR THE STATION
C
	CALL X2RSTNMS(PRO(INPTAB,PROBUF),STATION,PROCESS)
C 

C       ***** Start V02 changes *****

        IF (X2X_I4_STATION) THEN
	   PRO(LINENO,PROBUF)=STATION
        ELSE
	   HPRO(LINENO,PROBUF)=STATION
        ENDIF

C       ***** End V02 changes *****

	HPRO(TRCODE,PROBUF)=TYPX2X_RELAY_TIMOUT
C
C       QUEUE TO RELAY APPLICATION INPUT QUEUE
C
CV03	CALL X2RADDBF(PROBUF)
	CALL ATL(PROBUF,X2XR_INPUT_QUEUE,STATUS)		!V03
	STATUS=0
C
	RETURN
	END
