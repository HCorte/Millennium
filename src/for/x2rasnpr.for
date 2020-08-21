C
C SUBROUTINE X2RASNPR
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2RASNPR.FOV                                 $
C  $Date::   17 Apr 1996 16:27:40                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C ** Source - x2xrel.for **
C
C V02 20-JUL-94 WS MULTINETWORK CHANGES - Integrate UK changes 
C		   into X2X Baseline
C
C X2RASNPR.FOR
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C 
C     SUBROUTINE:
C        X2RASNPR(BUFFER,PROCESS)
C
C     PURPOSE:
C        ASSIGNS A PROCESS NUMBER FOR STARTS
C        PROCESS MUST BE IDENTICAL TO ADD TO AN ALREADY ACTIVE PROCESS
C
C     INPUT:
C       BUFFER       -     BUFFER MESSAGE
C	SUBNETWORK   -	   SUBNETWORK NO TO START PROCESS FROM - V02
C
C     OUTPUT:
C       PROCESS      -     PROCESS NUMBER TO BE ASSIGNED
C                          0 IF NONE AVAILABLE
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
	SUBROUTINE X2RASNPR(BUFFER,SUBNETWORK,PROCESS)		!V02
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:PRMPRO.DEF'
	INCLUDE 'INCLIB:X2XREL.DEF'
	INCLUDE 'INCLIB:X2RCMD.DEF'
C
	INTEGER*2 BUFFER(*)
	INTEGER*4 SUBNETWORK					!V02
	INTEGER*4 ACT_BYTE, IN_BYTE, OFF, NEXT_PROCESS, ATRIBUTE
	INTEGER*4 DATA_DEST, DATA_LEN, MES_NUM, PROCESS
C
        PROCESS = 0
	CALL MOV2TOI4(MES_NUM,BUFFER,X2ROFF_DATA_MSGNUM-1)
	CALL MOV2TOI4(DATA_LEN,BUFFER,X2ROFF_DATA_LEN-1)
	CALL ILBYTE(DATA_DEST,BUFFER,X2ROFF_DATA_DEST-1)
	CALL ILBYTE(ATRIBUTE,BUFFER,X2ROFF_DATA_ATRIBUTE-1)
C
	IF (DATA_LEN.EQ.0 .AND. MES_NUM.EQ.0) RETURN
C
C       CHECK TO SEE IF THE STARTED PROCESS HAS THE SAME CHARACTERISTICS
C       AS ANY CURRENTLY ACTIVE PROCESS. IF SO ASSIGN THIS PROCESS #
C
	DO 20 NEXT_PROCESS=X2XR_FIRST_COMMON_PROCESS,X2X_RELAY_APPS
C
C          PROCESS MUST BE ACTIVE
	   IF (X2XR_APP_STATUS(NEXT_PROCESS).NE.X2XR_APPS_ACTIVE)
     *	                                                   GOTO 20
	   IF (X2XR_SUBNETWORK(NEXT_PROCESS).NE.SUBNETWORK) GOTO 20	!V02
C
C          MUST HAVE SAME MESSAGE NUMBER
C
 	   IF (X2XR_APP_DATA_MSGNUM(NEXT_PROCESS).NE.MES_NUM) GOTO 20
C
C          MUST HAVE SAME LENGTH
C
	   IF (X2XR_APP_DATA_LEN(NEXT_PROCESS).NE.DATA_LEN) GOTO 20
C
C          MUST HAVE SAME DESTINATION 
C
	   IF (X2XR_APP_DATA_DEST(NEXT_PROCESS).NE.DATA_DEST) GOTO 20
C
C          MUST HAVE SAME ATTRIBUTE(I.E. STNBRO, ALLBRO, ETC.)
C
	   IF (X2XR_APP_ATRIBUTE(NEXT_PROCESS).NE.ATRIBUTE) GOTO 20
C
	   IF (DATA_LEN.NE.0) THEN
C
C            CHECK THE MESSAGE IN THE BUFFER
C
	     DO 10 OFF=1,DATA_LEN
	        CALL ILBYTE(IN_BYTE,BUFFER,X2ROFF_DATA+OFF-1)
	        CALL ILBYTE(ACT_BYTE,X2XR_APP_DATA(1,NEXT_PROCESS),
     *	                                                OFF-1)
	        IF (IN_BYTE.NE.ACT_BYTE) GOTO 20
10	     CONTINUE
	   ENDIF
C
	   PROCESS=NEXT_PROCESS      !THIS PROCESS IS SAME AS EXISTING
	   RETURN
C
20	CONTINUE
C
C       FIND NEXT AVAILABLE PROCESS WHICH IS IDLE AND START IT
C
	DO 30 NEXT_PROCESS=X2XR_FIRST_COMMON_PROCESS,X2X_RELAY_APPS
	   IF(X2XR_APP_STATUS(NEXT_PROCESS).NE.X2XR_APPS_IDLE) GOTO 30
	   IF (X2XR_SUBNETWORK(NEXT_PROCESS).NE.SUBNETWORK) GOTO 30	!V02
	   PROCESS=NEXT_PROCESS
	   RETURN
30	CONTINUE
C
	RETURN
	END
