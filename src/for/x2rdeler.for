C
C SUBROUTINE X2RDELER
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2RDELER.FOV                                 $
C  $Date::   17 Apr 1996 16:29:42                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C ** Source - x2xrel.for **
C
C 
C X2RDELER.FOR
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C 
C     SUBROUTINE:
C        X2RDELER(BUFFER,STATION,PROCESS,STATUS) 
C
C     PURPOSE:
C        STOPS A PROCESS BY DESTINATION
C        BUILDS STOP COMMAND 
C
C     INPUT:
C       BUFFER       -     BUFFER WITH ERROR MESSAGE
C       DESTINATION  -     STATION NUMBER
C
C     OUTPUT:
C       PROCESS      -     PROCESS NUMBER
C       STATUS       -     -1 (ALWAYS) NOTHING TO SEND
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
	SUBROUTINE X2RDELER(BUFFER,STATION,PROCESS,STATUS)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2FEMES.DEF'
	INCLUDE 'INCLIB:PRMPRO.DEF'
	INCLUDE 'INCLIB:X2XREL.DEF'
C 
	INTEGER*2 BUFFER(*)
	INTEGER*4 STATUS, PROCESS, STATION
C 
	IF (IAND(X2X_DEBUG,X2X_DEBUG_X2XREL).NE.0) THEN
           TYPE *,'X2RDELERR: STATION ,PROCESS ',
     *                        STATION, PROCESS
        ENDIF
C 
C       GET PROCESS NUMBER FROM ERROR MESSAGE
C
	CALL ILBYTE(PROCESS,BUFFER,X2FEMES_HOST_ID - 1)
	PROCESS=IAND(PROCESS,X2FEMES_HOST_ID_PROCESS_MASK)
C 
C       RESTORE LAST STATION ID TO THE VALUE PRIOR TO THE SEND
C       WILL WANT TO RE-SEND 
C 
	X2XR_STATION_ID(1,STATION,PROCESS)=
     *	         X2XR_LAST_STATION_ID(1,STATION,PROCESS)
	X2XR_STATION_ID(2,STATION,PROCESS)=
     *	         X2XR_LAST_STATION_ID(2,STATION,PROCESS)
C 
C       STATUS IS ALWAYS (-1)
C   
	STATUS=-1    
C
	RETURN
	END
