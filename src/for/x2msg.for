C
C SUBROUTINE X2MSG
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2MSG.FOV                                    $
C  $Date::   17 Apr 1996 16:24:08                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2rcvbuf.for;1 **
C
C V02 05-DEC-94 SCD Integrate UK changes into X2X Baseline
C
C+++++++++++++++++++++++++++++++++++++++
C
C     X2MSG(LAYER,STATUS,STATION_NO,TERMINAL_NO,MOVE_LEN,BUFF,MESSAGE
C          ,BUF_SSAP,MESSAGE_LEN, ADR_LEN, ADDRESS)			!V02
C     IN:
C     LAYER    -     MESSAGE TYPE, SUBTYPE E.T.C.
C     STATION_NO-    STATION # MESSAGE SENT TO
C     ERROR_CODE-    ERROR CODE FOR THIS MESSAGE
C     TERMINAL_NO-   TERMINAL IT IS SENT TO
C     MOVE_LEN   -   DATA FROM BUFFER TO MOVE
C     BUFFER     -   BUFFER WITH DATA TO COPY TO MESSAGE
C     BUF_SSAP   -   MESSAGE SAP
C     ADR_LEN    -   STATION ADDRESS LENGTH OR HARDWARE ID LENGTH	!V02
C     ADDRESS(2) -   STATIONS ADDRESS					!V02
C     OUT:
C     MESSAGE    -   FILLED WITH DATA
C     MESSAGE_LEN-   MESSAGE LENGTH
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
	SUBROUTINE X2MSG(LAYER,STATUS,STATION_NO,TERMINAL_NO,BUFFER,
     *	 MOVE_LEN,MESSAGE,BUF_SSAP,MESSAGE_LEN,ADR_LEN,ADDRESS)		!V02
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2PTLMES.DEF'
	BYTE      BUFFER(0:*)     !ELIMINATE ILBYTE AND ISBYTE
	BYTE      MESSAGE(0:*)    !ELIMINATE ILBYTE AND ISBYTE
	INTEGER*4 TO_MOVE, OFFSET, X2XPRO_STATUS, SND_LAYER
	INTEGER*4 MESSAGE_LEN, BUF_SSAP, MOVE_LEN, TERMINAL_NO
	INTEGER*4 STATION_NO, STATUS, LAYER
	INTEGER*4 ADR_LEN, ADDRESS(2)				!V02
	INTEGER*4 LOCAL_ADDRESS(2)				!V02
C
	SND_LAYER=LAYER
	IF (SND_LAYER/'00010000'X.EQ.X2X_MESTYP_ERR)
     *	SND_LAYER='00010000'X*X2X_MESTYP_CMD+MOD(SND_LAYER,'00010000'X)
C
	CALL I4TOBUF4(SND_LAYER,MESSAGE,X2PRO_MESTYP-1)
	X2XPRO_STATUS=STATUS       !X2XPRO WANTS TO SEE LOWER BITS ONLY
	X2XPRO_STATUS=IAND(X2XPRO_STATUS,'0000007F'X)  ! !!!!
	MESSAGE(X2PRO_STATUS-1) = X2XPRO_STATUS
	CALL I4TOBUF2(STATION_NO,MESSAGE,X2PRO_STATION-1)
	CALL I4TOBUF2(TERMINAL_NO,MESSAGE,X2PRO_TERMINAL-1)
	MESSAGE(X2PRO_SSAP-1) = BUF_SSAP
CV02	OFFSET=((X2PRO_OFFSET+1)/2)*2+1      !FIRST OFFSET FOR DATA
C 	***** Start V02 changes *****
	IF (STATUS.NE.X2ERR_BADADR) THEN
	    OFFSET=((X2PRO_OFFSET+1)/2)*2+1      !FIRST OFFSET FOR DATA
	ELSE
	    OFFSET=((X2PRO_OFFSET+13)/2)*2+1      !FIRST OFFSET FOR DATA
	    MESSAGE(X2PRO_ADR_LEN-1)=ADR_LEN
	    LOCAL_ADDRESS(1)=0
	    LOCAL_ADDRESS(2)=0
	    IF (ADR_LEN.GT.0) THEN
		LOCAL_ADDRESS(1)=ADDRESS(1)
		LOCAL_ADDRESS(2)=ADDRESS(2)
		CALL X2QSHFT(LOCAL_ADDRESS,(64-ADR_LEN*4))
	    ENDIF
	    CALL I4TOBUF4(LOCAL_ADDRESS(1),MESSAGE,X2PRO_ADR-1)
	    CALL I4TOBUF4(LOCAL_ADDRESS(2),MESSAGE,X2PRO_ADR+4-1)
D	    IF (IAND(X2X_DEBUG,1024).NE.0) 
D     *	      TYPE 9000,LOCAL_ADDRESS(1),LOCAL_ADDRESS(2),
D     *		      (MESSAGE(I),I=0,19)
D9000	      FORMAT(2(1X,Z8.8),20(1X,Z2.2))
	ENDIF
C 	***** End V02 changes *****
	MESSAGE(X2PRO_OFFSET-1) = OFFSET
	TO_MOVE=(MOVE_LEN+1)/2
	IF (MOVE_LEN.GT.X2PRO_MAXMES) TO_MOVE=X2PRO_MAXMES/2
	CALL MOVTAB2(BUFFER,MESSAGE((((OFFSET+1)/2)*2)-2),TO_MOVE)
	MESSAGE_LEN=OFFSET+TO_MOVE*2
	IF (IAND(X2X_DEBUG,X2X_DEBUG_SUBS).NE.0)
     *	TYPE*,'X2MSG ',LAYER,X2X_PTLMES(STATUS),STATION_NO,TERMINAL_NO,
     *	           BUFFER(1),MOVE_LEN,MESSAGE(1),BUF_SSAP,MESSAGE_LEN,	!V02
     *             ADDRESS(1),ADDRESS(2)				!V02
	RETURN
	END
