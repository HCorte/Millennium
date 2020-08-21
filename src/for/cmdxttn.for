C
C SUBROUTINE CMDXTTN
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]CMDXTTN.FOV                                  $
C  $Date::   17 Apr 1996 12:40:06                                         $
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
C V02 12-DEC-94 MODIFIED TO USE I4TOBUF2 TO LOAD THE CHKBUF AND SWAP
C               THE BYTES IN X2XD_CHKSUM WHEN DETERMINING CHECKSUMS
C		SIMILAR TO X2LODTTN
C
C =============================================================
C CMDXTTN
C
C This subroutine will clear the TITN checksums when the
C TITN class has been modified.
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
        SUBROUTINE CMDXTTN(FIELD,ALLREC,ADDFLG)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XTTN.DEF'
C
	INTEGER*2   CHKBUF(200)     !Checksum buffer
	INTEGER*4   TTN             !Station/terminal pointers
	INTEGER*4   CHKSUM2, CHKSUM1, I, CHKLEN
C
        INTEGER*4 ALLREC(128), FIELD, ADDFLG
C
	CALL FASTMOV(ALLREC,X2XTTN_REC,128)
C
C LOAD DATA INTO A BUFFER OF 2 BYTE ELEMENTS.
C
	TTN=X2XTTN_REC(1)
C
	CALL FASTMOV(ALLREC,X2XD_TTN_RECORD_BACKUP(1,TTN),128)
	IF (TTN.LE.0) RETURN
	CHKLEN=0
	DO 200 I=2,X2XTTN_ENTRIES-1
      	  CALL I4TOBUF2(X2XTTN_REC(I),CHKBUF,CHKLEN)                  ! V02
	  CHKLEN=CHKLEN+2
200	CONTINUE
C
C TTN PARAMETERS CAN CONTAIN UPTO 141 ENTIES.
C INCLUDE THIS FUTURE PARAMETERS INTO THE CHECKSUM.
C
	DO 202 I=CHKLEN/2+1,141
	    CHKBUF(I)=0
	    CHKLEN=CHKLEN+2
202	CONTINUE
C
C CALCULATE BOTH GTECH AND IEOR CHECKSUMS FOR TITAN PARAMETERS.
C
	CHKSUM1=0
	CHKSUM2=0
	CALL X2CHKSUM(CHKBUF,0,CHKLEN,CHKSUM1,CHKSUM2)
        CALL ISBYTE(CHKSUM2,X2XD_CHKSUM(TTN),0)                   ! V02
        CALL ISBYTE(CHKSUM1,X2XD_CHKSUM(TTN),1)                   ! V02
        RETURN
        END
