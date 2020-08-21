C
C SUBROUTINE X2LODTTN
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2LODTTN.FOV                                 $
C  $Date::   17 Apr 1996 16:22:22                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - x2lodttn.for;1 **
C
C X2LODTTN.FOR
C
C V03 05-DEC-94 SCD Integrate UK changes into X2X Baseline
C V02 24-MAR-94 MODIFIED TO USE I4TOBUF2 TO LOAD THE CHKBUF AND SWAP
C               THE BYTES IN X2XD_CHKSUM
C
C V01 01-DEC-91 DAS RELEASED FOR VAX (NETHERLANDS)
C
C This subroutine will load an array containing
C a checksum of each TTN class record.
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
	SUBROUTINE X2LODTTN
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2XTTN.DEF'
C
	INTEGER*2   CHKBUF(200)     !Checksum buffer
	INTEGER*4   TTN             !Station/terminal pointers
	INTEGER*4   ST              !Status
	INTEGER*4   TTNCNT          !Terminal count
	INTEGER*4   TTNFDB(7)       !Local file descriptor block
	INTEGER*4   CHKSUM2, CHKSUM1, I, CHKLEN
	CHARACTER   X2FILNAM*20     !File name function
C
C OPEN THE TTN CONFIGURATION FILE.
C
	TTNCNT=0
	CALL OPENX(12,X2FILNAM(XTTN),4,0,0,ST)
	CALL IOINIT(TTNFDB,12,X2XTTN_SECT*256)
	IF(ST.NE.0) THEN
	  CALL OPS('X2LODTTN: ERROR OPENING TTN',ST,ST)
	  GOTO 8000
	ENDIF
C
C PROCESS ALL RECORDS CONFIGURED.
C
	TTN=0
C***  WRITE(5,9000)
100	CONTINUE
	  TTN=TTN+1
	  IF(TTN.GT.X2X_MAXTTN) GOTO 8000
	  CALL READW(TTNFDB,TTN,X2XTTN_REC,ST)
	  IF(ST.EQ.144) GOTO 8000
	  IF(ST.NE.0) THEN
	    CALL OPS('X2LODTTN: ERROR READING TTN',ST,ST)
	    GOTO 8000
	  ENDIF
	  IF(X2XTTN_REC(1).LE.0) GOTO 100
	  CALL FASTMOV(X2XTTN_REC,					! V03
     *		  X2XD_TTN_RECORD_BACKUP(1,X2XTTN_REC(1)),128)		! V03
C
C LOAD DATA INTO A BUFFER OF 2 BYTE ELEMENTS.
C
	  CHKLEN=0
	  DO 200 I=2,X2XTTN_ENTRIES-1
            CALL I4TOBUF2(X2XTTN_REC(I),CHKBUF,CHKLEN)                  ! V02
	    CHKLEN=CHKLEN+2
200	  CONTINUE
C
C TTN PARAMETERS CAN CONTAIN UPTO 141 ENTIES.
C INCLUDE THIS FUTURE PARAMETERS INTO THE CHECKSUM.
C
	  DO 202 I=CHKLEN/2+1,141
	    CHKBUF(I)=0
	    CHKLEN=CHKLEN+2
202	  CONTINUE
C
C CALCULATE BOTH GTECH AND IEOR CHECKSUMS FOR TITAN PARAMETERS.
C
	  CHKSUM1=0
	  CHKSUM2=0
	  CALL X2CHKSUM(CHKBUF,0,CHKLEN,CHKSUM1,CHKSUM2)
          CALL ISBYTE(CHKSUM2,X2XD_CHKSUM(TTN),0)                   ! V02
          CALL ISBYTE(CHKSUM1,X2XD_CHKSUM(TTN),1)                   ! V02
	  TTNCNT=TTNCNT+1
	  GOTO 100
C
C CLOSE THE FILE AND RETURN.
C
8000	CONTINUE
C**** WRITE(5,9010) TTNCNT
	CALL CLOSEFILE(TTNFDB)
	RETURN
C
C     ==================== Format Statements =================
C
C***9000  FORMAT(1X,'Loading TTN Checksum information')
C***9010  FORMAT(1X,I6,' records loaded')
	END
