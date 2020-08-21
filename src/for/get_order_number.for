C
C GET_ORDER_NUMBER.FOR
C
C V02 13-NOV-2003 FRP Modify for Batch2 Totobola Changes.
C V01 05-JAN-2001 EPH INITIAL RELEASE FOR PORTUGAL
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
C Copyright 2001 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GET_ORDER_NUMBER (ORDER_NUMBER, EXT)
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'

	COMMON SCFREC

	INTEGER*4 ORDER_NUMBER(MAXGAM)

	INTEGER*4 SCFNAM(5),FDB(7)
	INTEGER*4 ST, EXT, I

C	DATA SCFNAM/'FILE',':SCF','.FIL','    ','    '/
	DATA SCFNAM/'SCF.','FIL ','    ','    ','    '/


	CALL COPYRITE
C
C       READ SCF RECORD
C
        EXT = 0
	CALL OPENW(1,SCFNAM,4,0,0,ST)
	CALL IOINIT(FDB,1,SCFSEC*256)
	IF (ST.NE.0) THEN
           EXT = 1
           RETURN
        ENDIF
	CALL READW(FDB,1,SCFREC,ST)
	IF (ST.NE.0) THEN
           CALL CLOSEFIL(FDB)
           EXT = 1
           RETURN
        ENDIF
	CALL CLOSEFIL(FDB)

        DO I=1,MAXGAM
	   ORDER_NUMBER(I) = SCF_ORDER(I)
        ENDDO

	RETURN
        END
