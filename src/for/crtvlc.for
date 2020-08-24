C
C PROGRAM CRTVLC
C 
C V02 15-DEC-1999 UXN MULTIWIN changes.
C V01 10-JAN-1999 GPW INITIAL RELEASE FOR FINLAND (FROM VLF2VLC)
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	PROGRAM CRTVLC
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:STOPCOM.DEF'

C
	INTEGER*4 I
	INTEGER*4 K, ST
C
	CALL COPYRITE
	CALL SNIF_AND_WRKSET
C
C
	CALL GETSCONF(SCFREC,ST)
C
C CREATE NEW FILES
C
        IF(SCFSFN(1,VLF).NE.SCFSFN(1,VLC)) THEN
          TYPE*,IAM(),'Validation copy file must have same volume as vlf'
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
        IF(SCFFSZ(VLC).NE.SCFFSZ(VLF)) THEN
          TYPE*,IAM(),'Validation copy file must be same size as vlf'
          CALL GSTOP(GEXIT_FATAL)
        ENDIF
        CALL CRTFIL(SCFSFN(1,VLC),SCFFSZ(VLC),ST)
        IF(ST.NE.0) THEN
          WRITE(6,904) (SCFSFN(K,VLC),K=1,5)
          CALL GPAUSE
        ELSE
	  VLCSTS = WCLR
	ENDIF
C
        CALL GSTOP(GEXIT_SUCCESS)
C

904	FORMAT(1X,A,1X,5A4,' Iopen error > ',I4)
	END
