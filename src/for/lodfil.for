C
C PROGRAM LODFIL
C $Log:   GXAFXT:[GOLS]LODFIL.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:52:58   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.5   25 Aug 1993 17:07:20   HXK
C  took out insrev
C  
C     Rev 1.4   12 Aug 1993 12:48:52   CXK
C  PUT INSTANT REVC INTO MEMORY ALSO
C  
C     Rev 1.3   06 Jul 1993 13:57:44   SXH
C  Load Max fractions
C  
C     Rev 1.2   23 Jun 1993 14:26:22   SXH
C  Load GLNAMES and GSNAMES
C  
C     Rev 1.1   23 Jun 1993 11:45:42   SXH
C  Added loading for GTNTAB,GNTTAB and KGNTAB
C  
C     Rev 1.0   21 Jan 1993 16:53:50   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - lodfil.for **
C
C LODFIL.FOR
C
C V01 09-JAN-92 GCAN INITIAL RELEASE FOR THE NETHERLANDS
C
C THIS PROGRAM WILL LOAD INTO MEMORY ALL SYSTEM FILE NAMES
C FROM THE SCF.FIL
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
C=======OPTIONS /CHECK=NOOVERFLOW
        PROGRAM LODFIL
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
C
	BYTE	    REV/01/		    !Program Revision.
C
	INTEGER*4   ST			    !Subroutine Return Status.
C
C DISPLAY PROGRAM NAME AND REVISION.
C
	TYPE*,IAM()
	TYPE*,IAM(),'<<<<< LODFIL  V',REV,
     *  ' LOADS SYSTEM FILE DESCRIPTIONS INTO MEMORY >>>>>'
	TYPE*,IAM()
C
C GET SYSTEM FILE CONFIGURATION FROM THE SCF.FIL
C
	CALL GETSCONF(SCFREC,ST)
	IF(ST.NE.0) THEN
	   TYPE*,IAM()
	   TYPE*,IAM(),'Unable to get System Controll Information'
	   TYPE*,IAM()
	   CALL BELLS(2)
	   CALL GSTOP(GEXIT_FATAL)
	ENDIF
C
C MOVE SYSTEM FILE NAMES FROM RECORD TO MEMORY.
C
        CALL FASTMOV(SCFSFN,SFNAMES,MAXFIL*5)
        CALL FASTMOV(SCFGFN,GFNAMES,MAXGAM*5)
        CALL FASTMOV(SCFFSZ,SFSIZES,MAXFIL)
        CALL FASTMOV(SCFGSZ,GFSIZES,MAXGAM)
C
	CALL FASTMOV(SCFGTN,   GTNTAB,   MAXTYP*MAXIND)
	CALL FASTMOV(SCFGNT,   GNTTAB,   MAXGAM*2)
	CALL FASTMOV(SCFKGN,   KGNTAB,   MAXGAM)
	CALL FASTMOV(SCFLGN,   GLNAMES,  MAXGAM*4)
	CALL FASTMOV(SCFSGN,   GSNAMES,  MAXGAM)

        CALL FASTMOV(SCFFRC,   MAXFRC,   MAXGAM)

C
	TYPE*,IAM()
	TYPE*,IAM(),'System File Descriptions Loaded into Memory.....'
	TYPE*,IAM()
	CALL GSTOP(GEXIT_SUCCESS)
	END
