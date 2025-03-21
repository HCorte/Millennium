C
C V03 29-APR-2011 RXK Opening of TCF and VLF moved to GUI_039.
C V02 20-MAR-2001 UXN Passive game added.
C V01 08-NOV-2000 UXN Initial release.
C
C SUBROUTINE TO OPEN FILES FOR GUIMGR
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GUIOPN
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:DESLOG.DEF'
	INCLUDE 'INCLIB:PRMVLF.DEF'
	INCLUDE 'INCLIB:GUIFIL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:RECAGT.DEF'
        INCLUDE 'INCLIB:RECDAF.DEF'
        INCLUDE 'INCLIB:DLTREC.DEF'
        INCLUDE 'INCLIB:DSPREC.DEF'
	INCLUDE 'INCLIB:DTGREC.DEF'
        INCLUDE 'INCLIB:DKKREC.DEF'
        INCLUDE 'INCLIB:DNBREC.DEF'
        INCLUDE 'INCLIB:DTSREC.DEF'
        INCLUDE 'INCLIB:DSCREC.DEF'
        INCLUDE 'INCLIB:DWIREC.DEF'
        INCLUDE 'INCLIB:DBNREC.DEF'
        INCLUDE 'INCLIB:DDBREC.DEF'
        INCLUDE 'INCLIB:DCPREC.DEF'
        INCLUDE 'INCLIB:DSSREC.DEF'
        INCLUDE 'INCLIB:DTRREC.DEF'
        INCLUDE 'INCLIB:DSTREC.DEF'
	INCLUDE 'INCLIB:DPAREC.DEF'
        INCLUDE 'INCLIB:X2XCOM.DEF'
        INCLUDE 'INCLIB:X2XSTN.DEF'
        INCLUDE 'INCLIB:X2XSCL.DEF'
        INCLUDE 'INCLIB:X2XTER.DEF'

	INTEGER*4 GAMSEC(MAXTYP)
	INTEGER*4 SECTOR, GTYPE, I, UNIT, ST
	DATA GAMSEC/DLTSEC,DSPSEC,DNBSEC,DKKSEC,DBNSEC,DWISEC,
     *              DCPSEC,DTRSEC,DDBSEC,DSTSEC,DTSSEC,DSCSEC,
     *		    DSSSEC,0,DTGSEC,DPASEC,0,0/
C
C OPEN TMF FILE
C
	CALL OPENW(10,SFNAMES(1,PTMF),4,0,0,ST)
        CALL IOINIT(TMFFDB,10, DBLOCK / 64 * 256)
        IF (ST.NE.0) CALL OPS('Failed to open TMF file, st',ST,0)
C
C OPEN ASF FILE
C
	CALL OPENW(11,SFNAMES(1,ASF),4,0,0,ST)
	CALL IOINIT(ASFFDB,11,ASFSEC*256)
	IF(ST.NE.0) CALL OPS('Failed to open ASF file, st',ST,0)
C
C OPEN DAF FILE
C
	CALL OPENW(12,SFNAMES(1,DAF),4,0,0,ST)
	CALL IOINIT(DAFFDB,12,DAFSEC*256)
	IF(ST.NE.0) CALL OPS('Failed to open DAF file, st',ST,0)
C
C OPEN X2XSTN.FIL
C
	CALL OPENW(13,SFNAMES(1,XSTN),4,0,0,ST)
	CALL IOINIT(X2STNFDB,13,X2XSTN_SECT*256)
	IF(ST.NE.0) CALL OPS('Failed to open X2XSTN.FIL, st',ST,0)
C
C OPEN X2XSCL.FIL
C
	CALL OPENW(14,SFNAMES(1,XSCL),4,0,0,ST)
	CALL IOINIT(X2SCLFDB,14,X2XSCL_SECT*256)
	IF(ST.NE.0) CALL OPS('Failed to open X2XSCL.FIL, st',ST,0)
C
C OPEN X2XTER.FIL
C
	CALL OPENW(15,SFNAMES(1,XTER),4,0,0,ST)
	CALL IOINIT(X2TERFDB,15,X2XTER_SECT*256)
	IF(ST.NE.0) CALL OPS('Failed to open X2XTER.FIL, st',ST,0)
C
C Open TCF.FIL
C
C 	 CALL IOPEN(SFNAMES(1,TCF),TCFLUN,LREC*2,LCDC,LSER*2-1,ST)
C        IF(ST.NE.0) CALL OPS('Failed to open TCF.FIL',ST,0)
C
C Open VLF.FIL
C
C        CALL IOPEN(SFNAMES(1,VLF),VLFLUN,VFLEN*2,VFSCDC,VFSSER*2-1,ST)
C        IF(ST.NE.0) CALL OPS('Failed to open VLF.FIL',ST,0)
C
C NB! units 18-19 are available to use.
C

C
C OPEN GAME FILES
C
	UNIT=20
	DO 10 I=1,MAXGAM
	   GTYPE=GNTTAB(GAMTYP,I)
	   IF(GTYPE.LT.1.OR.GTYPE.GT.MAXTYP) GOTO 10
	   SECTOR=GAMSEC(GTYPE)
	   IF(SECTOR.LT.1) GOTO 10 
	   CALL OPENW(UNIT,GFNAMES(1,I),4,0,0,ST)
	   CALL IOINIT(GAMFDB(1,I),UNIT,SECTOR*256)
	   IF(ST.NE.0) CALL OPS('Failed to open '//CGFNAMES(I)//', st', ST,0)
	   UNIT=UNIT+1
10	CONTINUE
	RETURN
	END
