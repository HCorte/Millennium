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
C
	SUBROUTINE SETPARAMS()
	IMPLICIT NONE
	INCLUDE 'INCLIB:SYSDEFINE.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:DESPAR.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'

	COMMON /SCF/ SCFREC

	SCFPAR(SUPWAG) = 1
	SCFPAR(SUPCAN) = 1
	SCFPAR(SUPVAL) = 1
	SCFPAR(SUPINS) = 1
	SCFPAR(ROMREV) = '0000'
	SCFPAR(GVTREV) = '0000'
	SCFPAR(CANTIM) = 30
	SCFPAR(MESLOG) = 5
	SCFPAR(MAXSPT) = 20000
	SCFPAR(CSHDAY) = 100
	SCFPAR(ODSUPD) = 300
	SCFPAR(TSLIAB) = 3000000
	SCFPAR(TSLMAX) = 50000
	SCFPAR(TSWMAX) = 100000
	SCFPAR(CANDRW) = 5
	SCFPAR(TVTIME) = 10
	SCFPAR(TSMXODD) = 300000
	SCFPAR(TSMXLI) = 15000 
	SCFPAR(MAXDBL) = 500
	SCFPAR(MAXCPL) = 500
	SCFPAR(MAXWIT) = 500
	SCFPAR(GVTEST) = 1
	SCFPAR(GVTGPL) = 10
	SCFPAR(GVTTIM) = 10 
	SCFPAR(FPTTIM) = 100
	SCFPAR(GVTSUP) = 10
	SCFPAR(FWDCNT) = 7
	SCFPAR(BCHSIZ) = 28 
	SCFPAR(VALPRNT) = 1
	SCFPAR(VALPAMT) = 300
	SCFPAR(ACTPRNT) = 1
	SCFPAR(CHKTIM) = 9000
	SCFPAR(MAXTRP) = 500 
	SCFPAR(MAXSSC) = 500
	SCFPAR(MAXSSN) = 10000 
	SCFPAR(SSCINB) = 50 
	SCFPAR(MAXTRPS) = 25000
	SCFPAR(MAXSSCS) = 25000
	SCFPAR(REG_DRWPCK) = 'DRAW'
	SCFPAR(ODD_DRWPCK) = 'DRAW'
	SCFPAR(CHKWRT) = 1
	SCFPAR(FSE_SNON) = 1
	END
