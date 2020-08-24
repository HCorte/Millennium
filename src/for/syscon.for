C
C SUBROUTINE SYSCON
C
C V06 17-OCT-2000 UXN Baseline release.
C V05 16-JUN-2000 UXN GETSCONF added.
C V04 16-JAN-1997 HXK Load IPS TCP address parameters at from SCF file to 
C                     CONCOM common
C V03 26-APR-1994 JXP Included win holding amount and day limits from SCF
C V02 15-AUG-1993 HXK LOAD INSTANT REV AND ALL OF NAMES INTO MEMORY
C V01 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C
C X2X Upgrade: 21-FEB-96 wsm Removed retrieval of SCFDLNAM and SCFDLTAB.
C
C SUBROUTINE TO LOAD SYSTEM CONFIGURATION FILE
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
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE SYSCON
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'

        INTEGER*4  ST                    !
C
	TYPE*,IAM(),' Reading system configuration file'
	CALL GETSCONF(SCFREC,ST)
	IF(ST.NE.0) CALL GSTOP(GEXIT_FATAL)

	CALL FASTMOV(SCFPAR,   P,        NUMPAR)
	CALL FASTMOV(SCFGTN,   GTNTAB,   MAXTYP*MAXIND)
	CALL FASTMOV(SCFGNT,   GNTTAB,   MAXGAM*2)
	CALL FASTMOV(SCFKGN,   KGNTAB,   MAXGAM)
	CALL FASTMOV(SCFLGN,   GLNAMES,  MAXGAM*4)
	CALL FASTMOV(SCFSGN,   GSNAMES,  MAXGAM)
	CALL FASTMOV(SCFSFN,   SFNAMES,  MAXFIL*5)
	CALL FASTMOV(SCFGFN,   GFNAMES,  MAXGAM*5)
	CALL FASTMOV(SCFFSZ,   SFSIZES,  MAXFIL)
	CALL FASTMOV(SCFGSZ,   GFSIZES,  MAXGAM)
	CALL FASTMOV(SCFTKC,   TKTCHG,   MAXGAM)
	CALL FASTMOV(SCFCOG,   COMGAM,   MAXGAM)
	CALL FASTMOV(SCFCOT,   COMTYP,   NUMFIN)
	CALL FASTMOV(SCFPRG,   PRGDAY,   MAXGAM)
	CALL FASTMOV(SCFRED,   REDMAX,   MAXGAM)
	CALL FASTMOV(SCFSTP,   STPFLG,   MAXIND)
        CALL FASTMOV(SCFGVN,   GVNAMES,  MAXGAM*5)
        CALL FASTMOV(SCFGVS,   GVSIZES,  MAXGAM)
        CALL FASTMOV(SCFFRC,   MAXFRC,   MAXGAM)
	CALL FASTMOV(SCFRMI,   REDMIN,   MAXGAM)

        CALL FASTMOV(SCF_OPNID,  SCC_OPNID,  PRM_NUMOPN)          
        CALL FASTMOV(SCF_OPNSEED,SCC_OPNSEED,PRM_NUMOPN)           
        CALL FASTMOV(SCF_OPNDATE,SCC_OPNDATE,PRM_ENDDAT*PRM_NUMOPN)
	CALL FASTMOV(SCF_HLDLIM,SCC_HLDLIM,MAXGAM*3)
	CALL FASTMOV(SCF_HLDDAY,SCC_HLDDAY,MAXGAM*3)

        CALL FASTMOV(SCF_TCPPORTS,TCP_PORTS,   2*2)         
        CALL FASTMOV(SCF_TCPPREFIX,TCP_PREFIX, 2)         
        CALL FASTMOV(SCF_TCPSUFFIX,TCP_SUFFIX, 2*4)         

	CALL FASTMOV(SCFRETCOM,   RETCOM,   NUMPAS)

	HVCLVL = SCFHVL
	HVCRAT = SCFHVR
	TAXLVL = SCFTAL
	TAXRAT = SCFTAR
	COMTAX = SCFCTX
        SCC_IVC = SCFIVC

	RETURN

	END
