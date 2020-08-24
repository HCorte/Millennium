C BLOCK DATA APPCOM
C  
C V10 19-DEC-2000 CS  ADDED PASCOM
C V09 28-NOV-2000 JHR Added TGLCOM
C V08 05-JUN-2000 OXK Added SSOCOM
C V07 06-NOV-1995 HXK Doubled record length for Double-Couple project
C V06 15-OCT-1994 HXK Adding /developing Bingo (15.Oct.94)
C V05 02-OCT-1994 HXK Added Bingo
C V04 07-JUN-1993 HXK Added V65COM.DEF (RAVI GAME COMMON)
C V03 03-JUN-1993 HXK Spede Game common (PPPCOM.DEF) added.
C V02 21-JAN-1990 DAB Initial Release Based on Netherlands Bible, 12/92,
C     			  and Comm 1/93 update DEC Baseline
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C APPCOM.FOR
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
        BLOCK DATA APPCOM
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSDEFINE.DEF'
C
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PRMHSH.DEF'
        INCLUDE 'INCLIB:PRMPRO.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
C
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:ENCCOM.DEF'
        INCLUDE 'INCLIB:KIKCOM.DEF'
        INCLUDE 'INCLIB:LOGCOM.DEF'
        INCLUDE 'INCLIB:LTOCOM.DEF'
        INCLUDE 'INCLIB:LSYSCOM.DEF'
        INCLUDE 'INCLIB:NBRCOM.DEF'
        INCLUDE 'INCLIB:QUECOM.DEF'
        INCLUDE 'INCLIB:SPTCOM.DEF'
        INCLUDE 'INCLIB:SCRCOM.DEF'
        INCLUDE 'INCLIB:WITCOM.DEF'
        INCLUDE 'INCLIB:TSLCOM.DEF'
        INCLUDE 'INCLIB:BNGCOM.DEF'
        INCLUDE 'INCLIB:DBLCOM.DEF'
        INCLUDE 'INCLIB:CPLCOM.DEF'
        INCLUDE 'INCLIB:SSCCOM.DEF'
        INCLUDE 'INCLIB:SSPCOM.DEF'
        INCLUDE 'INCLIB:TRPCOM.DEF'
        INCLUDE 'INCLIB:TROCOM.DEF'
        INCLUDE 'INCLIB:STRCOM.DEF'
        INCLUDE 'INCLIB:STROCOM.DEF'
        INCLUDE 'INCLIB:SSOCOM.DEF'
        INCLUDE 'INCLIB:TGLCOM.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'
C
        END
