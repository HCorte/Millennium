C This program is for testing porposes only. If it is compiled in DEBUG
C mode, it allows you to look into APPCOM.
C
C V05 05-JUN-2000 OXK SSOCOM ADDED
C V05 06-APR-2000 OXK STACOM ADDED
C V04 10-MAR-2000 OXK PRZCOM & RWFCOM ADDED
C V03 25-NOV-1999 OXK World Tour added.
C V02 17-MAY-1999 UXN Super Triple added.
C V01 17-JUL-1998 UXN Initial release.
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
C Copyright 1998 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
	PROGRAM SPY
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSDEFINE.DEF'
C
	INCLUDE 'INCLIB:MSGCOM.DEF'
        INCLUDE 'INCLIB:PRMHSH.DEF'
        INCLUDE 'INCLIB:PRMPRO.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
C
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:LTOCOM.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'
        INCLUDE 'INCLIB:QUECOM.DEF'
        INCLUDE 'INCLIB:SPTCOM.DEF'
        INCLUDE 'INCLIB:TGLCOM.DEF'
	INCLUDE 'INCLIB:STOPCOM.DEF'
	INCLUDE 'INCLIB:WINCOM.DEF'
	INCLUDE 'INCLIB:PRZCOM.DEF'
	INCLUDE 'INCLIB:KIKCOM.DEF'
C
	TYPE*,'I am a spy....'
C
	END
