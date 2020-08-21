C
C BLOCK DATA REQCOM
C $Log:   GXAFXT:[GOLS]REQCOM.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:42:52   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   19 Jun 1993 13:29:08   HXK
C  Initial revision.
C  
C     Rev 1.1   03 Feb 1993 14:00:36   EBD
C  Removed include of sysparam.def
C  
C     Rev 1.0   21 Jan 1993 17:29:14   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - reqcom.for **
C
C REQCOM.FOR
C
C V02 23-OCT-91 GCAN INITIAL RELEASE FOR THE NETHERLANDS
C V01 01-AUG-90 XXX  RELEASED FOR VAX
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	BLOCK DATA REQCOMM
	IMPLICIT NONE 
C
	INCLUDE 'INCLIB:SYSDEFINE.DEF'
C
C NAME: REQCOM.FOR
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:REQCOM.DEF'
	END
