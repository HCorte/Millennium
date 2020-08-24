C CHCKDIS.FOR
C
C V01 01-JUN-94 TJS INITIAL RELEASE FOR POLAND
C
C
C SUBROUTINE TO CHECK IF DISPATCHER IS ACTIVE; FOLLOWS CHKDIS PROGRAM
C
C IT CHECKS IF THE PROCESS NAMED: "nnnnDISPAT" (DISPATCHER),
C WHERE nnnn IS 4-CHARACTER PROJECT PREFIX NAME IS EXISTENT
C
C RETURNING VALUE ST=0 DISPAT EXISTS
C		    =1 DISPAT DOES NOT EXIST
C		    =2 OTHER ERROR
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
       SUBROUTINE CHCKDIS(ST)      
       IMPLICIT NONE
C
       INCLUDE '($JPIDEF)'
       INCLUDE '($SSDEF)' 
       INCLUDE '($SYSSRVNAM)'
       INCLUDE 'INCLIB:SYSDEFINE.DEF'
       INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
       INTEGER*4 I4DISNAM
       INTEGER*4 ITEMLIST(4), ST, STATUS, STAT, LEN, PRLEN
C
       INTEGER*2 ITEM2LIS(8)
C
       CHARACTER*10 PROC
       CHARACTER*1  PNAM(10)
       CHARACTER*6  DISNAM
C
       DATA DISNAM/'DISPAT'/
C
       EQUIVALENCE (PROC,PNAM(1))
       EQUIVALENCE (DISNAM,PNAM(5))
       EQUIVALENCE (PROC,I4DISNAM)
       EQUIVALENCE (ITEMLIST,ITEM2LIS)
C
C FIND 4-CHARACTER PREFIX PROJECT NAME (nnnn)
C
       CALL GETPRFX(I4DISNAM, PRLEN)
       IF(PRLEN.NE.4) THEN
	 ST=2
	 RETURN
       ENDIF
C
C CHECK IF THE PROCESS: "nnnnDISPAT" IS EXISTENT 
C
       ST=0
       LEN=0
C
       ITEM2LIS(1)=4      
       ITEM2LIS(2)=JPI$_STATE
       ITEMLIST(2)=%LOC(STAT)
       ITEMLIST(3)=%LOC(LEN)
       ITEMLIST(4)=0
       STATUS=SYS$GETJPIW( ,,PROC,ITEMLIST,,,)
C
C IF "nnnnDISPAT" NOT ACTIVE SET RETURN VALUE
C
       IF(.NOT.STATUS) ST=1
       RETURN
C
       END	
