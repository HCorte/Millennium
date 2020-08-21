C
C SUBROUTINE CHKASN
C $Log:   GXAFXT:[GOLS]CHKASN.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:31:30   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:48:36   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_getfinfo.for **
C
C NRM_GETFINFO.FOR
C
C V02 13-MAR-91 TKO Always subtract 1 from # of sectors because 1st sector is
C                   not used.
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C
C
C This source contains various subroutines to obtain information about
C a file already assigned to a logical unit and a routine to see if
C a file is already assigned.
C
C
C Calling sequences:
C
C     In each case, LUN is the logical unit
C
C     CALL CHKASN(LUN,ST)
C                     ST  = 0: LUN IS NOT ASSIGNED
C                         =-1: LUN IS ASSIGNED
C
C     CALL GETSIZ(LUN,SIZE)
C         OR
C     CALL GETFSIZ(LUN,SIZE)
C                      SIZE=# OF SECTORS OR RECORDS
C
C     CALL GETFTYP(LUN,TYPE)
C                      TYPE=0   IF CONTIGUOUS
C                           2   IF INDEXED
C                           255 IF NULL
C
C
C     CALL GETFLEN(LUN,RLEN)
C                      RLEN=LOGICAL RECORD LENGTH (0 IF CONTIGUOUS)
C
C
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
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE CHKASN(LUN, ST)
	IMPLICIT NONE
C
	INTEGER*4   LUN
	INTEGER*4   ST
C
	LOGICAL	    ISTHERE
C
C
	INQUIRE(LUN, OPENED=ISTHERE)
	IF(ISTHERE)THEN
	  ST = -1
	ELSE
	  ST = 0
	ENDIF
	RETURN
	END
