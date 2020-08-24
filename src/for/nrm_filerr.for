C
C SUBROUTINE FILERR
C $Log:   GXAFXT:[GOLS]FILERR.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:09:50   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:17:40   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_filerr.for **
C
C FILERR.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C SUBROUTINE TO REPORT FILE I/O ERRORS TO OPERATOR
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
	SUBROUTINE FILERR(NAME,FUN,ST,RECORD)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INTEGER*4 NAME(5), RECORD, ST, FUN
	IF(FUN.EQ.0) WRITE(5,890) IAM(),NAME,ST
	IF(FUN.EQ.1) WRITE(5,900) IAM(),NAME,ST
	IF(FUN.EQ.2) WRITE(5,910) IAM(),NAME,ST,RECORD
	IF(FUN.EQ.3) WRITE(5,920) IAM(),NAME,ST,RECORD
	IF(FUN.EQ.4) WRITE(5,930) IAM(),NAME,ST
	CALL GPAUSE
C
C
890     FORMAT(1X,A,1X,5A4,' handle error > ',I9)
900	FORMAT(1X,A,1X,5A4,' open error > ',I9)
910	FORMAT(1X,A,1X,5A4,' read error > ',I9,' record > ',I5)
920	FORMAT(1X,A,1X,5A4,' write error > ',I9,' record > ',I5)
930	FORMAT(1X,A,1X,5A4,' close error > ',I9)
	END
