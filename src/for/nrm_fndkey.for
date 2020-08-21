C
C SUBROUTINE FNDKEY
C $Log:   GXAFXT:[GOLS]FNDKEY.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:12:20   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:20:08   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_hshsubi4.for **
C
C HSHSUBI4.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 11-NOV-89 MTK SET STATUS TO ZERO IF SAME TUB IN READHASH
C B03 02-OCT-89 TKO  INSTEAD OF CALLING IOINIT TO CHANGE THE SIZE
C                    OF A BUCKET, CALL IOCHGBUK (WALTER MADE ME DO
C                    THIS FOR HIS NEW DISKIO STUFF).
C B02 11-MAR-88 TKO  HANDLE TUB I/O
C                    NOW INCLUDES READHASH, WRITHASH, TUBWRITE
C B01 27-MAY-86 TKO  INITIAL RELEASE
C
C THIS IS A SET OF SUBROUTINES USED BY HASHING PACKAGE
C
C
C
C *** FNDKEY
C
C This will search the current IOBUF for the keys specified
C and will continue reading buffers until either the key is found
C or an empty record is found.
C
C If in direct access mode, it will use READW & WRITEW to access
C the file.  Otherwise, it will use READHASH and WRITHASH.
C
C NOTE THAT MAXOVR IS SET TO THE MAXIMUM # OF BUCKETS TO SEARCH
C BEFORE GIVING UP.
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
	SUBROUTINE FNDKEY(LUN,IOBUF,NEWI4KEY,NEWI2KEY,OLDOFF,OLDLEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:HSHCOM.DEF'
C
	INTEGER*4 LUN                    !  INPUT: LOGICAL UNIT #
	INTEGER*4 IOBUF(*)               !SCRATCH: I/O BUFFER
	INTEGER*4 NEWI4KEY               !  INPUT: I4 KEY TO LOOK FOR
	INTEGER*4 NEWI2KEY               !  INPUT: I2 KEY TO LOOK FOR
	INTEGER*4 OLDOFF                 ! OUTPUT: OFFSET OF RECORD
C                                      ;         CONTAINING KEY OR
C                                      ;         EMPTY RECORD
	INTEGER*4 OLDLEN                 ! OUTPUT: >0=FOUND, LEN=OLDLEN
C                                      ;         =0=EMPTY FOUND
C                                      ;         -1=FILE FULL
C
	INTEGER*4 NEWBLK, STATUS, LENREC, NUMREC, TUBOFF, K, I4LOF
	INTEGER*4 I2HLF, I2OFF, I4OFF
C
	INTEGER*4  MAXOVR
	PARAMETER (MAXOVR=5)
C
C
	INTEGER*4 I4TEMP
	INTEGER*2 I2TEMP(2)
	EQUIVALENCE (I4TEMP,I2TEMP)
C
C
C
C
	I4OFF=FCB(FCBI4K,LUN)
	I2OFF=FCB(FCBI2K,LUN)
	I2HLF=FCB(FCBI2H,LUN)
	I4LOF=FCB(FCBLOF,LUN)
C
C
	DO 2500 K=1,MAXOVR+1
	  TUBOFF=FCB(FCBTUBOFF,LUN)
	  OLDOFF=FCB(FCBOFF,LUN)
C
1000	  CONTINUE
	  IF(IOBUF(TUBOFF+OLDOFF+I4OFF).EQ.0)THEN
	    OLDLEN=0
	    GO TO 9000
	  ENDIF
C
	  NUMREC=ISHFT(IOBUF(TUBOFF+OLDOFF+I4OFF),-30)
	  LENREC=FCB(FCBSZ1+NUMREC,LUN)
	  IF(IAND(IOBUF(TUBOFF+OLDOFF+I4OFF),INDMSK).EQ.NEWI4KEY)THEN
	    I4TEMP=IOBUF(TUBOFF+OLDOFF+I2OFF)
	    IF(I2TEMP(I2HLF).EQ.NEWI2KEY)THEN
	      OLDLEN=LENREC
	      GO TO 9000
	    ENDIF
	  ENDIF
C
	  OLDOFF=OLDOFF+LENREC
	  IF(OLDOFF.LE.I4LOF)GO TO 1000
C
	  IF(K.EQ.MAXOVR+1)GO TO 2500
	  IF(FCB(FCBCHG,LUN).NE.0)THEN
	    IF(FCB(FCBMOD,LUN).EQ.DIRMOD)THEN
	      CALL WRITEW(FCB(FCBFDB,LUN),FCB(FCBBLK,LUN),IOBUF,STATUS)
	    ELSE
	      CALL WRITHASH(LUN,FCB(FCBBLK,LUN),IOBUF,STATUS)
	    ENDIF
	    IF(STATUS.NE.0)THEN
	      OLDLEN=-1
	      GO TO 9000
	    ENDIF
	    FCB(FCBCHG,LUN)=0
	  ENDIF
	  NEWBLK=FCB(FCBBLK,LUN)+1
	  IF(NEWBLK.GT.FCB(FCBHSH,LUN))NEWBLK=1
	  FCB(FCBBLK,LUN)=0
	  IF(FCB(FCBMOD,LUN).EQ.DIRMOD)THEN
	    CALL READW(FCB(FCBFDB,LUN),NEWBLK,IOBUF,STATUS)
	  ELSE
	    CALL READHASH(LUN,NEWBLK,IOBUF,STATUS)
	  ENDIF
	  IF(STATUS.NE.0)THEN
	    OLDLEN=-1
	    GO TO 9000
	  ENDIF
	  FCB(FCBBLK,LUN)=NEWBLK
	  FCB(FCBOFF,LUN)=0
C
2500	CONTINUE
	OLDLEN=-1
	GO TO 9000
C
C
9000	CONTINUE
	RETURN
	END
