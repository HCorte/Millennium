C===============================================================================
C
C NRM_IDXSUBS.FOR
C
C V01 29-JAN-2001 ANG      INITIAL RELEASE FOR PORTUGAL
C
C Routines to handle direct access files.
C
C	FIDX_OPEN     ( FDB, FNAME, RECSIZ, STOPEN, ST )
C       FIDX_READ     ( FDB, TICKET, RECSIZ, REC, ST )
C       FIDX_WRITE    ( FDB, TICKET, RECSIZ, REC, ST )
C       FIDX_CLOSE    ( FDB, ST )
C
C===============================================================================
C This item is the property of GTech Corporation, Providence, Rhode Island, and
C contains confidential and trade secret information. It may not be transferred
C from the custody or control of GTech except as authorized in writing by an
C officer of GTech. Neither this item nor the information it contains may be
C used, transferred, reproduced, published, or disclosed, in whole or in part,
C and directly or indirectly, except as expressly authorized by an officer of
C GTech, pursuant to written agreement.
C
C Copyright (c)1999 GTech Corporation. All rights reserved.
C===============================================================================
C
C
C===============================================================================
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE FIDX_OPEN( FDB, FNAME, RECSIZ, STOPEN, ST )
        IMPLICIT NONE

	INCLUDE		'INCLIB:SYSPARAM.DEF'
	INCLUDE		'INCLIB:SYSEXTRN.DEF'
        INCLUDE         'INCLIB:GLOBAL.DEF'
        INCLUDE         'INCLIB:CONCOM.DEF'
        INCLUDE		'INCLIB:IDXPRM.DEF'
C
C ROUTINE PARAMETERS
C
        INTEGER*4       FDB(*), RECSIZ, ST
	CHARACTER*(*)	FNAME, STOPEN

        INTEGER*4   DISKOPEN
        EXTERNAL    DISKOPEN
C
C Open file according to mode
C
	IF( RECSIZ.LE.0 ) THEN
	    ST = FIDX_FPE
	ELSE
	    FDB(FDB_IDXBYTSZ) = RECSIZ	    !SIZE TO FORMATTED FILE FORM

      	    CALL FIND_AVAILABLE_LUN( FDB(FDB_IDXLUN), ST )

	    IF( ST.EQ.0 ) THEN
	        OPEN( UNIT         = FDB(FDB_IDXLUN),
     *	              FILE         = FNAME,
     *		      ORGANIZATION = 'SEQUENTIAL',
     *		      ACCESS       = 'DIRECT', SHARED,
     *		      FORM         = 'FORMATTED',
     *		      RECORDTYPE   = 'FIXED',
     *		      STATUS       = STOPEN,
     *                RECL         = FDB(FDB_IDXBYTSZ),
     *		      IOSTAT       = FDB(FDB_IDXSTAT) )

      		ST = FDB(FDB_IDXSTAT)
	    ENDIF
	ENDIF

        RETURN
        END
C
C===============================================================================
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE FIDX_READ( FDB, TICKET, RECSIZ, REC, ST )
        IMPLICIT NONE

	INCLUDE		'INCLIB:SYSPARAM.DEF'
	INCLUDE		'INCLIB:SYSEXTRN.DEF'
        INCLUDE		'INCLIB:IDXPRM.DEF'
        INCLUDE		'INCLIB:STANDARD.DEF'

        INTEGER*4       FDB(*), RECSIZ, TICKET, ST, IND
	BYTE		REC(RECSIZ)
C
C Test parameter record size
C
	IF( FDB(FDB_IDXBYTSZ) .NE. RECSIZ) THEN
	    ST = FIDX_FPE
	ELSE
C
C Read record from file using provided key value (sequence number)
C (we assume file is open in correct mode; if not, RMS/VMS will take care...)
C
C Add 1 to TICKET, because ticket number starts with 0. 
C
	    READ( UNIT=FDB(FDB_IDXLUN), FMT='(<RECSIZ>A1)', REC=TICKET+1,
     *            IOSTAT=FDB(FDB_IDXSTAT) ) (REC(IND),IND=1,RECSIZ)
	    ST = FDB(FDB_IDXSTAT)
	ENDIF

        RETURN
        END
C
C===============================================================================
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE FIDX_WRITE( FDB, TICKET, RECSIZ, REC, ST )
        IMPLICIT NONE

	INCLUDE		'INCLIB:SYSPARAM.DEF'
	INCLUDE		'INCLIB:SYSEXTRN.DEF'
        INCLUDE		'INCLIB:IDXPRM.DEF'

        INTEGER*4       FDB(*), RECSIZ, ST, IND, TICKET
	BYTE		REC(RECSIZ)
C
C Test parameter record size
C
	IF( FDB(FDB_IDXBYTSZ) .NE. RECSIZ) THEN
	    ST = FIDX_FPE
	ELSE
C
C Write record to file
C (we assume file is open in correct mode; if not, RMS/VMS will take care...)
C
	    WRITE( UNIT=FDB(FDB_IDXLUN), FMT='(<RECSIZ>A1)', REC=TICKET+1,
     *             IOSTAT=FDB(FDB_IDXSTAT) ) (REC(IND),IND=1,RECSIZ)
	    ST = FDB(FDB_IDXSTAT)
	ENDIF

        RETURN
        END
C
C===============================================================================
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE FIDX_CLOSE( FDB, ST )
        IMPLICIT NONE

	INCLUDE		'INCLIB:SYSPARAM.DEF'
	INCLUDE		'INCLIB:SYSEXTRN.DEF'
        INCLUDE		'INCLIB:IDXPRM.DEF'

        INTEGER*4       FDB(*), ST

        CLOSE( UNIT=FDB(FDB_IDXLUN), IOSTAT=FDB(FDB_IDXSTAT) )
        ST = FDB(FDB_IDXSTAT)

        RETURN
        END
