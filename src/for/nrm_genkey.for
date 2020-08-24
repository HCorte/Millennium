C
C SUBROUTINE GENKEY
C $Log:   GXAFXT:[GOLS]GENKEY.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:18:00   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:23:46   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_scram.for **
C
C SCRAM.FOR
C
C V01 12-DEC-90 XXX RELEASED FOR VAX
C
C	* * * * *  C O N F I D E N T I A L  * * * * *
C	* * * * *  C O N F I D E N T I A L  * * * * *
C	* * * * *  C O N F I D E N T I A L  * * * * *
C	* * * * *  C O N F I D E N T I A L  * * * * *
C
C This is the fortran version of the traditional scrambler used in Denmark.
C
C
C *** If you need to change the scrambling algorithm to go from system to
C     system, you should change the values in SYSID1, SYSID2, and SYSIDC.
C	SYSID1, SYSID2 will cause a change in the external serial number
C	SYSIDC will cause a change in the checkdigits only
C
C This version was written by T. Oram from SCRAM.MAC in use in Denmark on
C 12/12/90.  The previous version (by Jim Bray) was taken from the PDP-11 code
C last modified by Walter Szrek on 4/22/85.  The original version was written
C by Mike Tessarowicz long long ago.
C
C
C As with all previous versions, this version produces an external serial #
C which is 3 bytes long and checkdigits which are in 1 byte.  Older versions,
C however, produced only 7 digit external numbers (23 bits: 0 - 8388607) and
C a 2 digit checksum (0-99).  This version produces an 8 digit external number
C (24 bits: 0 - 16777215) and a 3 digit checksum (8 bits: 0-255).
C
C This version will convert a 27 bit internal number into an external number
C by scrambling the low order 24 bits of the internal number and including the
C high order 3 bits of the internal serial number in the checksum (which is
C also scrambled).  The internal serial # must be in the following range:
C	0 - 134,217,727 (i.e., 27 bits)
C
C
C        THE NEW ALGORITHM WILL LOOK AS BELOW
C
C        EXTERNAL SERIAL #:
C        BIT 0-7 - SET TO 0
C        BIT 8 - SAME AS BIT 0 OF INTERNAL SERIAL #
C        BIT 9-31 - SAME AS OLD ALGORITHM BIT 9-31
C
C        LOW BYTE OF CHECKSUM:
C        BIT 0-2 - PARITY OF EVEN,ODD BITS AND EVERY OTHER 2 BIT GROUP
C        BIT 3-4 - 2 LOW BITS OF CDC
C        BIT 5-7 - SAME AS BIT 5-7 OF INTERNAL SERIAL #
C	 (the checksum is then scrambled using MIXCHK)
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
C
C
C *** GENKEY
C
C This is an internal subroutine and is never called directly by another
C program. It is called with a cdc-date and returns 2 values to be used as
C a key for scrambling plus the value of SYSIDC.
C
C CALLING SEQUENCE:
C
C CALL GENKEY( CDC, CDCH, CDCL, SYSIDX)
C
C INPUT:
C	CDC	    CDC date for the ticket
C
C OUTPUT:
C  CDCH	    High part of scrambling key
C  CDCL	    Low  part of scrambling key
C  SYSIDX    Value of the parameter SYSIDC
C
C SUBROUTINES CALLED:
C        None.
C
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GENKEY(CDC, CDCH, CDCL, SYSIDX)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
C
	INTEGER*4  CDC           ! INPUT: CDC DATE
	INTEGER*4  CDCH          !OUTPUT: CDC HIGH KEY
	INTEGER*4  CDCL          !OUTPUT: CDC LOW  KEY
	INTEGER*4  SYSIDX        !OUTPUT: VALUE OF SYSIDC
C
	INTEGER*4  SYSID1        !SITE SPECIFIC KEY 1
	PARAMETER (SYSID1 = 143)
C
	INTEGER*4  SYSID2        !SITE SPECIFIC KEY 2
	PARAMETER (SYSID2 = 914)
C
	INTEGER*4  SYSIDC        !SITE SPECIFIC KEY 3 (0-255)
	PARAMETER (SYSIDC = 25)
C
	CDCL = IOR( CDC, '0000FC00'X) !HIGH 6 BITS
C
	CDCH = CDC
	IF( IAND( CDCL, '01'X).NE.0)THEN      !IF ODD...
	  CDCH = IOR( CDCH, '00000040'X)      !  SET THIS BIT
	ENDIF
C
	CDCL = CDCL + SYSID1
	CDCH = CDCH + SYSID2
	CDCH = IAND( CDCH, '0000007F'X)       !LAST 7 BITS
	CDCL = IAND( CDCL, '0000FFFF'X)       !LAST 16 BITS
C
	SYSIDX = SYSIDC
	RETURN
	END
