C
C SUBROUTINE PRMNUM
C $Log:   GXAFXT:[GOLS]PRMNUM.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:31:20   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 17:21:56   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_inpmod.for **
C
C INPMOD.FOR
C
C V04 05-AUG-91 WLM Added in XXXNUM call to IMONY function for converting 
C		    substring containing money amount to count of money 
C		    units (BETUNIT)
C V03 25-APR-91 TKO Added a PRM... entry point for each routine (see below)
C V02 11-MAR-91 JPJ INITIAL RELEASE FOR MARYLAND
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C
C This is a set of routines to ask for and receive data from a user.  Each
C routine has the form INP.... which will cause the routine to write a prompt
C string and read the input.
C
C As of V03, I have added entry points called PRM... for each of these routines
C When you call PRM... instead of INP..., the routine will call PRMTEXT rather
C than WIMG to get the input.  PRMTEXT will try to determine whether or not
C we are running as a subprocess and will either perform an normal WIMG prompt
C or will prompt and receive an answer via a MAILBOX.
C
C PRMNUM or
C INPNUM - INPUT NUMBERIC DATA
C	    *V04* ACCEPT $ SIGN TO MARK MONIES ENTRY
C
C CALLING SEQUENCE
C     CALL INPNUM(STRING,NUM,LOW,HIGH,EXT)
C
C INPUT
C     STRING - CHARACTER STRING
C     HIGH   - UPPER LIMIT
C     LOW    - LOWER LIMIT
C OUTPUT
C     NUM    - NUMBER
C     EXT    - EXIT FLAG (EXIT = -1)
C
C
C INPDAT - INPUT DATE ROUTINE.
C
C CALLING SEQUENCE:
C     CALL INPDAT(CDC,EXT)
C INPUT
C     NONE
C OUTPUT
C     CDC    - CDC DATE.
C     EXT    - EXIT FLAG.
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
	SUBROUTINE PRMNUM(STRING,NUM,LOW,HIGH,EXT)
	IMPLICIT NONE
C
	CHARACTER   STRING*(*)
	INTEGER*4   NUM,LOW,HIGH,EXT
C
	CALL XXXNUM(.TRUE., STRING,NUM,LOW,HIGH,EXT)
	RETURN
	END
