C
C SUBROUTINE ENCRPTBF
C $Log:   GXAFXT:[GOLS]ENCRPTBF.FOV  $
C  
C     Rev 1.0   17 Apr 1996 13:04:46   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 16:13:16   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - enc_encsubs.for **
C
C
C	ENC_SUBS.FOR
C
C	V01 WS 7-APR-91
C
C	SOFT ENCRYPTION PROCESSING ROUTINES
CC
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
C       ADDITIONAL ROUTINES ARE "BUILT IN" IN THIS MODULE
C       ENCRPTBF(BUFFER,STATUS)           - PREPARE TO SOFT ENCRYPT BUFFER
C       DECRYPTBF(BUFFER,STATUS)          - PREPARE TO SOFT DECRYPT BUFFER
C       CHKENCPT(BUFFER,ENCRYPTION_TYPE,STATUS) - CHECK IF ANYTHING
C                                             WAS ENCRYPTED OR DECRYPTED
C                                             BY SOFT ENCRYPTION/DECRYPTION
C
C
C***************************************************
C
C       ENCRPTBF(BUFNUM,STATUS)
C       IN:
C       BUFNUM     -    PROCOM BUFFER USED
C       OUT:
C       STATUS     -    0 IF OPERATION "QUEUED TO BE EXECUTED"
C
C       ENCRYPT   BUFFER WITH DATA WITH DATA, THIS SUBROUTINE ONLY STARTS
C       ENCRYPTION PROCESS, ACTUAL ENCRYPTION RESULTS ARE OBTAINED FROM
C       SUBROUTINE CHKENCPT
C
C       FOR SOFT ENCRYPTION
C       BUFFERS TO CRYPT RESIDE ON SOFT_ENCQUE LIST,
C       BUFFERS TO ENCRYPT HAVE BUFFER POINTER POSITIVE
C       BUFFERS TO DECRYPT HAVE BUFFER POINTER NEGATIVE
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE ENCRPTBF(BUFNUM,STATUS)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:ENCCOM.DEF'
C
        INTEGER*4 BUFNUM,STATUS
C
C       COULD CHECK MESSAGE LENGTH HERE, I AM NOT SURE WHAT SHOULD
C       BE DONE AT THAT POINT, POSTPONED
C
        CALL ABL(BUFNUM,SOFT_ENCQUE,STATUS)
        STATUS=0
        RETURN
        END
