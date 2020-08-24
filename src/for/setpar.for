C
C SUBROUTINE SETPAR
C
C SETPAR.FOR
C 
C V11 12-MAR-2003 GPW DESENCR P(NUMENC)=1  ADDED
C V10 25-JUL-2000 UXN MESLOG removed.
C V09 13-MAY-1999 UXN STRTIM CHANGED TO STARTIM
C V08 18-MAR-1997 RXK By default flush time is not used, set to 0  
C V07 05-FEB-1997 WPW Set GVTFLG to 0.
C V06 30-JAN-1997 WPW Remove BCHSIZ,VALPRNT,VALPAM and ACTPRNT as they are 
C                     set in BLDSYS.
C V05 17-APR-1996 HXK Release of Finland for X.25, Telephone Betting, 
C                     Instant Pass Thru Phase 1
C V04 16-JUN-1993 SXH Released for Finland
C V03 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V02 07-OCT-1991 MTK INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C
C SUBROUTINE TO SET SYSTEM PARAMETERS.
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
	SUBROUTINE SETPAR
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
C
	P(NXTTRA)  = 1
	P(SYSSTS)  = SYSACT         !SET SYSTEM ACTIVE
	P(LOGBLO)  = 125            !LOGGER BLOCKING LIMIT
	P(MAXTRA)  = 500            !MAXIMUM BATCH LIMIT
C       P(CHKTIM)  = 4800           !CHECKPOINT EVERY 10 MINUTES
	P(LOGTIM)  = 1              !KICK LOGGER EVERY 1/8 SEC
	P(DPTTIM)  = 1              !KICK DISPATCHER EVERY 1/8 SEC
	P(MAXMES)  = 50             !MAXIMUM NUMBER OF QUEUED MESS.
	P(ACTTIM)  = 0              !START TIME
	P(CLRKACT) = 0              !CLERK ACCOUNTING ENABLED
	P(POOLACT) = 0              !LOTTO POOLS ON THIS SYSTEM
	P(SUPPUD)  = 0              !LOTTO POOLS ENABLED
	P(DESACT)  = 0              ! DES ENCRYPTION ON THIS SYSTEM
	P(XXDEBUG) = 1              !DEBUG DISABLED
C
        P(MAXCRS)=100           !MAX CRSPRO TRANSACTIONS
        P(MAXINSTIM)=20         !TIME OUT CRSPRO
        P(SUPWEL)=0             !ENABLE WELL BOARD ID CHECK
        P(GVTFLG)='00000000'X   !BIT MAP OF WHAT TO DO IF INCORRECT REV
CPXN    P(GVTSUP)=10            !WAIT 10 MINUTES WHEN SUPPRESSED
        P(GVTGPL)=10            !WAIT 10 MINUTES WHEN NOT CONNECTED
CPXN    P(GVTRST)=0             !RESET GVT AT MIDNIGHT
CPXN    P(GVTDFL)=0             !BIT MAP OF GVT ESTABLISHMENT
        P(GVTIVL)=20            !GVT VALIDATION MAXIMUM
        P(MAXDWN)=80            !MAX GVT DOWNLOADS (80 ports)
        P(DWNTIM)=1200          !DOWNLOAD TIME IN SECONDS (20 MINUTES)
        P(X2XIDX)=1             !SET PTL INDEX
C
        P(STARTIM)=21600        ! GVT START TIME (06:00)
        P(ENDTIM)=72000         ! GVT END TIME   (20:00)
CRXK    P(FLSTIM)=68400         ! GVT FLUSH TIME (19:00)
        P(FLSTIM)=0             ! GVT FLUSH TIME
        P(GVTBMP)=0             !GVT BITMAP FLAG (SIGNON)
CPXN	P(GVTAUTH)=0            !GVT 8 DIGIT AUTHORIZATION (SIGNON)
CWXW    P(VALPRNT)=0            ! GVT Validation print parameter
CWXW    P(ACTPRNT)=1            ! GVT Activation print parameter
        P(SETPRNT)=0            ! GVT Settlement print parameter
C                               ! (not used in Belgium)
        P(ISSPRNT)=2            ! GVT Issuance print parameter
        P(RETPRNT)=2            ! GVT Returns print parameter
        P(DELPRNT)=1            ! GVT Confirm (delivery) print parameter
C
CWXW    P(VALPAMT)=0            ! GVT validation print amount
CWXW    P(BCHSIZ)=28            ! GVT validation batch size
C
        P(NUMENC)=1             !NUMBER OF ENCPRO TASKS !V11
	RETURN

	END
