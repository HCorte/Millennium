C
C $Log:   GXAFXT:[GOLS]AGTSUP.FOV  
C  
C     Rev 1.0   17 Apr 1996 12:09:28   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.1   07 Nov 1993 22:22:26   GXA
C  Added Ticket Message Enabeling/Disabeling and Operation State Ena/Dis.
C  
C     Rev 1.0   07 Nov 1993  4:27:50   GXA
C  Initial revision.
C
C Program to Enable / Disable Agent Global Wagering,Cancellations,Validations.
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
C Copyright 1993 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        PROGRAM AGTSUP
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:RECAGT.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
C
C
	INTEGER*4	TER		!Loop Variable
	INTEGER*4	STER		!Starting Terminal Number.
	INTEGER*4	ETER		!Ending Terminal Number.
	INTEGER*4	WSUP		!Wagering Supression Flag.
	INTEGER*4	CSUP		!Cancellation Supression Flag.
	INTEGER*4	VSUP		!Validation Supression Flag.
	INTEGER*4	TSUP		!Ticket message Supression Flag.
	INTEGER*4	SSUP		!Signon Flag.
	INTEGER*4	ST		!Subroutine Return Status.
	INTEGER*4	FDB(7)		!File Descriptor block for ASF.
	INTEGER*4	BITMAP		!Agent Bitmap
	INTEGER*4	MBITMAP		!Agent Bitmap in Memory
	INTEGER*4	ANS		!Answer Flag.
C
	CHARACTER*9	STATE(0:1)	!Enable/Disable State
	CHARACTER*9	SON_STATE(0:1)	!Signon State
C
	DATA		STATE/'Active   ','Supressed'/
	DATA	    SON_STATE/'Signon   ','Signoff  '/
C
C
	TYPE*,IAM(),
     *  '<<<<< AGTSUP V01  Global Agent Wag/Can/Val Supression >>>>>'
	TYPE*,IAM()
C
C GET SYSTEM CONFIGURATION INFO.
C
	CALL GETSCONF(SCFREC,ST)
	IF(ST.NE.0) THEN
	   TYPE*,IAM(),'Unable to get System Configuration info.'
	   CALL GSTOP(GEXIT_FATAL)
	ENDIF
C
C GET PROGRAM OPTIONS
C
10	CONTINUE
	TYPE*,IAM(),'This Program will change the Supression for a range'
        TYPE*,IAM(),'of terminals, in Memory and in the ASF'
	TYPE*,IAM(),'NOTE! Memory is updated on this system ONLY!!'
	TYPE*,IAM()
	TYPE*,IAM()
	TYPE*,IAM(),'Enter Range of Terminal #''s to affect with change'
	TYPE*,IAM(),'or ''E'' to Exit '
	TYPE*,IAM()
	CALL INPNUM('Enter Starting Terminal #: ',STER,1,NUMAGT,ST)
	IF(ST.NE.0) CALL GSTOP(GEXIT_OPABORT)
	CALL INPNUM('Enter Ending Terminal #  : ',ETER,STER,NUMAGT,ST)
	IF(ST.NE.0) CALL GSTOP(GEXIT_OPABORT)
	TYPE*,IAM()
C
	TYPE*,IAM()
	TYPE*,IAM(),
     *  'Enter ''0'' to ACTIVATE or ''1'' to SUPRESS following functions'
	TYPE*,IAM()
	CALL INPNUM('Enter WAGERING     Supression Flag: ',WSUP,0,1,ST)
	CALL INPNUM('Enter CANCELLATION Supression Flag: ',CSUP,0,1,ST)
	CALL INPNUM('Enter VALIDATION   Supression Flag: ',VSUP,0,1,ST)
	CALL INPNUM('Enter TICKET MSG   Supression Flag: ',TSUP,0,1,ST)
	CALL INPNUM('Enter SIGNON                  Flag: ',SSUP,0,1,ST)
C
C DISPLAY PICKED VALUES
C
	TYPE*,IAM()
	TYPE*,IAM(),'Values Entered for Terminal ',STER,' to ',ETER
	TYPE*,IAM()
	WRITE(5,900) IAM(),STATE(WSUP)
	WRITE(5,901) IAM(),STATE(CSUP)
	WRITE(5,902) IAM(),STATE(VSUP)
	WRITE(5,903) IAM(),STATE(TSUP)
	WRITE(5,904) IAM(),SON_STATE(SSUP)
	TYPE*,IAM()
	CALL WIMG(5,'Is this correct Y/N ')
	CALL YESNO(ANS)
	IF(ANS.NE.1) GOTO 10
	
C
C OPEN THE AGENT SALES FILE
C
	CALL OPENW(ASF,SFNAMES(1,ASF),4,0,0,ST)
	CALL IOINIT(FDB,ASF,ASFSEC*256)
	IF(ST.NE.0) CALL FILERR(SFNAMES(1,ASF),1,ST,0)
C
C LOOP FOR SELECTED RANGE OF TERMINALS
C
	DO TER = STER,ETER
	   CALL READW(FDB,TER,ASFREC,ST)
	   IF(ST.NE.0) CALL FILERR(SFNAMES(1,ASF),2,ST,TER)
C
	   CALL ASCBIN(ASFINF,STTYP,LTTYP,BITMAP,ST)
	   IF(ST.NE.0) THEN
	      TYPE*,'Agent Type Conversion error for Terminal: ',TER
	      CALL GPAUSE
	   ENDIF
	   MBITMAP = AGTTAB(AGTTYP,TER)
C
	   IF(WSUP.EQ.1) THEN
	      CALL BSET(BITMAP,AGTWAG)
	      CALL BSET(MBITMAP,AGTWAG)
	   ELSE
	      CALL BCLR(BITMAP,AGTWAG)
	      CALL BCLR(MBITMAP,AGTWAG)
	   ENDIF
C
	   IF(CSUP.EQ.1) THEN
	      CALL BSET(BITMAP,AGTCAN)
	      CALL BSET(MBITMAP,AGTCAN)
	   ELSE
	      CALL BCLR(BITMAP,AGTCAN)
	      CALL BCLR(MBITMAP,AGTCAN)
	   ENDIF
C
	   IF(VSUP.EQ.1) THEN
	      CALL BSET(BITMAP,AGTVAL)
	      CALL BSET(MBITMAP,AGTVAL)
	   ELSE
	      CALL BCLR(BITMAP,AGTVAL)
	      CALL BCLR(MBITMAP,AGTVAL)
	   ENDIF
C
C THIS ONE WORKS IN A SLIGHTLY DIFFERENT WAY
C
	   IF(TSUP.EQ.0) THEN
	      CALL BSET(BITMAP,AGTTKM)
	      CALL BSET(MBITMAP,AGTTKM)
	   ELSE
	      CALL BCLR(BITMAP,AGTTKM)
	      CALL BCLR(MBITMAP,AGTTKM)
	   ENDIF
C
	   IF(SSUP.EQ.0) THEN
	      AGTHTB(AOPSTS,TER) = SIGNON
	   ELSE
	      AGTHTB(AOPSTS,TER) = SIGNOF
	   ENDIF
C
           CALL BINASC(ASFINF,STTYP,LTTYP,BITMAP)
	   AGTTAB(AGTTYP,TER) = MBITMAP
C
C WRITE RECORD BACK
C
	   CALL WRITEW(FDB,TER,ASFREC,ST)
	   IF(ST.NE.0) CALL FILERR(SFNAMES(1,ASF),3,ST,TER)
C
	END DO
C
	TYPE*,IAM(),'Update Completed for Terminals ',STER,' to ',ETER
C
C
900	FORMAT(1X,A,'Wagering        will be ',A9)
901	FORMAT(1X,A,'Cancellations   will be ',A9)
902	FORMAT(1X,A,'Validations     will be ',A9)
903	FORMAT(1X,A,'Ticket Msg      will be ',A9)
904	FORMAT(1X,A,'Agent OPS State will be ',A9)
	END
