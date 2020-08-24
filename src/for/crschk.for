C  GXSRC:CRSCHK.FOR
C  
C  $Log:   GXAFIP:[GOLS]CRSCHK.FOV  $
C  
C     Rev 1.1   28 Jan 1997 19:27:10   RXK
C  Message numbers for Errlog changed 
C  
C     Rev 1.0   17 Apr 1996 12:43:50   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.3   26 Oct 1994  7:16:24   MCM
C  CORRECT TIME-OUT ERROR BETWEEN GOLS AND IPS (UNRELEASED INTERNAL)
C  
C     Rev 1.2   01 Sep 1994 18:07:00   MCM
C  SWAPPING BYTES IS NO LONGER NECESSARY FOR THE DEC LMS
C  
C     Rev 1.1   03 Jan 1994 20:03:22   SYSTEM
C  Applying PVCS header for automatic revision history
C  
C     Rev 1.0    21 Dec 1993 17:33:58   SYSTEM
C  Initial revision.
C
C
C
C V02 18-AUG-92 NJA ADDED (CHECKWRITER)
C V01 18-NOV-91 KWP INITIAL RELEASE FOR VAX
C
C This subroutine will only be called when no transmit buffers
C are available, and the buffer will be requeued back to
C CRSPRO'S queue.  A count is then assigned to the buffer, and
C if the max number of retry times is encounted, the transaction
C is timed out.
C
C Input parameters:
C
C   PBUF   Int*4   Procom buffer number from CRSPRO
C
C Output parameters:
C
C   None
C
C
C COPYRITF.DEF+++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C COPYRIGHT 1994 GTECH CORPORATION.  ALL RIGHTS RESERVED.
C
C CONFIDENTIAL PROPRIETARY INFORMATION
C This item is the property of GTECH Corporation, W. Greenwich, Rhode
C Island, and contains confidential and trade secret information.  It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH.  Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published or disclosed, in whold or in part, directly
C or indirectly, except as expressly authorized by an officer of
C GTECH pursuant to written agreement.
C COPYRITF.DEF-------------------------------------------------------
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE CRSCHK(PBUF)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:CRSCOM.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INCLUDE 'INCLIB:QUECOM.DEF'
C
	INTEGER*4   PBUF            !Procom buffer number
	INTEGER*4   TRTYPE          !Transaction type
	INTEGER*4   XRFNUM          !Lolita XRF #
	INTEGER*4   ST              !Work variable
	INTEGER*4   DUMMY           !Dummy variable for RPTERR
	INTEGER*4   MESS(EDLEN)	    !ERRLOG Message Buffer
	INTEGER*4   TASK	    !Task id
C
	INTEGER*4   I4TEMP
	INTEGER*2   I2TEMP(2)
	BYTE	    I1TEMP(4)
	EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)
C
	TASK = CRS
C
C INCREMENT THE COUNTER IN THE PROCOM BUFFER
C
C NOTE: THE FIRST BYTE IN THE INSTANT TABLE IS USED AS A COUNTER
C       FOR THE NUMBER OF TIMES THE BUFFER COULD NOT BE ADDED TO
C       THE SEND QUEUE.
C
	BPRO(BINSTAB,PBUF)=BPRO(BINSTAB,PBUF)+1
	IF(BPRO(BINSTAB,PBUF).GT.REQTRY) THEN
C
C A BUFFER HAS BEEN REQUEUED THE MAX NUMBER OF TIMES.
C SET STATUS TO COULD NOT SEND AND SET THE TRCODE ALLOWING
C DISPAT TO QUEUE TO THE APPROPRIATE PROCESSING TASK.
C
	  I4TEMP=0
	  I1TEMP(1)=BPRO(BINSTAB+5,PBUF)
	  I1TEMP(2)=BPRO(BINSTAB+6,PBUF)
	  I1TEMP(3)=BPRO(BINSTAB+7,PBUF)
	  I1TEMP(4)=BPRO(BINSTAB+8,PBUF)
	  XRFNUM=I4TEMP			!Cross Reference #
C
	  I4TEMP=INCNS			    !COULD NOT SEND
	  BPRO(BINSTAB+13,PBUF) = I1TEMP(1)
	  BPRO(BINSTAB+14,PBUF) = I1TEMP(2)
C
	  IF((HPRO(TRCODE,PBUF).NE.TYPCWT) .AND.   !Checkwriter
     *       (HPRO(TRCODE,PBUF).NE.TYPSSI) .AND.   !Agent Update
     *       (HPRO(TRCODE,PBUF).NE.TYPFPT)) THEN   !Financial Pass-thru
	    HPRO(TRCODE,PBUF)=TYPCRS	           !Send others to INSOUT
	  ENDIF
C
C REMOVE BUFFER OFF OF CRSPRO'S QUEUE
C
	  CALL DQUTRA(TASK,PBUF)
C
C QUEUE BACK TO DISPAT.
C
	  CALL ABL(PBUF,QUETAB(1,DIS),ST)
C
C DISPLAY TIMED OUT ERROR MESSAGE TO CONSOLE.
C
	  MESS(1) = CRS
	  MESS(2) = TEGEN
	  MESS(3) = 35
	  MESS(4) = XRFNUM
	  MESS(5) = PBUF
	  CALL QUEMES(MESS)
	ENDIF
C
	RETURN
	END
