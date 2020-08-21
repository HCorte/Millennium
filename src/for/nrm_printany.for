C PRINTANY
C
C V01 12-MAY-2011 FJG INITIAL RELEASE
C
C SUBROUTINE TO PRINTOUT ANY STRING EVEN IN FG OR DETACHED MODE
C
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode
C Island, and contains confidential and trade secret information. It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expressly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright 1991-2000 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	subroutine printany(message)
	implicit none
C
	include 'INCLIB:SYSPARAM.DEF'
	include 'INCLIB:SYSEXTRN.DEF'
C
        character*(*) message
C
	if(isdetached()) then
	  call opstxt(message)         ! Send message to ELOG
        else
          type*,iam(),message          ! Send message to CONSOLE
	endif
C	
	end
