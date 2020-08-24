C REPROX REPROCESS TASK INVOKED BY RESET
C
C V01 FJG 11-MAY-2011 INITIAL RELEASE
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode Island,
C and contains confidential and trade secret information. It may not be
C transferred from the custody or control of GTECH except as authorized in
C writing by an officer of GTECH. Neither this item nor the information it
C contains may be used, transferred, reproduced, published, or disclosed,
C in whole or in part, and directly or indirectly, except as expressly
C authorized by an officer of GTECH, pursuant to written agreement.
C
C Copyright 1994 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Purpose:
C       REPROCESS TASK:
C               THIS TASK DOES THE REPROCESS FOR RESET
C               THIS MEANS COULD USE SENDTRA, SINCE SHOULD BE SENT FROM THE SAME
C               CPU
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      program reprox
      implicit none
C
      include 'INCLIB:SYSPARAM.DEF'
      include 'INCLIB:SYSEXTRN.DEF'
      include 'INCLIB:GLOBAL.DEF'
      include 'INCLIB:CONCOM.DEF'
C
      logical       rtask
      character*132 message
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      call copyrite
      call snif_and_wrkset
C
      rtask=.true.
      if(iand(p(reprow),isrtsk).eq.0) rtask=.false.
C
C     Start REPROCESSING
C
      if(iand(p(reprow),iswork).eq.0) then
        call printany('WARNING: Missing REPROW value')
      endif
      call reproc(rtask)
C
C     Inform RESET that finished
C
      if(daysts.ne.dskill) then
        p(reprow) = 0
      endif
C
      call gstop(gexit_success)
      end
