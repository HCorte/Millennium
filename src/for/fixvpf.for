C FIXVPF.FOR
C
C V01 01-JAN-2010 FJG INITIAL RELEASE FOR PORTUGAL
C
C PASSIVE FIXER VPF FOR QA
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
C Copyright 1990,1993 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	PROGRAM FIXVPF
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PASCOM.DEF'
C	INCLUDE 'INCLIB:DESVAL.DEF'
C       INCLUDE 'INCLIB:VDETAIL.DEF'
	INCLUDE 'INCLIB:PRMHSH.DEF'
	INCLUDE 'INCLIB:PRMVPF.DEF'	
C	INCLUDE 'INCLIB:VALPASFIL.DEF'
C	INCLUDE 'INCLIB:HSHCOM.DEF'
C
        integer*4     cseq
        integer*4     vlun  
        integer*4     erro        
        integer*4     cerr
!        
        integer*4     rind
        integer*4     mrec
        integer*4     buck
        integer*4     ffdb(7)
        integer*4     rmax
        integer*4     rwri
        integer*4     trec
        logical       updt
        parameter     (rmax=i4bucsiz/vpflen)
!
        integer*4     bbig(i4bucsiz)
        integer*4     bone(vpflen,rmax)
        equivalence   (bbig,bone)
!
        integer*1     i1temp(4)
        integer*2     i2temp(2)
        integer*4     i4temp
        equivalence   (i4temp,i2temp,i1temp)
!
        integer*4     filn(5)        
        character*20  cfil
        equivalence   (filn,cfil)
!=======START PROCESS===========================================================
        type*,iam()
        type*,iam(),'<<<<< FIXVPF - FIX VPFs >>>>>'
        type*,iam()
	call wimg(5,'Enter validation file name:   ')
	read(5,'(5A4)') filn
	if(cfil(1:2).eq.'E '.or.cfil(1:2).eq.'e '.or.cfil(1:1).eq.' ') call gstop(GEXIT_OPABORT)
!        
        if(.not.filexist(cfil)) then
          type*,iam(),'file does not exist: ',cfil
          call gstop(GEXIT_OPABORT)
        endif        
        call find_available_lun(vlun,erro)
        if(erro.ne.0) then
          type*,iam(),'Error getting logical unit ',erro
          call gstop(GEXIT_FATAL)
        endif 
!+++++++Sequential Access+++++++++++++++++++++++++++++++++++++++++++++++++++++
        call openw(vlun,filn,4,0,0,erro)
        if(erro.ne.0) then
          call filerr(cfil,1,erro,0)
          call gstop(GEXIT_OPABORT)
        endif  
        call ioinit(ffdb,vlun,bucsec*256)
        buck = 0
        cseq = 0
        cerr = 0
        rwri = 0
!
        type*,iam(),'Scanning file for errors...'
        do while (.true.)
          buck = buck + 1
          call readw(ffdb,buck,bbig,erro)
          if(erro.eq.144) exit
          if(erro.ne.0) then
            call filerr(cfil,2,erro,buck)
            call gstop(gexit_fatal)
          endif
          mrec = 0
          updt = .false.
          do rind = 1,rmax          
            if(mrec.gt.1) then
              mrec = mrec - 1
            else
              if(bone(1,rind).ne.0) then
                cseq = cseq + 1
                mrec = ishft(bone(1,rind),-30) + 1
!                
                i4temp = bone(2,rind)
                if(i1temp(2).eq.1) then
                  trec = zext(i1temp(3))
                  trec = ishft(trec,-5) 
                  updt = .true.
                  cerr = cerr + 1
                  i1temp(2) = 0
                  bone(2,rind) = i4temp
                  if(trec.eq.VAL_SHORT) then
                    i4temp        = bone(8,rind)
                    i1temp(4)     = 1
                    bone(8,rind)  = i4temp
                  elseif(trec.eq.VAL_REG) then
                    i4temp        = bone(10,rind)
                    i1temp(2)     = 1
                    bone(10,rind) = i4temp
                  else
                    type*,iam(),'TREC erroneous: ',trec
                  endif
                endif                
              endif
            endif
          enddo
!          
          if(updt) then
            rwri = rwri + 1
            call writew(ffdb,buck,bbig,erro)
            if(erro.ne.0) then
              call filerr(cfil,3,erro,buck)
              call gstop(gexit_fatal)
            endif            
          endif
        enddo
        call closefil(ffdb)    
        buck = buck - 1
!
        type*,iam()
        type*,iam(),'Total buckets read:    ',buck
        type*,iam(),'Total buckets rewrite: ',rwri        
        type*,iam(),'Total records read:    ',cseq
        type*,iam(),'Total records fixed:   ',cerr        
        type*,iam()        
!===============================================================================        
200     continue 
        call gstop(GEXIT_SUCCESS)    
!=======FORMAT SECTION==========================================================
        end
