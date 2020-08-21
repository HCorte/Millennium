C SCANVPF.FOR
C
C V01 01-JAN-2010 FJG INITIAL RELEASE FOR PORTUGAL
C
C PASSIVE VALIDATION SCANNER
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
	PROGRAM SCANVPF
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PASCOM.DEF'
C	INCLUDE 'INCLIB:DESVAL.DEF'
C       INCLUDE 'INCLIB:VDETAIL.DEF'
	INCLUDE 'INCLIB:VALPASFIL.DEF'
	INCLUDE 'INCLIB:HSHCOM.DEF'
!
        integer*4     emis
        integer*4     gind
        integer*4     mnum
        integer*4     mser
        integer*4     mfra        
!
        integer*4     cnot
        integer*4     coks
        integer*4     ctot
        integer*4     cseq
        integer*4     cnul
        integer*4     vlun  
        integer*4     rlun          
        integer*4     erro        
        integer*4     junk  
        integer*4     vkey(2)
        integer*4     cerr
        integer*4     errn   
        integer*4     errs        
!        
        integer*4     mmod
        integer*4     rind
        integer*4     mrec
        integer*4     buck
        integer*4     tbuk
        integer*4     ffdb(7)
        integer*4     rmax
        integer*4     null
        integer*4     rpag
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
!        
        integer*2     matrix(0:pmaxnumpop,pmaxsercla,pmaxfrapop)
!=======INIT VARS===============================================================
        rpag = 0  
!=======START PROCESS===========================================================
10      continue
        type*,iam()
        type*,iam(),'<<<<< SCANVPF - Test VPFs >>>>>'
        type*,iam()
!
        do mnum = 0,pmaxnumpop
          do mser = 1,pmaxsercla
            do mfra = 1,pmaxfrapop
              matrix(mnum,mser,mfra) = -1
            enddo
          enddo
        enddo
!        
	call wimg(5,'Enter validation file name:   ')
	read(5,'(5A4)') filn
	if(cfil(1:2).eq.'E '.or.cfil(1:2).eq.'e '.or.cfil(1:1).eq.' ') call gstop(GEXIT_OPABORT)
        do gind=1,numpas
	  do emis=1,pagemi
            if(cpasvpffil(emis,gind).eq.cfil) goto 20
	  enddo
        enddo
!        
        call inpnum('Enter game index              ',gind,1,numpas,erro)
        if(erro.ne.0) call gstop(GEXIT_OPABORT)
        call inpnum('Enter emission index          ',emis,1,pagemi,erro)        
        if(erro.ne.0) call gstop(GEXIT_OPABORT)
!        
20      continue
        type*,iam()        
        type*,iam(),'SCML draw: ',pasdraw(emis,gind)
        type*,iam(),'Looking for      ',cpasvpffil(emis,gind)
        type*,iam()
        if(.not.filexist(cpasvpffil(emis,gind))) then
          type*,iam(),'file does not exist: ',cpasvpffil(emis,gind)
          call gstop(GEXIT_OPABORT)
        endif        
        call find_available_lun(rlun,erro)
        if(erro.ne.0) then
          type*,iam(),'Error getting logical unit ',erro
          call gstop(GEXIT_FATAL)
        endif       
        call ropen('SCANVPF.REP',rlun,erro)
        if(erro.ne.0) then
          type*,iam(),' cannot open report file ',erro
          call gstop(GEXIT_FATAL)
        endif
        call title(cpasvpffil(emis,gind) // ' FILE ANALYSIS','SCANVPF',1,rlun,rpag,daycdc)        
        call find_available_lun(vlun,erro)
        if(erro.ne.0) then
          type*,iam(),'Error getting logical unit ',erro
          call gstop(GEXIT_FATAL)
        endif 
!+++++++Step 1: Sequential Access+++++++++++++++++++++++++++++++++++++++++++++++
        call openw(vlun,pasvpffil(1,emis,gind),4,0,0,erro)
        if(erro.ne.0) then
          call filerr(pasvpffil(1,emis,gind),1,erro,0)
          call gstop(GEXIT_OPABORT)
        endif  
        call ioinit(ffdb,vlun,bucsec*256)
        call getsiz_used(vlun,tbuk)
        tbuk = tbuk / BUCSEC
        buck = 0
        cseq = 0
        cerr = 0
        cnul = 0
!
        type*,iam(),'Step 1: Scanning file sequentially .........'
        do while (.true.)
          buck = buck + 1
          call readw(ffdb,buck,bbig,erro)
          if(erro.eq.144) exit
          if(erro.ne.0) then
            call filerr(pasvpffil(1,emis,gind),2,erro,buck)
            call gstop(gexit_fatal)
          endif
          null = 0
          mrec = 0
          do rind = 1,rmax          
            if(mrec.gt.1) then
              mrec = mrec - 1
            else
              if(bone(1,rind).ne.0) then
                cseq = cseq + 1
                mrec = ishft(bone(1,rind),-30) + 1
                i4temp = iand(bone(1,rind),'3FFFFFFF'X)
                mmod = mod(i4temp,tbuk)+1
                mnum   = iand(i4temp,'00FFFFFF'X)
                mser   = ishft(i4temp,-24)
                i4temp = bone(2,rind)
                mfra   = zext(i1temp(1))
                matrix(mnum,mser,mfra) = buck
                if(mmod.ne.buck) then
                  cerr = cerr + 1                   
                  write(rlun,9020) cerr,mnum,mser,mfra,mmod,buck,bone(1,rind),bone(2,rind)                   
                endif
               else
                null = null + 1
              endif
            endif
          enddo
          if(null.eq.rmax) cnul = cnul + 1
        enddo
        call closefil(ffdb)    
        buck = buck - 1
        if(cnul.gt.0) then
          type*,iam(),cnul,' of ',buck,' buckets empties'
          write(rlun,9030) cnul,buck
        endif
        if(cerr.gt.0.or.cnul.gt.0) call title(cpasvpffil(emis,gind) // ' FILE ANALYSIS','SCANVPF',1,rlun,rpag,daycdc)
!+++++++Step 2: Direct access+++++++++++++++++++++++++++++++++++++++++++++++++++
        type*,iam(),'Step 2: Scanning file with direct access ...'
        call iopen(pasvpffil(1,emis,gind),vlun,VPFLEN*2,VFSCDC,VFSSER*2-1,erro)
        if(erro.ne.0) then
          call filerr(pasvpffil(1,emis,gind),1,erro,0)
          call gstop(GEXIT_OPABORT)
        endif       
        cnot = 0
        coks = 0
        ctot = 0      
        errn = 0
        errs = 0
        do mnum = 0,pasnumtck(emis,gind)-1
          do mser = 1,pasnumser(emis,gind)
            do mfra = 1,pasnoffra(emis,gind)        
              ctot = ctot + 1
              vkey(1) = mfra
              vkey(2) = ishft(mser,24) + mnum
              call iread(vkey,V4BUF_PAS,vlun,erro)
              if(erro.ne.0) then
                if(erro.eq.ERRRNF) then
                  cnot = cnot + 1
                  if(matrix(mnum,mser,mfra).ne.-1) then
                    if(mod(errn,54).eq.0) call title(cpasvpffil(emis,gind) // ' FILE ANALYSIS','SCANVPF',1,rlun,rpag,daycdc)
                    errn = errn + 1
                    write(rlun,9000) errn,mnum,mser,mfra,matrix(mnum,mser,mfra)
                  endif
                else
                  call filerr(pasvpffil(1,emis,gind),2,erro,(mnum*10000)+(mser*100)+mfra)                  
                endif
              else
                coks = coks + 1  
                if(matrix(mnum,mser,mfra).eq.-1) errs = errs + 1
              endif                    
            enddo
          enddo
        enddo
!
        type*,iam()
        type*,iam(),'+++++++ Step  1:   Sequential  +++++++'
        type*,iam(),'Total sequential records: ',cseq,' (Check with ANLVPF)'        
        type*,iam(),'Total hashing key errors: ',cerr,' (Tickets erroneous)'
        type*,iam(),'Total real buckets read:  ',buck,' (Sequential BUCKETS)'
        type*,iam(),'Total calculated buckets: ',tbuk
        type*,iam(),'Total hash buckets:       ',FCB(FCBHSH,vlun)
        type*,iam()        
        type*,iam(),'+++++++ Step  2: Direct Access +++++++'
        type*,iam(),'Records NOT FOUND:        ',cnot
        type*,iam(),'Records Found:            ',coks,' (As sequential records)'        
        type*,iam(),'Total records tried:      ',ctot,' (Total possibilities)'
        type*,iam(),'Consistency Error RNF:    ',errn,' (More in sequential)'
        type*,iam(),'Consistency Error XTR:    ',errs,' (More in direct access)'
        type*,iam()        
!        
        call iclose(vlun,bbig,junk)
        close(rlun)         
!===============================================================================        
        call gstop(GEXIT_SUCCESS)    
!=======FORMAT SECTION==========================================================
9000    format(1X,'Error RNF:  ',I6,' Num: ',I5.5,' Ser: ',I2,' Fra: ',I2,' Buk: ',I8)                               
9010    format(1X,'Bucket: ',I6,' Record: ',I4,' Num: ',I5.5,' Ser: ',I2,' Fra: ',I2,' Key1: ',Z8,' Key2: ',Z8)
9020    format(1X,'Error HASH: ',I6,' Num: ',I5.5,' Ser: ',I2,' Fra: ',I2,' Mod: ',I8,' Buk: ',I8,' [',Z8.8,'][',Z8.8,']')
9030    format(1X,I6,' of ',I6,' buckets are empty.')
        end
