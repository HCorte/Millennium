C
C PASIOSUBS.FOR
C
C V02 01-JAN-2010 FJG ePassive
C
C Subroutines to handle access to TPF files.
C
C pasio_init   : Init FDB for TPF
C pasio_create : Create TPF file
C pasio_open   : Open TPF file
C pasio_read   : Read TPF file
C pasio_write  : Write TPF file
C pasio_close  : Close TPF file
C
C===============================================================================
C This item is the property of GTech Corporation, Providence, Rhode Island, and
C contains confidential and trade secret information. It may not be transferred
C from the custody or control of GTech except as authorized in writing by an
C officer of GTech. Neither this item nor the information it contains may be
C used, transferred, reproduced, published, or disclosed, in whole or in part,
C and directly or indirectly, except as expressly authorized by an officer of
C GTech, pursuant to written agreement.
C
C Copyright (c)2009 GTech Corporation. All rights reserved.
C===============================================================================
C===============================================================================
C       PASIO_INIT: Init FDB record
C===============================================================================
C=======OPTIONS /check=nooverflow
        subroutine pasio_init(pasfdb,gind,gdraw,gmaxnum,gmaxser,gmaxfra,gfilnam)
        implicit none
!        
        include 'inclib:sysparam.def'
        include 'inclib:sysextrn.def'
        include 'inclib:global.def'    
        include 'inclib:standard.def'
        include 'inclib:pasiosubs.def'
!
        record /stpasfdb/ pasfdb
        record /stpasrec/ pasbuk(pasbukrec) !32 of stpasrec  
!                
        integer*4         gind
        integer*4         gdraw
        integer*4         gmaxser
        integer*4         gmaxfra
        integer*4         gmaxnum                
        character*20      gfilnam
!
        pasfdb.err = ioe_noerr
        if(sizeof(pasbuk).ne.pasbuklen)             pasfdb.err = pasfdb.err + ioe_rlen
        if(gind.le.0.or.gind.gt.numpas)             pasfdb.err = pasfdb.err + ioe_gind
        if(gdraw.le.0.or.gdraw.gt.9999)             pasfdb.err = pasfdb.err + ioe_draw            
        if(gind.eq.psbcla) then
          if(gmaxser.le.0.or.gmaxser.gt.pmaxsercla) pasfdb.err = pasfdb.err + ioe_maxser           
          if(gmaxfra.le.0.or.gmaxfra.gt.pmaxfracla) pasfdb.err = pasfdb.err + ioe_maxfra                              
          if(gmaxnum.le.0.or.gmaxnum.gt.pmaxnumcla) pasfdb.err = pasfdb.err + ioe_maxnum                              
        else
          if(gmaxser.le.0.or.gmaxser.gt.pmaxserpop) pasfdb.err = pasfdb.err + ioe_maxser           
          if(gmaxfra.le.0.or.gmaxfra.gt.pmaxfrapop) pasfdb.err = pasfdb.err + ioe_maxfra                              
          if(gmaxnum.le.0.or.gmaxnum.gt.pmaxnumpop) pasfdb.err = pasfdb.err + ioe_maxnum                              
        endif
!        
        if(pasfdb.err.eq.ioe_noerr) then
          pasfdb.lun    = 0
          pasfdb.gind   = gind    
          pasfdb.draw   = gdraw    
          pasfdb.maxser = gmaxser  
          pasfdb.maxfra = gmaxfra  
          pasfdb.maxnum = gmaxnum  
          pasfdb.filnam = gfilnam
!         pasfdb.fdb will be initialized in IOINIT
        endif
!        
        return
        end
C===============================================================================
C       PASIO_CREATE: Create a TPF file
C===============================================================================
C=======OPTIONS /check=nooverflow
        subroutine pasio_create(pasfdb)
        implicit none
!        
        include 'inclib:sysparam.def'
        include 'inclib:sysextrn.def'
        include 'inclib:global.def'    
        include 'inclib:standard.def'
        include 'inclib:pasiosubs.def'
!
        record /stpasfdb/ pasfdb
!        
        integer*4         wrkerr
        integer*4         wrktot
!        
        integer*4         ifilen(5)
        character*20      cfilen
        equivalence       (ifilen,cfilen) 
!
        if(pasfdb.err.eq.ioe_noerr) then    ! If exist a previous error, directly return
          write(pasfdb.filnam,'(A8,I2.2,I4.4,A6)') pasfdb.filnam(1:8),pasfdb.gind,pasfdb.draw,pasfdb.filnam(15:20)        
!
          call find_available_lun(pasfdb.lun,wrkerr)
          if(wrkerr.eq.0) then
            wrktot = ((pasfdb.maxser*pasfdb.maxfra*(pasfdb.maxnum+1))/pasbukrec)
            cfilen = pasfdb.filnam
            call crtfil_ncntg(ifilen,wrktot,pasfdb.err)
            if(pasfdb.err.ne.ioe_noerr) pasfdb.err = -pasfdb.err          
          else
            pasfdb.err = ioe_lun
          endif
        endif      
!
        return
        end       
C===============================================================================
C       PASIO_OPEN: Open a TPF file
C===============================================================================
C=======OPTIONS /check=nooverflow
        subroutine pasio_open(pasfdb)
        implicit none
!        
        include 'inclib:sysparam.def'
        include 'inclib:sysextrn.def'
        include 'inclib:global.def'    
        include 'inclib:standard.def'
        include 'inclib:pasiosubs.def'
!
        record /stpasfdb/ pasfdb
!        
        integer*4         wrkerr
        character*6       wrkchr           
        integer*4         optrea  
!
        integer*4         ifilen(5)
        character*20      cfilen
        equivalence       (ifilen,cfilen)        
!        
        optrea=4
        goto 100
!        
        entry pasio_openro(pasfdb)
        optrea=0    
!
100     continue
        if(pasfdb.err.eq.ioe_noerr) then    ! If exist a previous error, directly return
          write(wrkchr,'(I2.2,I4.4)') pasfdb.gind,pasfdb.draw
          if(pasfdb.filnam(9:14).ne.wrkchr) then
            pasfdb.err = pasfdb.err + ioe_filnam        
          else
            call find_available_lun(pasfdb.lun,wrkerr)
            if(wrkerr.eq.0) then
              cfilen = pasfdb.filnam
              call openw(pasfdb.lun,ifilen,optrea,0,0,pasfdb.err)
              if(pasfdb.err.ne.ioe_noerr) then
                pasfdb.err = -pasfdb.err     
              else
                call ioinit(pasfdb.fdb,pasfdb.lun,pasbuklen)                
              endif
            else
              pasfdb.err = ioe_lun
            endif
          endif
        endif
!
        return
        end
C===============================================================================
C       PASIO_WRITE/READ: Write/Read to a TPF file
C===============================================================================
C=======OPTIONS /check=nooverflow
        subroutine pasio_write(pasfdb,pasnum,passer,pasfra,pasrec)
        implicit none
!        
        include 'inclib:sysparam.def'
        include 'inclib:sysextrn.def'
        include 'inclib:global.def'    
        include 'inclib:standard.def'
        include 'inclib:pasiosubs.def'
!
        record /stpasrec/ pasrec  
        record /stpasfdb/ pasfdb
        record /stpasrec/ pasbuk(pasbukrec) !32 of stpasrec         
!                
        integer*4         pasnum
        integer*4         passer
        integer*4         pasfra                
        integer*4         offrec
        integer*4         offbuk
        logical           iswrit
        logical           issync     
        logical           ischek
!=======For sys$flush function
        integer*4         forlun
        integer*4         for$rab
!-------NORMAL WRITE
        iswrit = .true.
        issync = .false.
        ischek = .true.        
        goto 100
!-------SYNC WRITE
        entry pasio_swrite(pasfdb,pasnum,passer,pasfra,pasrec)
        iswrit = .true.
        issync = .true.
        ischek = .true.
        goto 100
!-------FIRST WRITE NO KEY CHECK
        entry pasio_fwrite(pasfdb,pasnum,passer,pasfra,pasrec)
        iswrit = .true.
        issync = .false.
        ischek = .false.
        goto 100
!-------NORMAL READ
        entry pasio_read(pasfdb,pasnum,passer,pasfra,pasrec)
        iswrit = .false.
        issync = .false.
        ischek = .true.
!        
100     continue
        if(pasfdb.err.eq.ioe_noerr) then    ! If exist a previous error, directly return
          if(passer.gt.pasfdb.maxser.or.passer.le.0) pasfdb.err = pasfdb.err + ioe_maxser
          if(pasfra.gt.pasfdb.maxfra.or.pasfra.le.0) pasfdb.err = pasfdb.err + ioe_maxfra
          if(pasnum.gt.pasfdb.maxnum.or.pasnum.lt.0) pasfdb.err = pasfdb.err + ioe_maxnum       
          if(pasfdb.err.ne.ioe_noerr) return
!===============================================================================
!       Very important. The file is  sequential  and flat so the record offset
!       is calculated based  on  the  maximum   series and fractions but it is
!       draw dependant. The FDB structure should have this correct information
!       so to find the proper record  we have to calculate an offset as shown:
!===============================================================================
          offrec = (pasnum+1-1)*pasfdb.maxser*pasfdb.maxfra !step 1: pasnum-1+1 because starts at zero
          offrec = offrec + ((passer-1)*pasfdb.maxfra)      !step 2: the previous sers * # of fras
          offrec = offrec + pasfra                          !step 3: add the searched fraction
          offbuk = ((offrec - 1)/pasbukrec)+1               !step 4: calculate bucket record
          offrec = offrec-((offbuk-1)*pasbukrec)            !step 5: offset within the bucket
!===============================================================================
!         Since records belong to a bucket, always we need to perform a read
!         Let's expect a proper behavior of the VMS cache
!===============================================================================
          call readw(pasfdb.fdb,offbuk,pasbuk,pasfdb.err)
          if(pasfdb.err.eq.ioe_noerr) then
            if(ischek.and.(pasbuk(offrec).key.ne.pasio_key(pasnum,passer,pasfra))) then
             type*,ischek
             type*,pasbuk(offrec).key
             type*,pasio_key(pasnum,passer,pasfra)
             pasfdb.err = ioe_keyerr
            else              
              if(iswrit) then
!                pasrec.tics = pasrec.tics + 1
                pasbuk(offrec) = pasrec
                call writew(pasfdb.fdb,offbuk,pasbuk,pasfdb.err)    
                if(pasfdb.err.ne.ioe_noerr) pasfdb.err = -pasfdb.err
                if(issync) then
                  forlun = pasfdb.lun
                  call sys$flush(%val(for$rab(forlun)))
                endif
              else
                pasrec = pasbuk(offrec)  ! Load PASREC from BUCKET
              endif
            endif
          else
            pasfdb.err = -pasfdb.err
          endif          
        endif
!     
        return
        end        
C===============================================================================
C       PASIO_CLOSE: Close a TPF file
C===============================================================================
C=======OPTIONS /check=nooverflow
        subroutine pasio_close(pasfdb)
        implicit none
!        
        include 'inclib:sysparam.def'
        include 'inclib:sysextrn.def'
        include 'inclib:global.def'    
        include 'inclib:standard.def'
        include 'inclib:pasiosubs.def'
!
        record /stpasfdb/ pasfdb
!
        call closefil(pasfdb.fdb)
!
        pasfdb.lun = 0
        pasfdb.gind = 0
        pasfdb.draw = 0
        pasfdb.maxser = 0
        pasfdb.maxfra = 0
        pasfdb.maxnum = 0
        write(pasfdb.filnam,'(20X)')
!
        return
        end     
C===============================================================================
C       PASIO_DUMP: Dump FDB record
C===============================================================================
C=======OPTIONS /check=nooverflow
        subroutine pasio_dump(pasfdb)
        implicit none
!        
        include 'inclib:sysparam.def'
        include 'inclib:sysextrn.def'
        include 'inclib:global.def'    
        include 'inclib:standard.def'
        include 'inclib:pasiosubs.def'
!
        record /stpasfdb/ pasfdb
!               
        type*,iam(),'lun:    ',pasfdb.lun
        type*,iam(),'gind:   ',pasfdb.gind   
        type*,iam(),'draw:   ',pasfdb.draw   
        type*,iam(),'maxser: ',pasfdb.maxser 
        type*,iam(),'maxfra: ',pasfdb.maxfra 
        type*,iam(),'maxnum: ',pasfdb.maxnum 
        type*,iam(),'filnam: ',pasfdb.filnam 
!        
        return
        end
C===============================================================================
C       PASIO_KEY: Generate KEY
C===============================================================================
C=======OPTIONS /check=nooverflow
        character*3 function pasio_key(pasnum,passer,pasfra)
        implicit none
!        
        integer*4         pasnum
        integer*4         passer
        integer*4         pasfra 
        integer*4         iwkey
        character*4       cwkey
!        
        equivalence       (iwkey,cwkey)
!
        iwkey = ishft(pasnum,7)+ishft(passer,3)+pasfra
        pasio_key = cwkey(1:3)
!
        return
        end        
