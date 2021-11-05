$!
$! v01 13-feb-95 wxm initial release
$!
$ if "''p1'" .eqs. "" then p1 = "ACTIVE"
$ if "''p1'" .eqs. "*"      then goto START
$ if "''p1'" .eqs. "ACTIVE" then goto START
$ if "''p1'" .eqs. "LIVE"   then goto START
$ if "''p1'" .eqs. "TOOL"   then goto START
$ if "''p1'" .eqs. "JUNK"   then goto START
$ if "''p1'" .eqs. "UPDATE" then goto START
$ write sys$output "Invalid group parameter."
$ goto END
$ START:
$ if f$search("gxsrc:sitelib.lis") .nes. "" then del/nolog gxsrc:sitelib.lis;*
$ if "''p1'" .eqs. "*"
$ then
$  domain = "library"
$  cms sho element/format="#E"/out=gxsrc:sitelib.lis SIT_*.FOR
$ else
$  domain = "''p1' group"
$  cms sho element/format="#E"/out=gxsrc:sitelib.lis 'p1'_code
$ endif
$ if f$search("gxsrc:sitelib.lis") .eqs "" then goto NONE
$ open/read/error=erropin infil gxsrc:sitelib.lis
$ if f$search("gxsrc:sitelib.com") .nes. "" then del/nolog gxsrc:sitelib.com;*
$ open/write/error=erropout outfil gxsrc:sitelib.com
$ write/err=errwrout outfil "$set noon"
$ write/err=errwrout outfil "$set ver"
$ cnt = 0
$ name1 = ""
$ NEXT:
$  read/end=endin/err=errrdin infil file
$  if f$parse("''file'",,,"TYPE") .nes. ".FOR" then goto NEXT
$  name = f$parse("''file'",,,"NAME")
$  if f$extract(0,4,"''name'") .nes. "SIT_" then goto NEXT
$  if name1 .nes. ""
$  then
$   if cnt .lt. 25 then name1 = name1 + ",-"
$   write outfil name1
$   name1 = ""
$  endif
$  if cnt .eq. 0
$  then
$   name1 = "$LIBRARY/CREATE/OBJECT GXSRC:SITELIB.OLB ''name'"
$   cnt = 1
$  else
$   if cnt .eq. 25
$   then
$     name1 = "$LIBRARY/REPLACE/OBJECT GXSRC:SITELIB.OLB ''name'"
$     cnt = 1
$   else
$     name1 = "''name'"
$     cnt = cnt+1
$   endif
$  endif
$ goto NEXT
$ ERRRDIN:
$ write sys$output "Error reading temporary file GXSRC:SITELIB.LIS"
$ close infil
$ close outfil
$ goto END
$ ERRWROUT:
$ write sys$output "Error writing output file GXSRC:SITELIB.COM"
$ close infil
$ close outfil
$ goto END
$ ERROPIN:
$ write sys$output "Error opening temporary file GXSRC:SITELIB.LIS"
$ goto END
$ ERROPOUT:
$ write sys$output "Error opening output file GXSRC:SITELIB.COM"
$ close infil
$ goto END
$ ENDIN:
$ if name1 .nes. ""
$ then
$   if f$extract(f$length("''name1'")-1,1,"''name1'") .eqs. "-"
$   then
$     name1 = f$extract(0,f$length("''name1'")-1,"''name1'")
$   endif
$   write outfil name1
$ endif
$ write/err=errwrout outfil "$set nover"
$ close infil
$ close outfil
$! del/nolog gxsrc:sitelib.lis;*
$ if cnt .eq. 0
$ then
$  write sys$output "No SIT_*.FOR files found in ''domain'"
$  del/nolog gxsrc:sitelib.com;*
$ endif
$ goto END
$ NONE:
$ write sys$output "No SIT_*.FOR files found in ''domain'"
$ END:
$ exit
