*
*  ORCA/M Shell
*

unset exit
set orca 18

Newer {orca}/ORCA.Sys16 sh.rez
if {status} != 0
   set exit on
   echo compile -e sh.rez keep={ORCA}/ORCA.Sys16
   compile -e sh.rez keep={ORCA}/ORCA.Sys16
   unset exit
end

if {#} == 0 then

   Newer obj/mm.a mm.asm mm.macros sh.dp
   if {Status} != 0
      set mm mm
   end

   Newer obj/subs.a subs.asm sh.dp
   if {Status} != 0
      set subs subs
   end

   Newer obj/call.a call.asm sh.dp
   if {Status} != 0
      set call call
   end

   Newer obj/control.a control.asm sh.dp
   if {Status} != 0
      set control control
   end

   Newer obj/cmd.a cmd.asm sh.dp
   if {Status} != 0
      set cmd cmd
   end

   Newer obj/switch.a switch.asm sh.dp
   if {Status} != 0
      set switch switch
   end

   Newer obj/line.a line.asm sh.dp
   if {Status} != 0
      set line line
   end

   Newer obj/io.a io.asm sh.dp
   if {Status} != 0
      set io io
   end

   Newer obj/sh.a sh.asm sh.dp
   if {Status} != 0
      set sh sh
   end

   set exit on

   for i in {sh} {mm} {call} {io} {cmd} {line} {switch} {subs} {control}
      echo assemble +t +e {i}.asm
      assemble +t +e {i}.asm
   end

else

   set exit on

   for i
      echo assemble +t +e {i}.asm
      assemble +t +e {i}.asm
   end
end

linkit
