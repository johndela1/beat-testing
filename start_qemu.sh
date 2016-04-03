#qemu -hda /home/john/diskfile  -nographic -kernel /home/john/dev-linux/arch/x86/boot/bzImage  -append "console=ttyS0 rootfstype=ramfs"

#qemu  -nographic -kernel /home/john/dev-linux/arch/x86/boot/bzImage  \
-append "console=ttyS0"

#"earlyprintk=serial,uart0,57599"


qemu  -nographic -kernel /home/john/dev-linux/arch/x86/boot/bzImage  -append "console=ttyS0" -net nic  -net tap,ifname=tap0,script=no,downscript=no -net nic  -net tap,ifname=tap1,script=no,downscript=no

