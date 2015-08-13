# SPC Audio Streaming
Audio streaming for SNES/SPC700

Copyright (C) 2014,2015 Christopher Jack
Released under the terms of the GNU LGPL 3.0 license (see: LICENSE)
No warrnaties of any kind.

Related discussion (forum) on romhacking.net:
http://www.romhacking.net/forum/index.php/topic,17929.0.html

Here it is for anyone that wants to play around with it or try it on real hardware.
This version includes a funky VU bar that won't work in ZSNES (ZSNES reports DSP OUTX registers as 00).

I don't have real hardware to test this on so I've coded the timer target's to keep in sync with BSnes/Higan which I believe is the more accurate of the emulators I test with.  (It assumes target=4 makes stage 2 count as 0,1,2,3,*0,1,2,3,*0,1...  where * are advances of the 4-bit stage 3 counter.  This assumption causes overflow in Geiger9X and underflow in ZSNES).  If someone tests this on real hardware, please report the result and hopefully prove Anomie's APU docs correct.

1. Stick the audio (INCBIN) into the ROM image from file offset $008000.
2. Pad the brr audio file to a multiple of 9*32768 (9 banks, $048000).
3. Set AUDBANK_HI appropriately: A 9-bank brr would be AUDBANK_LO=$01, AUDBANK_HI=$0A

The source uses the SA-1 MMC registers to bank the first 1-2MB of ROM to 00-3f, 3-4 of RAM to 80-bf and 5-8MB to c0-ff.
The audio code will play into the $c0 region but I have some issue trying to loop back correctly to AUDBANK_LO (the issue is with getting a proper multiple of 9 so brr chunks aren't mismatched).  Someone who fixes this please let me know :)  Thanks.

I've tried to unroll the SPC code a bit to make the SNES spin less in the transfer code but it's still doing 10-11 spins per pass during transfer (got it down from 12-13 tho).  In theory it'll spin less in PAL since the S-CPU is running slower relative to the SPC's code.

Assembled with Asar.
Sample build batchfile included.

Known issue:
A utility like IpsAndSum may be needed to correct the ROM header checksum.

I haven't included any audio files due to the same taboos as asking for ROMs in the forums. :P
