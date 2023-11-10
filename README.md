# Scriptor
Scriptor is a document editor for Sphere microcomputers. It's a personal project that I'm maintaining here. (Personal because I can't imagine anyone else ever wanting to us it.)

## What it is

The Sphere series of microcomputers were produced in Bountiful, Utah, from 1975-1977. They are based around the Motorola 6800 processor and "complete" systems included a built-in in keyboard and memory-mapped video display.  

That setup sounds ready-made for word processing applications, but there were none for the Sphere for several reasons:

- "Word processing" as an home productivity application category didn't exist yet. There were "text editors" but these were either designed to edit assembly code or acted as line-based meta editors that also had a real learning curve.
- Memory was limited: early systems had 4K, and although later 8-20K became common, pages of text eat up memory _very quickly_.
- Online storage was generally limited to cassette until disks because available, and Sphere's Kansas City cassette standard especially is an extremely slow means of storing and retrieving large chunks of written text.
- The development environment was fragile and difficult, so putting together "user focused" complex software with a lot of edge cases would have been a massive pain.  

But now enter Scriptor: a WYSIWYG, live-word-wrap document editor for the 1975 Sphere. Scriptor is designed for drafting and basic editing of multipage text documents. It keeps the active document in a backing store offscreen, and can move the cursor around in the document by character, line or page. Inserts and deletes are performed directly within the working text, just as you'd expect from any simple contemporary text editor. And crucially, Scriptor features soft word wrapping that is always active.  

The program will expand its document area to fill all available memory. At least 8K is required, and frankly, you should have the whole of available RAM populated if you're working on any nontrivial text. (I don't know what happens when you run out of available document memory. Probably bad stuff, I should look into that...)

The way to save and load documents is, still, by cassette. Honestly, that part still _really_ sucks. 

## What it isn't

A shining example of efficient, clean, well-designed and nicely styled assembly code. 

## Building and running it

I'm sorry this is not in a super convenient format, again, I don't expect anyone but me to want to use it. The program is in a single assembly file, which targets a home-brew toolchain that I use, but is so standard in convention that I expect the code to be acceptable to any other 6800 assemblers. 

Once you've assembled the file, you have a raw program binary. You can turn this into a cassette image using the [bin2sphere](https://github.com/bzotto/bin2sphere) tool. From there the `.cassette` image file can be turned into a Kansas City standard format `wav` file for use on real hardware, or can be loaded directly into my [Sphere web emulator](https://sphere.computer/emulator). 

## Using Scriptor

It's largely self explanatory. Program starts with an empty document which you type into. The Sphere's Backspace key is the backspace. Move the cursor with the arrow keys if you are using a keyboard that has them (or the emulator), or Control-WASD if not. Control P and L are "page up" and "page down" respectively. Control K and O are line home and end, respectively.  

Control-Y yanks (aka kills) the current cursor line.

Use Control-V to save the current document to cassette and Control-B to load a document from cassette. Both of these will take a long time for nontrivial documents. (Sorry, I haven't implemented prefix coding compression yet.)

## What you need for real hardware

Got a Sphere system and want to write some stuff? Great! The default program assumes a hardware configuration like this: 

- CRT/1 video board (32x16 lines of text) at $E000 (the default).
- PDS-V3N firmware (or compatible; the PDS-V3D? should work too if you have the KBD/1 keyboard connected).
- At least 8K RAM (i.e. a MEM/1 or MEM/6 board installed).
- SIM/1 for cassette and SYS2NF cassette firmware.

## For more information

Please see my Sphere computer site at https://sphere.computer
You can reach me at bzotto@gmail.com

Thanks!

