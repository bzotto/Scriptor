# Scriptor
Scriptor is a document editor for Sphere microcomputers. It's a personal project that I'm maintaining here. (Personal because I can't imagine anyone else ever wanting to use it, but if you do, by all means!)

## What it is

The Sphere series of microcomputers were produced in Bountiful, Utah, from 1975-1977. They are based around the Motorola 6800 processor and "complete" systems included a built-in in keyboard and memory-mapped video display.  

That setup sounds ready-made for word processing applications, but there were none for the Sphere, for several reasons:

- "Word processing" as a home productivity application category didn't exist yet. There were "text editors" but these were either designed to edit assembly code or act as line-based meta editors that also had a real learning curve.
- Memory was limited: early systems had 4K, and although later 8-20K became common, pages of can text eat up memory _quickly_.
- Online storage was generally limited to cassette until disks became widely available in the late 70s, and Sphere's Kansas City cassette standard  is an extremely slow (300 baud) means of storing and retrieving large chunks of written text.
- The development environment (such as it was) was fragile and difficult, so putting together complex "user focused" software with a lot of edge cases would have been a massive pain.  

But now enter Scriptor: a WYSIWYG, live-word-wrap document editor, for the 1975 Sphere. Scriptor is designed for drafting and basic editing of multipage text documents. It keeps the active document in a backing store offscreen, and can move the cursor around in the document by character, line or page. Inserts and deletes are performed directly within the working text, just as you'd expect from any simple contemporary text editor. And crucially, Scriptor features soft word wrapping that is always active.  

The program will expand its document area to fill all available memory. At least 8K is required, and frankly, you should have the whole of available RAM populated if you're working on any nontrivial text. (I don't know what happens when you run out of available document memory. Probably bad stuff, I should look into that...)

The way to save and load documents is, still, by cassette. It can be agonizing, although this program implements a form of compression for the saved text to improve that experience somewhat.

## What it isn't

A shining example of efficient, clean, well-designed and nicely-styled assembly code. :-)

## Building and running it

I'm sorry this is not in a super convenient format, again, I guess I don't really expect anyone but me to ever see this ()but would be delighted if someone somewhere found it useful). The program is in a single assembly file, and targets my own home-brew toolchain, but it's totally standard in convention and I expect the code to be acceptable to your preferred 6800 assembler. 

Once you've assembled the file, you have a raw program binary. You can turn this into a cassette image using the [bin2sphere](https://github.com/bzotto/bin2sphere) tool. From there the `.cassette` image file can be turned into a Kansas City standard format `wav` file for use on real hardware, or can be loaded directly into my [Sphere web emulator](https://sphere.computer/emulator). 

The cold start entry point is at `$200`. There is a "warm start" entry at `$205` that does not reset the document area, so if you end up doing a system reset you have a chance at rescuing the document if you come back via `$205`.

## Using Scriptor

It's largely self explanatory. Program starts with an empty document which you type into. The Sphere's Backspace key is the backspace. Move the cursor with the arrow keys if you are using a keyboard that has them (or the emulator), or Control-WASD if not. Control P and L are "page up" and "page down" respectively. Control K and O are line home and end, respectively.  

Control-Y yanks (aka kills) the current cursor line.

Use Control-V to save the current document to cassette and Control-B to load a document from cassette. Both of these will take a long time for nontrivial documents, but the data is compressed to about half the raw text size, so that's something.

Use Control-U to send hard copy output to a SWTPC PR-40 printer attached to port B of the onboard PIA.

## What you need for real hardware

Got a Sphere system and want to write the great American short story? Great! The default program assumes a hardware configuration that includes: 

- PDS-V3N firmware (or compatible; the PDS-V3D? should work too if you have the KBD/1 keyboard connected).
- CRT/1 video board (32x16 lines of text) at $E000 (the default) 
- At least 8K RAM (i.e. a MEM/1 or MEM/6 board installed).
- SIM/1 for cassette with SYS2NF cassette firmware.
- Optional: SWTPC PR-40 or similar parallel printer for hard copy output

## Implementation notes

The program keeps the backing document in an offscreen doubly-linked list of fixed size screen-ready lines. Each line contains a next pointer, previous pointer, and 32 bytes of text data. There is a singly-linked free list and simple alloc/free routines to manage these lists. When edits are made to the document, the altered line(s) are marked dirty via an array of per-line flags. The main edit loop will re-render (copy the full line) any dirty lines after the edit event is done. 

The live word wrap happens during editing. There is a separate "reflow" buffer and collection of routines for insert, delete, carriage return, and other spupporting routeines. Extraneous copies to screen are generally avoided because the cause visible snow. During "normal" typeahead on one line, there is a fast path that doesn't require recopying the edited line every added character.

Cassette I/O routines are deconstructed from the standard SNS2NF read/write block, since the document is neither created nor retrieved as a single flat buffer with explicit size. Instead we do a bunch of work to write from the document data and read into document data. 

To keep document save and load to tape as fast as possible, the program compresses the output using a Huffman-style prefix encoding with a fixed symbol dictionary and tree. This tends to get 50% or better savings in terms of byte count (which directly translates into minutes you are waiting for the cassette recorder).

Printer routine sends to a SWTPC PR-40 printer at `$F042` (port B of onboard PIA). Any parallel printer using similar interface semantics via a PIA can be easily supported with only minor changes. (Something with 80 columns would surely be more useful for longer documents.)

## For more information

Please see my Sphere computer site at https://sphere.computer
You can reach me at bzotto@gmail.com

Thanks!

