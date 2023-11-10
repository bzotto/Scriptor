;------------------------------------------------------------------------------
;
; scriptor.asm
;
; Scriptor for Sphere, a contemporary-ish document text editor for an ancient
; platform that never had one.
;
; Copyright (c) 2023 by Ben Zotto
; https://sphere.computer
;
; I make no claims as to the efficiency or style of the code below. It works
; well enough for my needs but it turns out there's a reason why it took
; a while for "word processing" to become an established kind of nontrivial
; micro software. 
;
; Scriptor loads and runs at address $200. It finds the top of available memory
; (below $E000 of course) to use for working document space. The program does
; live "WYSIWIG" (lol) style document editing with live soft wordwrap, and full
; scrolling cursor. The Sphere Backspace key works in the modern way.
;
; Special commands:
;   Ctrl-WASD will move the cursor if your keyboard doesn't have arrow keys.
;   Ctrl-Y ("yank") will delete the line the cursor is currently on.
;   Ctrl-V will save the current document to cassette
;   Crtl-B will load document from cassette (trashes current document!).
;   Ctrl-P and -L are page up and page down, respectively.
;   Ctrl-K and -O are home/end (of line), respectively
;   
; There's not much hand holding around destructive actions, no undo, and probably 
; a bunch of bugs in the corner cases. CAVEAT SCRIPTOR. :-) Save early, save
; often.
;
; Requires: Sphere system with CRT/1 video at $E000; at least 8K of RAM, but
; as much as you can afford; KBD/2; PDS-V3N firmware and SYS2NF cassette ROM.
;
;
;------------------------------------------------------------------------------

; Memory locations used by PDS-V3N and SYS2NF firmware ROMs.

CSTATS  EQU     $09         ; SYS2-NF Cassette I/O Status
CURSOR  EQU     $1C         ; PDS-V3N Cursor location, [$E000-E200) is valid.
BLKNAM  EQU     $33         ; SYS2-NF Block name to read
NOPRNT  EQU     $3A         ; SYS2-NF Suppress I/O indicator on screen when zero
BFRPTR  EQU     $3C         ; SYS2-NF Buffer for cassette.
BFRSZE  EQU     $3E         ; SYS2-NF Length of cassette buffer.

; Routine addresses in PDS-V3N and SYS2NF firmware ROMs.

WRTHDR  EQU     $FB0B       ; SYS2-NF (internal) Write cassette block header
CASOUT  EQU     $FB62       ; SYS2-NF Cassette byte out
TURNON  EQU     $FB77       ; SYS2-NF Turn cassette motor on
CASIN   EQU     $FB7E       ; SYS2-NF Read cassette byte
RDTLR   EQU     $FB97       ; SYS2-NF Read block trailer from cassette
TRNOFF  EQU     $FBB0       ; SYS2-NF Turn cassette motor off
RDHDR   EQU     $FBB5       ; SYS2-NF Read block header from cassette
HOME    EQU     $FC37       ; PDS-V3N Move cursor to top left of screen
CLEAR   EQU     $FC3D       ; PDS-V3N Clear from cursor onwards
GETCHR  EQU     $FC4A       ; PDS-V3N Input one character 
PUTCHR  EQU     $FCBC       ; PDS-V3N Put character onscreen at the cursor location.

; Constants used in this program

TOPMEM  EQU     $1000       ; Starting address for document storage       
SCREEN  EQU     $E000       ; Starting address of video memory
EOSCRN  EQU     $E200       ; One address beyond the end of video memory
SCRN_W  EQU     $20         ; Width of screen in characters
SCRN_H  EQU     $10         ; Height of screen in characters.
BLNKCH  EQU     $60         ; $60 represents a special "blank" character-- used for non-semantic fill
REALCR  EQU     $0D         ; Actual CR character, allowed in the offscreen document only
NODESZ  EQU     $24         ; Size of each raw linked list entry = SCRN_W + 4

;
; Program variables. These are all located in the direct page, safely above 
; the PDS state management area. They are not in a especially meaningful order
; here, sorry. 
;
; The nature of doing a lot with 16-bit pointers on 6800 is that you end up
; needing reliably temporary storage for 16 bit values, and you can't put them
; on the stack, so there are a bunch of one-offs here. But that's fine, the 
; direct page is not full yet.
;

CPTEMP  EQU     $50         ; Temp storage for copy in and copy out routines.
TEMPST  EQU     $52         ; Scratch location for stack pointer register.
TEMPX   EQU     $54         ; Scratch location for X register. Only for use in "local scope"!!! Calling any routine may trash it.
CMPX    EQU     $56         ; Comparand for 16 bit compares.
DOCHED  EQU     $58         ; Document head pointer
DOCTAL  EQU     $5A         ; Document tail pointer
FREHED  EQU     $5C         ; Free list head pointer
CPYSRC  EQU     $5F         ; Copy source temp pointer
CPYDST  EQU     $61         ; Copy dest temp pointer
CPYEND  EQU     $63         ; Copy end temp pointer
CPYCC   EQU     $65         ; Copy character count
LASTOK  EQU     $66         ; "Last token" ptr used by FNDEOL
RINITL  EQU     $68         ; Reflow count of initial lines to copy-in (1 or 2).
RFLCMD  EQU     $69         ; Reflow edit command (0 = delete, otherwise character to insert)
R1STDS  EQU     $6A         ; Reflow first line address (used for computing next-source, next-dest, and recovering cursor)
RSRCLN  EQU     $6C         ; Reflow next-source (offset from 1st line)
RDSTLN  EQU     $6D         ; Reflow next-destination (offset from 1st line)
RLNSPD  EQU     $6E         ; Reflow lines pending (balance count)
RFLLOC  EQU     $6F         ; Reflow edit location
RFLPTR  EQU     $71         ; Pointer to end of reflow buffer (next avail byte)
DOCSZE  EQU     $73         ; Workspace for document size (not generally valid)
WRTMP1  EQU     $75         ; I/O temp 1
WRTMP2  EQU     $77         ; I/O temp 2
WRCNT   EQU     $79         ; I/O temp count
INSTMP  EQU     $7A         ; Temp insert X val
TEMPX2  EQU     $7C         ; Scratch for X register, can be used across subroutine calls but BE CAREFUL
LLTPRV  EQU     $7E         ; Linked list op working registers x3
LLTNEW  EQU     $80         
LLTNXT  EQU     $82
CSRDLN  EQU     $84         ; Document cursor, line ptr
CSRDIX  EQU     $86         ; Document cursor, index/offset into line.
ALLDRT  EQU     $87         ; Flag to indicate full re-render required.
RNDRLN  EQU     $88         ; Render temp storage
RSCRLN  EQU     $8A         ; Render temp storage
DRTLNS  EQU     $90         ; Dirty lines map [$90-$9F]
EOFMEM  EQU     $A0         ; End of usable memory (determined at start)
SCRDOC  EQU     $A2         ; Pointer to the first visible on-screen document line.

;
; Actual program starts at $200, which is compatible with PDS memory map.
;

        ORG     $200

;
; Load and cold start is at ENTRY. You can try jumping into RESTRT to rescue an
; active document when you've had to reset the system. 
;

ENTRY   LDS     # *-1       ; Ensure the stack is just below the program start. 
        BRA     ENTRY2
RESTRT  JSR     MRKALL      ; You can jump into the program here to recover current document state. 
        BRA     MAIN_     
ENTRY2  JSR     HOMCLR      ; Home & clear the screen
        JSR     SPLASH
        JSR     HOMCLR
        JSR     FNDEOM      ; Find and remember the end of useful memory.
        STX     EOFMEM         
        JSR     RSTDOC      ; Reset and create a new document
        LDX     DOCHED      ; Set the document's head as the top of screen
        STX     SCRDOC      
        STX     CSRDLN      ; Document cursor to top-left (this matches the screen cursor already at start)
        CLR     CSRDIX
        JSR     RSTDRT      ; Reset dirty state        
                
; Top of the main input loop.

MAIN    JSR     GETCHR      ; Get an input character
        
        ; Input substitution 
        CMP A   #'~         ; Replace '~' (on my keyboard) with apostrophe for convenience.
        BNE     CKALPH
        LDA A   #''         
        
        ; If input is a printable character, go directly to the character input processing. 
CKALPH  CMP A   #$21        ; '!'
        BCS     MSPECL
        CMP A   #$5F        ; '_'
        BCS     MAINCH
        
        ; If not, fall through into specials handling.
        
        ; Handle input. Walk the command jump table to see if we find a match.
MSPECL  CLR B
        LDX     #CMDCHR     ; Start at beginning of command table
CHSRCH  CPX     #CMDJMP     ; Did we run off the command list?
        BEQ     NOTFND      
        CMP A   0, X        ; Did the entered character match this entry?
        BNE     CHSRNX  
        ASL B               ; B *= 2 to go from index to address offset
        STA B   CMDLOD+1    ; Patch the indexed LDX below with t
        LDX     #CMDJMP
CMDLOD  LDX     0, X        ; !!SMC!! Load command's jump target
        STX     CMDGO+1     ; Patch the jump instruction below. We need X for the cursor location.
        LDX     CURSOR
CMDGO   JSR     $0000       ; !!SMC!!
        BRA     MAIN_       ; Continue on to the tail of the main loop
CHSRNX  INX                 ; Keep looking...
        INC B
        BRA     CHSRCH

; Bottom of the main input loop. 

MAIN_   JSR     UPDSCC      ; Update the screen cursor (derive it from the document cursor)
        JSR     RENDER      ; Render whatever needs rendering
        BRA     MAIN        ; Go back up to program loop

MAINCH  ; Input was not in the command table. 
        ; Anything else is just a normal character insert.
        JSR     INSCHR
        BRA     MAIN_        
   
NOTFND  BRA     MAIN        ; No command found, just go back for more input.
        
; END of MAIN routine/loop!        

;------------------------------------------------------------------------------
;
; Command and edit handlers.
;
;------------------------------------------------------------------------------

; GOBKSP: Handler for backspace input. 
;
GOBKSP  CLR A                   ; We use 0 as delete marker
        JMP     INSDEL          ; Tail call to the insert/delete routine.
            
; YANKLN: Yank line from the document at the current cursor location. Puts the cursor at the start of the
;   "replacement" line.
;
YANKLN  LDX     CSRDLN          ; Load the cursor document line.
        CPX     SCRDOC          ; Are we going to be deleting the first line on screen? If so we need to handle that.
        BNE     YANK1
        CLR     SCRDOC          ; Wipe the high byte from the screen head ptr so we know to replace it later.
YANK1   JSR     DELL            ; Delete it.
        STX     CSRDLN          ; Cursor gets start of the replacement line
        CLR     CSRDIX           
        LDA A   SCRDOC          ; Check the high byte which we might have cleared as a flag above
        BNE     YANKDN
        STX     SCRDOC          ; If we replaced the top screen line, refresh that here
YANKDN  JSR     MRKFRM          ; Re-render from here down.
        RTS
        
; PAGEUP: Scroll the text downwards one page, moving the cursor to the top left of the screen.
;
PAGEUP  LDX     SCRDOC          
        LDA B   #SCRN_H-1   ; Scroll the text down, at most, 15 lines so there's one line overlap
PU1     LDX     2, X
        BEQ     PUDONE
        STX     SCRDOC
        DEC B
        BNE     PU1
PUDONE  LDX     SCRDOC      ; Reload whatever is the new top of screen line
        STX     CSRDLN      ;   and set that as the new cursor, top-left.
        CLR     CSRDIX
        JSR     MRKALL      ; Trigger a re-render of the whole screen.
        RTS
        
; PAGEDN: Scroll the text upwards one page, moving the cursor to the bottom. This is a much
; trickier bit of logic than page up because we can't just keep going until we run into the 
; NULL end of document. We need to precompute the last desirable top-of-screen line is, which
; means working backwards from the tail. 
        
PAGEDN  LDX     DOCTAL
        STX     TEMPX       ; Track the "last available first line"
        LDA B   #9          ; The minimum number of lines we would leave on one screen
PD1     LDX     2, X        ; line = line->prev
        BEQ     PD2
        STX     TEMPX
        DEC B   
        BNE     PD1
PD2     LDX     SCRDOC
        LDA B   #SCRN_H-1   ; Scroll the text up, at most, 15 lines so there's one line overlap
PD3     CPX     TEMPX       ; Is this the last allowable top line? There's an off by one here based on 
                            ; the minimum line count above but I don't think it will cause a problem
        BEQ     PD4      
        LDX     0, X        ; line = line->next
        BEQ     PD4         ; Did we run out of lines?
        STX     SCRDOC
        DEC B
        BNE     PD3
PD4     LDA B   #SCRN_H     ; Put the cursor at start of the last line of the screen
        LDX     SCRDOC      
PD5     STX     TEMPX
        LDX     0, X
        BEQ     PDDONE      ; handle the end of document case
        DEC B
        BNE     PD5         ; handle the end of count case
PDDONE  LDX     TEMPX       ; load back the last onscreen line
        STX     CSRDLN      ; move the cursor there
        CLR     CSRDIX
        JSR     MRKALL
        RTS

; UPDSCC: Update screen cursor. This routine locates the document cursor relative to the current
; screen window, and sets the screen cursor to the matching screen location. This routine will
; look one line above the screen through one lines after in case the cursor has moved offscreen.
; If found offscreen, the screen top will be updated accordingly!
;
UPDSCC  LDX     CSRDLN      
        STX     TEMPX       ; This is the node ptr we are looking for.
        LDX     SCRDOC      ; Start cursor search at head of document 
        LDX     2, X        
        BEQ     UPDSCR      ; No prev pointer , move on to checking the screen. 
        CPX     TEMPX       ; Is the cursor on this line above the screen?
        BNE     UPDSCR      ; Nope, continue on to the screen.
        STX     SCRDOC      ; Cursor is above the screen! Reset the head of the screen
        JSR     MRKALL      ; Mark the whole screen for a render. FALL INTO the normal screen case!
UPDSCR  CLR B               ; B = 0 to track line index.
        LDX     SCRDOC      ; Start checking screen lines with the head of document.
UPSDN2  CPX     TEMPX       ; Is this the line we're looking for?
        BEQ     UPSFND      ; Yes, we're done!
        LDX     0, X        ; Indirect through the next pointer
        BEQ     CNTNFD      ; Null next pointer means no more lines to search-- leave cursor where it is.
        INC B               ; Incrememnt the line count.
        CMP B   #SCRN_H     ; Did we already look through the whole screen without finding?
        BEQ     UPDBOT      ;  If so, check the bottom line.
        BRA     UPSDN2
UPDBOT  CPX     TEMPX       ; X is either the next line after the screen, or possibly NULL. Either way,
                            ;  we are checking to see whether it's what we're looking for.
        BNE     CNTNFD      ; Hm.
        ; Need to scroll the screen up one line and restart!
        LDX     SCRDOC
        LDX     0, X
        STX     SCRDOC      ; Save the new top of screen
        JSR     MRKALL      ; Mark the whole screen for rendering
        DEC B               ; Back up the index one line because the cursor is now on the last line, and FALL THROUGH
UPSFND  CLR A               ; This looks like a terribly brutish way of composing the
        ASL B               ;  final cursor address but it works, has no branches, and
        ASL B               ;  is actually faster than constructing a table lookup!
        ASL B
        ASL B
        ASL B
        ROL A
        ORA A   #$E0
        ORA B   CSRDIX
        STA A   CURSOR
        STA B   CURSOR+1
CNTNFD  RTS

;------------------------------------------------------------------------------
;
; Live word wrap / reflow routines.
;
;------------------------------------------------------------------------------

; INSCHR: Insert (non-whitespace) character. This is the simplest reflow because it only shifts
; forwards. There's also an optimization path for straight typeahead patterns. 
;
INSCHR  ; Before we do the full insert flow, check to see if we qualify for fast-path insert, which
        ; is viable if (a) the current character under the cursor is a hard or soft CR ie, we are
        ; at the end of a *logical* line and (b) we are not in the final column of the *physical* line, 
        ; so we know there's enough room for at least one more character that won't trigger any
        ; wrapping.
        ;
        LDA B   CSRDIX
        CMP B   #SCRN_W-1   ; are we in the final column?
        BEQ     INSGO       ; yes we are, so we fail the fast path
        JSR     RDCHR       ; ok, look at char under cursor
        CMP B   #$0D        ; is it a hard CR?
        BEQ     INSFP       ; yes? we qualify!
        CMP B   #BLNKCH     ; is it a blank?
        BNE     INSGO       ; no? we don't qualify!
INSFP   LDX     CURSOR      ; Do NOT mark for render, instead do a literal...
        LDA B   0, X        ;   ...screen update to avoid video snow. [Gross that this is here]
        STA B   1, X        ; The document cursor will already be updated and when
        STA A   0, X        ; the tail of MAIN runs it will get updated.
        JMP     INSHFT      ; FAST PATH>> Insert-shift 
INSGO   STA A   RFLCMD      ; Save the character to insert
        JSR     CSRPTR      ; Get the current document cursor address.
        JSR     MRKCUR      ; "Mark" the character currently under the cursor, so we can recover it later.
        STX     RFLLOC      ; Reflow edit address is at the cursor location 
        JSR     SREFLW      ; Setup the reflow state context. 
        JSR     RCPYIN      ; Run COPY-IN once to start with.
INSLP   JSR     RCPOUT      ; COPY-OUT-SHIFT
        JSR     RBUFMT      ; is the reflow buffer empty?
        BEQ     INSDN       ;  if yes, we are done!
        JSR     RBUFCR      ; is there a hard CR in the buffer?
        BEQ     INSLP       ;  if yes, don't copy in more text, just continue 
        JSR     RCPYIN      ; copy more text in 
        BNE     INSLP       ; back to the top of the loop if copyin did something
        ; If we ran out of lines to copy in, then copy out until nothing left to copy
DRAIN   JSR     RCPOUT      ; COPY-OUT-SHIFT
        JSR     RBUFMT      ; is the reflow buffer empty?
        BNE     DRAIN       ;  if yes, fall through to finish.        

INSDN   JSR     RECCUR      ; recover the updated cursor (find it in the document)
        RTS
        
        
; INSDEL: Superset insert or delete a character, and manage the text reflow that results. 
;   This is used for all deletes, and the insertion of space characters.
; A contains character to insert, or zero for delete.
; X contains the current cursor position.
;
INSDEL  STA A   RFLCMD      ; Save the character to insert
        JSR     CSRPTR      ; Get the cursor's address
        JSR     MRKCUR      ; "Mark" the character currently under the cursor, so we can recover the cursor later.
        TST     RFLCMD      ; Is this actually an insert (space) or delete?
        BNE     INSINS
        LDX     CSRDLN
        LDA B   CSRDIX
        JSR     FNDPRV      ; For a delete, the reflow address is cursor-1, but because it could span lines, we need a helper routine to find it.
        JSR     FQP2X       ; Turn the FQ address into a one-off X value.
INSINS  STX     RFLLOC      ;  ... which is the reflow location.
        JSR     SREFLW      ; Setup the reflow state context. 
        
        ; *** Note that this is the destination for a mid-routine jump in when inserting a hard CR. See INSCR below.
IDCIN   JSR     RCPYIN      ; Run COPY-IN as many times as setup tells us we need to.
        DEC     RINITL
        BNE     IDCIN       ; We want to enter the main loop with this temp value back to zero.        

IDCLP   JSR     RBUFCR      ; Does the buffer contain a hard CR?
        BEQ     IDRAIN      ;   If so, no more copy ins, just drain the buffer.
        JSR     RBUF1L      ; Does the buffer contain less than one line's worth of text?
        BEQ     IDCLP1      ;   If so, jump to copy more text in
        JSR     RCPOUT      ;   If not, and there's definitely more, then we can safely copy this out
        BRA     IDCLP       ; Go back to the top of the loop
IDCLP1  JSR     RBUFMT      ; We want to bring in more text, but if the buffer is empty, we need to stop. 
        BEQ     IDRAIN      ;   If buffer is empty here, a copy in won't do anything for us us. (This probably shouldn't happen.)
        JSR     RCPYIN      ; Bring in more text and go back to top of loop.
        BNE     IDCLP       ; if copyin runs out of lines, drop through into the drain

IDRAIN  JSR     RCPOUT      ; Copy out a line (we know there's *at least* a hard CR in there...)
        JSR     RBUFMT      ; is the work buffer empty yet?
        BNE     IDRAIN      ; Not yet, keep copying out.     
        
IDCHFN  TST     RLNSPD      ; are there any lines pending remaining? if so, we need to delete!
        BEQ     IDDONE      ; no, we're actually done
        JSR     MRKALL      ; If we're deleting lines, just repaint the whole screen rather than trying to figure out 
                            ; what's happening.
        JSR     RNXDST      ; ok, delete at next-destination
IDDLLP  CPX     #0          ; is this the end of document? we can't delete that...
        BEQ     IDDONE      
        JSR     DELL        ; This routine returns the value of the next line that takes the deleted line's place.    
        DEC     RLNSPD      ; decrement the count
        BNE     IDDLLP      ; and keep going until we're really done.
IDDONE  JSR     RECCUR      ; recover the updated cursor (find it onscreen)
        RTS

; INSCR: Insert CR character. This is a weird special case!
;
INSCR   JSR     INSDEL      ; Run the insert/delete with the hard CR insert. This will work, but will not correctly
                            ; reflow the newly created following paragraph.
        LDX     CSRDLN      ; "Start" reflow on the newly created cursor line.                
        STX     R1STDS      
        JSR     MRKFRM      ; Mark from new line to end of screen for render. The line above will have already been marked.
        CLR     RSRCLN
        CLR     RDSTLN 
        INC     RINITL      ; Indicate just one copy-in to begin with.
        JSR     CSRPTR      ; Mark the cursor so the tail of the insert-delete will recover it correctly.
        JSR     MRKCUR
        JMP     IDCIN       ; [TC] Jump midway back into the routine without doing a re-setup, except for indicating the one copy-in.

; SREFLW: Setup state for the reflow operation. This changes based on whether we're already
; on the first line of the screen, or of the paragraph, and for what function and input this is. 
;   X contains the start address of the cursor's line.
;
SREFLW  LDA A   #1          ; Start by assuming one intiial copy-in
        STA A   RINITL
        LDX     CSRDLN      ; Load the line node of the cursor's line.
        CPX     DOCHED      ; Are we on the first line of the whole document?
        BEQ     SREF3       ;  Yes? Then no matter what we are starting on the given line.
        JSR     ISPARA      ; Is the start of a parargaph? if so, can't start earlier.
        BNE     SREF1       ;  Not para first line
        TST     CSRDIX      ; first line but is it first actual char?  
        BNE     SREF3       ; no.. go right to same-line
SREF1   LDA A   RFLCMD      ; Is this a delete?
        BEQ     SREF2       ; If so, we definitely are going to start early.
        CMP A   #' '        ; If this is an insert, are we inserting whitespace?
        BEQ     SREF2       ;  (space or CR) If so, also start early.
        CMP A   #$0D        ; 
        BNE     SREF3       ; Go to normal same-line insert.
SREF2   INC     RINITL
        STX     TEMPX
        LDX     2, X        ; Go back a line when called for.
        BNE     SREF3     
        DEC     RINITL      ; If DECLIN returned 0000 (can't go back farther), then nevermind
        LDX     TEMPX       ; Load back the line we failed to go back from.
SREF3   STX     R1STDS      ; Save as first-destination, too.
        CLR A
        STA A   RDSTLN      ; Reset destination line offset.
        STA A   RSRCLN      ; Reset source line offset
        STA A   RLNSPD      ; Reset lines-pending
        LDX     #RFLBUF     ; Reflow pointer to start of reflow buffer
        STX     RFLPTR      
        JSR     CLRRFL      ; Clear the reflow buffer
        RTS

; RCPYIN: COPY-IN routine for reflow.
; This routine on return sets the zero flag if there were no more source lines to copy in.
; (Z flag will be clear on return in normal operation)
;   
RCPYIN  JSR     RNXSRC      ; Get next source line start
        CPX     #0          ; Compare to zero.
        BNE     RCIN0
        RTS                 ; There's literally nothing to do if there's no text left.
RCIN0   JSR     LNTEXT      ; Offset X to the start of the content.
        STX     CPYSRC      ; Save that as our copy source 
        JSR     ADDLN       ; .. and compute the copy endpoint, which is 32 chars later
        STX     CPYEND
        LDX     CPYSRC      ; Load back copy source to start working        
RCIN1   CPX     RFLLOC      ; Is this the location of the edit?
        BNE     RCIN2   
        CLR     RFLLOC      ; Corrupt the edit location (high byte to zero) so we never match again! 
        LDA A   RFLCMD      ; Test the command
        BNE     RCIN3       ; For insert, we skip past the source load and go direct to the paste
        INX                 ; For delete, we want to omit the character we'd normally load, so we increment the source past it
        CPX     CPYEND      ; .. but first check that we're not *skipping* the end of this line as a tiny corner case here.
        BEQ     RCINDN      ;   yes? wow, what are the odds?? (they exist). so .. we're done. 
                
RCIN2   LDA A   0, X        ; load character to copy
        CMP A   #BLNKCH     ; We copy up to the first soft CR we find, and then we stop.
        BEQ     RCINDN
        INX                 ; Update and save the source ptr 
        STX     CPYSRC  
RCIN3   LDX     RFLPTR      ; Load the destination ptr
        STA A   0, X        ; Store the character
        INX                 ; Incrememt and save it.
        STX     RFLPTR      
        LDX     CPYSRC      ; Load back the source pointer and start over
        CPX     CPYEND      ; .. but first check that we didn't walk off the end of this line.
        BNE     RCIN1
RCINDN  INC     RSRCLN      ; Increment the source-line offset
        INC     RLNSPD      ; Increment lines-pending.
        RTS                 ; Because the previous line is always incremeting, the Z flag will always be clear.
        
; RCPOUT: COPY-OUT-AND-SHIFT routine for reflow
;
RCPOUT  TST     RLNSPD      ; Has lines-pending fallen to zero?
        BNE     RCO2
        JSR     RNXDST      ; Insert a line at the next-destination
        JSR     INSL
        JSR     MRKFRM      ; Mark the rest of the page as needing refresh, since we are inserting a line.
        INC     RSRCLN      ; Increment next-source by one line to compensate.
        INC     RLNSPD      ; Increment lines-pending 
RCO2    ; Perform the copy-out
        LDX     #RFLBUF     ; Load buffer start address
        STX     CPYSRC      ; Stash this as the copy start point.
        JSR     FNDEOL      ; Find the "EOL" of what's in the reflow buffer
        STX     CPYEND      ; Remember that as copy end
        JSR     RNXDST      ; Copy dest = next-destination
        JSR     MRKDRT      ; Mark this line for re-render before doing the copy.
        JSR     LNTEXT      ; Destination is in the document so start where content is.
        STX     CPYDST
        STX     CPTEMP
        JSR     COPY2       ; COPY2 counts bytes.         
        PSH B               ; Stash the number of bytes actually copied, we'll use later.
        DEC     RLNSPD      ; Decrement lines-pending
        INC     RDSTLN      ; Increment next-destination
        ; Prepare to fill out the rest of the space in the destination line with blanks.
        STX     CPYSRC      ; Save the prior copy endpoint because we will need it for the shift.
        LDX     CPTEMP      ; Load back the original copy dest which was RNXDST
        JSR     ADDLN
        STX     CPYEND      ; Save the dest+32 pointer for end compare
        LDA A   #BLNKCH
        LDX     CPYDST      ; This will already be one past the end of the copied data.
RC03    CPX     CPYEND      ; Done blanking to end of line? 
        BEQ     RCO4
        STA A   0, X
        INX
        BRA     RC03
        ; Setup for left shift of the reflow buffer. Conveniently, the previous copy end, which was mirrored in 
        ; copy source is now the start of the shift copy source, so it's already set.
RCO4    LDX     RFLPTR      ; Use the end of buffer pointer as the copy (shift) end mark.
        STX     CPYEND
        PUL B               ; Get back number of bytes copied.
        JSR     DEC16B      ; Decrement this pointer by the number of bytes we're shifting.
        STX     RFLPTR      ; Save the new end of buffer pointer.
        LDX     #RFLBUF     ; Destination is of course the start of the copy buffer
        STX     CPYDST
        JSR     COPY2       ; Do the shift
        ; Fill remaining buffer with soft CRs.
        LDA A   #BLNKCH
        LDX     RFLPTR
RCO5    CPX     #RFLEND      
        BEQ     RCO6
        STA A   0, X
        INX
        BRA     RCO5
RCO6    RTS                 ; Finally we are done with our copy and shift.        

; ISPARA: Is this the first line of a paragraph? It is if the prior line ends
;  in a hard CR. X contains just a line start address.
;
ISPARA  STX     TEMPX2
        CLR B               ; FNDPRV takes a fully qualified pointer, so needs an index too.
        JSR     FNDPRV      
        JSR     LNTEXT
        STA B   ISPAR1+1    ; [SMC] Patch the following load 
ISPAR1  LDA A   0, X        ; [SMC] Load the X (content) + offset
        LDX     TEMPX2
        CMP A   #$0D
        RTS

; FNDPRV: Finds the previous valid character address prior to the given X address.
;   X is a line start address, and B is an index into the line.  
;   The returned pointer in X is a line start address and B is an index pointer into 
;   that line.
;
FNDPRV  STX     TEMPX       ; Remember the actual line address
        JSR     LNTEXT      ; Move X to the start of the content
FPONS   CMP B   #0          ; Are we already at index 0?
        BEQ     FPGOUP
        DEC B
        STA B   FPCHK+1     ; [SMC] Patch the load below
FPCHK   LDA A   0, X        ; [SMC] Load the correct index.
        CMP A   #BLNKCH     ; Is this just blank?
        BEQ     FPONS       ; Keep scanning left
        LDX     TEMPX       ; Found a valid character. Restore the line address.
        RTS
FPGOUP  LDX     TEMPX
        LDX     2, X        ; get prev line pointer.
        BNE     FPGU1
        LDX     TEMPX       ; Reload the temp pointer and B will be 0.
        RTS
FPGU1   LDA B   #SCRN_W     ; Set B just beyond the new content.
        BRA     FNDPRV      ; Go back to the start of the routine.

; FQP2X: Fully qualified pointer to X address. Assumes line start pointer in
; X and character offset in B.
;
FQP2X   STX     TEMPX
        CLR A
        ADD B   #4
        ADD B   TEMPX+1
        ADC A   TEMPX
        STA A   TEMPX
        STA B   TEMPX+1
        LDX     TEMPX
        RTS
        
; RBUFMT: Is the reflow buffer empty? 
;
RBUFMT  LDA A   RFLBUF
        CMP A   #BLNKCH
        RTS

; RBUFCR: Does reflow buffer contain a hard CR?
;
RBUFCR  LDX     #RFLBUF
RBCRLP  LDA A   0, X
        AND A   #$7F     ; strip off high bit, if present.
        CMP A   #$0D
        BEQ     RBCRDN 
        INX
        CPX     #RFLEND  ; !!! the faster way to do this is to check for soft CR and bail early
        BNE     RBCRLP
        LDA A   #1      ; no CR found so clear the zero flag
RBCRDN  RTS

; RBUF1L: Does reflow buffer contain only one line's worth of text if any?
; 
RBUF1L  LDA A   RFLBUF+SCRN_W
        CMP A   #BLNKCH
        RTS

; CLRRFL: Clear the reflow buffer
;
CLRRFL  LDA A   #BLNKCH
CLRRF2  STA A   0, X
        INX
        CPX     #RFLEND
        BNE     CLRRF2
        RTS 

; RNXSRC: Return the current source reflow line. 
;        
RNXSRC  LDX     R1STDS
        LDA B   RSRCLN
        BRA     RLNCLC      ; tail call to the helper routine

; RNXDST: Return the current destination reflow line. This one takes the structure of a classic
; for loop!
;
RNXDST  LDX     R1STDS
        LDA B   RDSTLN
        BRA     RLNCLC      ; tail call to the helper routine

; RLNCLC: Line calculation helper routine used by the above routines
;
RLNCLC  BEQ     RNCLDN      ; test B == 0
        LDX     0, X        ; line = line->next
        DEC B
        BRA     RLNCLC
RNCLDN  RTS

; FNDEOL: Find the end of a logical text line in a string. 
;  X contains the starting pointer. Result will be in X-- one past the EOL.
;  A logical line contains: all the tokens that can fit entirely within a line.
;  A carriage return character prior to the line length will automatically terminate it,
;  or of course if we run out of characters/tokens to examine.
;  A nonbreaking token is : An alphanumeric string followed by one space.
;
FNDEOL  CLR     LASTOK      ; Set LASTOK to null (no current token)
        CLR     LASTOK+1
        LDA B   #SCRN_W     ; Use B as count of characters examined. 
FELOOP  LDA A   0, X
        AND A   #$7F        ; !!! remove the high bit if present.
        CMP A   #$0D        ; A hard CR is an automatic end
        BEQ     FEDN1
        CMP A   #BLNKCH        ; A soft CR means we've already exceeded the valid data
        BEQ     FEDN0
        CMP A   #'          ; Check space characters
        BNE     FELALP      ; Not a space, jump to normal alphanumeric token processing
        
        STX     TEMPX       
        LDX     LASTOK      ; last token == NULL?
        BNE     FELP1       ;  no, we're inside a token, so this is its final char
        BRA     FEEOL0      ; yes, we're not inside a token, so ignore this space and keep going
        
FELP1   LDX     #0  
        STX     LASTOK
        BRA     FEEOL0

FELALP  STX     TEMPX       
        LDX     LASTOK      ; last token == NULL?
        BNE     FEEOL0      ; yes? jump to loop bottom
        LDX     TEMPX       ; no,
        STX     LASTOK      ;   set last token = current location
        BRA     FEEOL       ; jump to loop bottom
        
FEEOL0  LDX     TEMPX
FEEOL   INX
        DEC B
        BNE     FELOOP

         ; By definition if we've fallen out of the loop above, we ran out of characters either in the middle of a token
         ; Or in the middle of a run of free ranging spaces. In the former case, lastTokStart will be the desired 
         ; return value (one past the end of the last full token). In the latter case, it's the current pointer plus one.

        STX     TEMPX       
        LDX     LASTOK      ; check to see if last token is NULL (we're not in a token)
        BNE     FEDN0       ; if not, then last token ptr is the result
        LDX     TEMPX       ; but if it was NULL, then reload X value and return that +1
        BRA     FEDN0       ; (ben the optimizer sez: save a byte by replacing this with a DEX instruction!) 
FEDN1   INX                 ; Increment to one-beyond the final character.
FEDN0   RTS


;------------------------------------------------------------------------------
;
; Screen render and supporting functions.
;
;------------------------------------------------------------------------------

; RENDER: Main render routine for the screen. Relies on dirty flags being set by
; editing operations correctly. This routine walks through an index for every line
; on the screen. For each one, it checks the dirty state (which is either flagged for
; the individual line, or overridden with a master screen-dirty flag) and either
; skips the line or renders it. Rendering is just a transform copy from the backing
; document. In the case of screen lines that exist beyond the end of the document,
; if marked dirty, the line will be blanked out.
;
; We want to be carefully conservative in copying to screen-- or blanking
; spare lines, even when it's a no-op-- the video circuit creates "snow" with every
; write to video RAM, so we try hard to keep writes to a minimum.
;
RENDER  LDX     #SCREEN
        STX     RSCRLN      ; Save the starting screen position.
        LDX     SCRDOC     
        CLR A               ; A counts the line index
RNDR2   STX     RNDRLN      ; Keep the render line pointer available for this whole loop
        JSR     ISDRTY      ; Is this line marked dirty?
        BNE     SKPLN
        CPX     #0          ; Is the line pointer null? That means we've run off the end of
        BNE     RNDR3       ;   the document, but not the screen.
        LDX     RSCRLN      ; For a dirty blank line, load back the screen address 
        JSR     BLNKLN      ; Blank it out
        BRA     SKPLN       ; Skip over the text copy.
RNDR3   JSR     LNTEXT      ; Move the pointer to the text section
        STX     CPYSRC
        LDX     RSCRLN
        STX     CPYDST
        PSH A
        JSR     CP2SCR      ; Run the special copy routine to render to screen
        PUL A
SKPLN   INC A               ; Screen index++
        CMP A   #SCRN_H     ; did we complete the render?
        BEQ     RNDRDN
        LDX     RSCRLN      ; Screen ptr += 32
        JSR     ADDLN
        STX     RSCRLN
        LDX     RNDRLN      ; Reload the line pointer.
        BEQ     RNDR2       ; If the line pointer is already NULL, just jump back
        LDX     0, X        ; Otherwiuse load the next line before jumping.
        BRA     RNDR2               
RNDRDN  BSR     RSTDRT      ; Reset the dirty state.
        RTS

; MRKDRT: Mark a line as dirty (X is a document line address)
;
MRKDRT  STX     TEMPX       ; This is the line address we will mark if we find it.
        CLR A
        LDA B   #SCRN_H
        LDX     SCRDOC
MD0     CPX     TEMPX
        BEQ     MDFND
        LDX     0, X        ; ptr = ptr->next
        BEQ     MDNTFD      ; End of document, not found.
        INC A
        DEC B
        BNE     MD0         ; End of screen, not found.
MDNTFD  LDX     TEMPX       ; Restore the original X value
        RTS                 ; Not found, just return
MDFND   STA A   MDFND1+1    ; [SMC] Patch the CLR instruction below.
        LDX     #DRTLNS
MDFND1  CLR     0, X        ; Zero out the relevant line flag.
        LDX     TEMPX       ; Restore the original X value
        RTS
        
; MRKALL: Marks entire screen dirty. This triggers a (somewhat faster) complete re-rendering 
; of the screen.
;
MRKALL  CLR     ALLDRT
        RTS
        
; MRKFRM: Mark dirty from the given line address to the end of the screen. Used for kills,
; as well as insertions of carriage returns.
;
MRKFRM  STX     TEMPX       ; This is the starting line address we will mark if we find it.
        CLR A               ; A counts up, line count
        LDA B   #SCRN_H     ; B counts down
        LDX     SCRDOC
MFF0    CPX     TEMPX
        BEQ     MFFND
        LDX     0, X        ; ptr = ptr->next
        BEQ     MFNTFD      ; End of document, not found.
        INC A
        DEC B
        BNE     MFF0        ; End of screen, not found.
MFNTFD  LDX     TEMPX       ; Not found. Restore the original X value
        RTS                 ; 
MFFND   STA A   MFFND1+1    ; [SMC] Patch the CLR instruction below.
MFFND0  LDX     #DRTLNS
MFFND1  CLR     0, X        ; Zero out the relevant line flag.
        INC A
        DEC B               
        BEQ     MFFND2
        STA A   MFFND1+1    ; [SMC] Patch the CLR insrtruction above.
        BRA     MFFND1
MFFND2  LDX     TEMPX       ; Restore the original X value
        RTS
        
; ISDRTY: Is the LINE INDEX in A dirty? Does a fancy dance to preserve all registers.
;
ISDRTY  PSH B
        LDA B   ALLDRT
        BEQ     ISDRDN
        STX     TEMPX
        PSH A
        STA A   ISDRT1+1
        LDX     #DRTLNS
ISDRT1  TST     0, X        ; Is the indexed dirty byte zero? (0 = = dirty)
        TPA                 ; Freeze the comparison result from the test
        LDX     TEMPX       ; Load back the X register
        TAP                 ; Restore the condition codes 
        PUL A               ; Restore the original A value (no condition codes change)
ISDRDN  PUL B               ; Restore B
        RTS
        
; RSTDRT: Clear the dirty flags. Brute force since there are only 16 bytes to mark.
; The flag is inverted here for easy comparison and update: zero means dirty, anything else
; (FF) means unflagged.
;
RSTDRT  LDX     #$FFFF
        STX     DRTLNS
        STX     DRTLNS+2
        STX     DRTLNS+4
        STX     DRTLNS+6
        STX     DRTLNS+8
        STX     DRTLNS+10
        STX     DRTLNS+12
        STX     DRTLNS+14
        LDA A   #$FF
        STA A   ALLDRT
        RTS

; CP2SCR: Copy a full line from the document to somewhere on the screen, performing 
; any character transformations necessary. (Currently just CR to blank)
;
CP2SCR  STX     TEMPX
        LDA B   #SCRN_W
CP1     LDX     CPYSRC
        LDA A   0, X
        INX 
        STX     CPYSRC
        ; Perform transform(s) here.
        CMP A   #$0D        ; \n -> (BLANK)
        BNE     CP2
        LDA A   #BLNKCH
        ; End transforms
CP2     LDX     CPYDST
        STA A   0, X
        INX
        STX     CPYDST
        DEC B
        BNE     CP1
CPDONE  LDX     TEMPX
        RTS

;------------------------------------------------------------------------------
;
; Document memory management.
;
;------------------------------------------------------------------------------

; MEMCLR: The raw memory preparation routine. Takes everything between TOPMEM and EOFMEM
;  and turns it into a linked free list. Free list nodes are singly-linked, forward through
;  their next pointer (first two bytes). 
;           
MEMCLR  LDX     #0          ; At memory clear there exists is no document at all.  
        STX     DOCHED  
        STX     DOCTAL    
        LDX     #TOPMEM     ; Start the free list at the top of memory.
        STX     FREHED
MCCLR0  STX     MCCLR1+1    ; [SMC] Patch the STX below so we are storing the results of the add 
        BSR     MCLR2       ; Compute the next node address (X += 36)
        STX     CMPX
        LDX     EOFMEM
        JSR     CMP16       ; EOFMEM - new ptr -> underflow? (returns with new ptr in X)
        BCC     MCCLR1      ; If no underflow, go store the next ptr
        LDX     #0          ; If we're done, store a NULL instead.
MCCLR1  STX     $FFFF       ; [SMC] Store the next ptr into this node's first two bytes.   
        BNE     MCCLR0      ; If that wasn't the end, go back and create the next node.
        RTS
        
; Helper routine. I guess we could fold this back into the routine above but for clarity 
; I have it here right now.
MCLR2   STX     TEMPX
        CLR A
        LDA B   #NODESZ
        ADD B   TEMPX+1
        ADC A   TEMPX
        STA A   TEMPX
        STA B   TEMPX+1
        LDX     TEMPX
        RTS

; ALLOCL: Allocate a single line from the free list. This returns a pointer to the entire
;  node and doesn't set anything in its pointers or data. The caller must do that.
;
ALLOCL  LDX     FREHED      
        BEQ     NOMEM       ; If the free pointer is NULL, we're in trouble.
        STX     TEMPX
        LDX     0, X        ; Pull out the next pointer, which is the new free head.
        STX     FREHED
        LDX     TEMPX
NOMEM   RTS         

; FREEL: Free a line node, adding it to the head of the free list. 
;
FREEL   STS     TEMPST      ; Save the stack pointer so we can use it as a 16bit register
        LDS     FREHED      ; S = pointer to old head node 
        STS     0, X        ; Set the next pointer of the newly freed node to the previous head
        STX     FREHED      ; Make the newly freed node to the head of the list.
        LDS     TEMPST      ; Restore SP
        RTS     
    
;------------------------------------------------------------------------------
;
; Line list management.
;
;------------------------------------------------------------------------------

; RSTDOC/NEWDOC: Reset the current document. Clear memory into a free list, and
;  create a single line containing a CR in the document. Jumping to NEWDOC will
;  create and install the intial line, assuming the free list is already set up.
;
RSTDOC  BSR     MEMCLR      ; Reset the allocator
NEWDOC  BSR     ALLOCL      ; Allocate the initial line. 
        STX     DOCHED      ; This will be the head and tail of the document.
        STX     DOCTAL
        CLR     0, X        ; Next pointer is NULL.
        CLR     1, X
        CLR     2, X        ; Prev pointer is NULL.
        CLR     3, X
EMTLIN  BSR     LNTEXT      ; Move to the data section  ** External jump target
        JSR     BLNKLN      ; Blank out the 32 bytes of data.
        LDA A   #$0D        ; Set a CR as the first character in this line.
        STA A   0, X
        RTS
    
; PRVLIN: Get the previous line handle given the line handle given by X.
; 
PRVLIN  LDX     2, X
        RTS                 ; Return the NULL

; NXTLIN: Get the next line handle given the line handle given by X.
;
NXTLIN  LDX     0, X
        RTS
    
; LNTEXT: Node text. Jump X from a node pointer to its content area
; (ie, past the next/prev pointers)
;
LNTEXT  INX
        INX
        INX
        INX
        RTS
        
;------------------------------------------------------------------------------
;
; Document cursor management.
;
;------------------------------------------------------------------------------

; CSRPTR: Internal helper routine to computer the address (in X) of the current cursor
;   This takes a nontrivial number of cycles so it should only be used when a calling
;   routine needs to both access and shift the cursor in lockstep. If you just need to
;   examine or write the single character location, there are faster ways to do it.
;
CSRPTR  LDA B   CSRDLN+1
        LDA A   CSRDLN
        ADD B   #4         
        ADC A   #0
        ADD B   CSRDIX
        ADC A   #0
        STA A   TEMPX
        STA B   TEMPX+1
        LDX     TEMPX
        RTS
                      
; LSTLPT: Last in line pointer. Given a node pointer X, return pointer to one beyond
; the last character.
;
LSTLPT  JSR     TXAB
        ADD B   #SCRN_W+4   
        ADC A   #0
        JMP     TABX
                
; CSRLFT: Move the document cursor left by one valid position.
;
CSRLFT  BSR     CSRPTR      ; Get the actual cursor ptr
        LDA B   CSRDIX      ; Use B to hold the index within the line.
CSRLF_                      ;  *** ALTERNATE ENTRY POINT *** 
CSRLF0  DEC B               ; Decrement the index.  
        BMI     CSRLF1      ; Did we underflow the line?
        DEX                 ; OK to move ptr back.
        LDA A   0, X        ; Examine the character.
        CMP A   #BLNKCH     ; Is this a blank?
        BNE     CSRLFD      ; If not, then we landed on a valid character and we're done.
        BRA     CSRLF0      ; If that was a blank, back up another index.
CSRLF1  LDX     CSRDLN      ; We underflowed so need to go back a line, if possible. 
        LDX     2, X        ; Get previous line pointer
        BEQ     CSRFL2      ; No previous line means we stop at first char on current line
        STX     CSRDLN      ; Save the new current cursor pointer.
        BSR     LSTLPT      ; Get the X pointer back in order.
        LDA B   #SCRN_W     ; Get the B index back in order (will predecrement above)
        BRA     CSRLF0      ; Go back into the scanning loop.
CSRFL2  CLR B
CSRLFD  STA B   CSRDIX      ; Save the new cursor index.
        RTS
        
; CSRRT: Move the document cursor to the right by one valid position.
;
CSRRT   BSR     CSRPTR      ; Get the actual cursor ptr
        LDA B   CSRDIX      ; Use B to hold the index within the line.
        INC B               ; Pre-increment the pointers 
        INX
CSRRT0  CMP B   #SCRN_W     ; Did we overflow the line?
        BEQ     CSRRT1      ; We overflowed
        LDA A   0, X        ; Examine the character.
        CMP A   #BLNKCH     ; Is this a blank?
        BNE     CSRRTD      ; If not, we landed on a valid character and we're done.
        INC B
        INX
        BRA     CSRRT0
CSRRT1  LDX     CSRDLN      ; We overflowed so need to go forward a line, if possible. 
        LDX     0, X        ; Get next line pointer        
        BEQ     CSRRT2      ; No next line! Uh oh. We need to go back to where we were.
        STX     CSRDLN      ; Save the new cursor line.
        JSR     LNTEXT      ; Set up the pointer to start of text area
        CLR B               ; Reset index to zero.
        BRA     CSRRT0      ; Go back into the loop
CSRRT2  LDX     CSRDLN      ; Get X and B setup to go back into the leftwards scan.
        JSR     LSTLPT      ;  X to one past end of the line.
        LDA B   #SCRN_W     ;  B to one past end of the line.
        JMP     CSRLF_      ; Go left from here to find the end of this line. That's a very
                            ; roundabout way of getting back to where we started, but the code is simpler
                            ; and it's an uncommon case.
CSRRTD  STA B   CSRDIX
        RTS

; CSRUP: Move the document cursor up by one line. 
;
CSRUP   LDX     CSRDLN
        LDX     2, X        
        BEQ     CSRUP2      ; No previous line!
        STX     CSRDLN      ; Save the new line.
        JSR     CSRPTR      ; Get the X ptr location of the cursor
        INX                 ; Pre-increment it for the leftwards scan.
        LDA B   CSRDIX      ; Load the current index
        INC B               ; Pre-increment it so we can run the left scan.
        JMP     CSRLF_      ; Scan left from this location.
CSRUP2  CLR     CSRDIX      ; can't go up anymore, just move cursor to start of line.  
        RTS

; CSRDN: Move the document cursor down by one line.
;
CSRDN   LDX     CSRDLN
        LDX     0, X        ; Get the next line ptr
        BEQ     CSRDN1      ; No next line! 
        STX     CSRDLN      ; Save the new line
        JSR     CSRPTR      ; Get the X ptr location of the cursor
        INX                 ; Pre-increment it for the leftwards scan.
        LDA B   CSRDIX      ; Load the current index
        INC B               ; Pre-increment it so we can run the left scan.
        JMP     CSRLF_      ; Scan left from this location.        
CSRDN1  LDX     CSRDLN      ; Reload the current line ptr   
        JSR     LSTLPT      ;  X to one past end of the line.
        LDA B   #SCRN_W     ;  B to one past end of the line.
        JMP     CSRLF_      ;  Left scan!

; CSRHOM: Move the document cursor to the start of the current line.
;
CSRHOM  CLR     CSRDIX
        RTS
        
; CSREND: Move the document cursor the end of the current line.
;   (TODO: This is the same sequence that appears at CSRDN1, could
;       we just jump there instead?)
;
CSREND  LDX     CSRDLN
        JSR     LSTLPT      ;  X to one past end of the line.
        LDA B   #SCRN_W     ;  B to one past end of the line.
        JMP     CSRLF_      ;  Left scan!


;------------------------------------------------------------------------------
;
; Editing primitives
;
;------------------------------------------------------------------------------

; WRCHR: Sets the character (in A) at the current cursor location within the document. 
;
WRCHR   LDX     CSRDLN
        LDA B   CSRDIX
        ADD B   #4              ; Add 4 to the pointer offset skip the prev/next ptrs
        STA B   WRCHR1+1        ; [SMC] Patch the next instruction
WRCHR1  STA A   0, X            ; [SMC] Index byte updated to point to offset
        RTS

; RDCHR: Gets the character at the current cursor location within the document. (Returns in B)
;
RDCHR   LDX     CSRDLN
        LDA B   CSRDIX
        ADD B   #4              ; Add 4 to the pointer offset skip the prev/next ptrs
        STA B   RDCHR1+1        ; [SMC] Patch the next instruction
RDCHR1  LDA B   0, X            ; [SMC] Index byte updated to point to offset
        RTS

; INSHFT: Insert-shift is a speciality surgical edit that inserts the character in A,
; and takes whatever was at this location before and pushes it forward one. This is
; used for fast-path normal typeahead, and the caller must guarantee that it's 
; appropriate.
; 
INSHFT  BSR     RDCHR       ; Get current character into B.
        PSH B
        BSR     WRCHR       ; Write current char from A.
        INC     CSRDIX      ; Increment the cursor position
        PUL A               ; Pop the original character into A
        BRA     WRCHR       ; Tail call to write the orignal character following it.

; INSL: Insert a line into the document, immediately *preceding* the line pointed to by X.
;  This is unconventional for doubly linked lists (usually you insert after) but it maps
;  to the standard usage for document editing.
; This routine allocates a new line object, fixes up the pointers in the prev and next nodes,
; sets the pointers in the new node, blanks its data area, and returns its handle.
;
INSL    CPX     #$0         ; Are we inserting a line at the end of the list? 
        BNE     INSL1
        STX     LLTNXT      ; If we're inserting at the end of the list, then next = 0
        LDX     DOCTAL      ; And the tail node of the document is the prev
        BRA     INSL2
INSL1   STX     LLTNXT      ; and that's the "next" node
        LDX     2, X        ; indirect through the prev ptr to find the prev node.
INSL2   STX     LLTPRV      ; save the prev ptr.
        JSR     ALLOCL      ; allocate the new line.
        BEQ     INSLER      ; If we got an OOM error, nothing to do here.
        STX     LLTNEW      ; Save the new ptr
        STS     TEMPST      ; Save the stack pointer so we can use it.
        TXS                 ; S = new node pointer
        INS
        LDX     LLTPRV
        BNE     INSL3
        STS     DOCHED      ; if prev ptr was NULL, we are inserting at the top of the list, so save the new head
        BRA     INSL4
INSL3   STS     0, X        ; prev->next = new
INSL4   TXS                 ; S = prev
        INS
        LDX     LLTNEW
        STS     2, X        ; new-prev = prev
        LDS     LLTNXT
        STS     0, X        ; new->next = next
        BNE     INSL5      ; is the next ptr NULL?
        STX     DOCTAL      ; tail ptr = new
        BRA     INSL6
INSL5   TSX                 ; X = next
        DEX
        LDS     LLTNEW      ; S = new
        STS     2, X        ; next->prev = new
        TSX                 ; X = new
        DEX
INSL6   LDS     TEMPST      ; load back the real stack ptr
        JSR     LNTEXT      ; X still has the new node. Get the data pointer.
        JSR     BLNKLN      ; Tail call to blank the data. The blank routine preserves X which is what our caller should see.
        LDX     LLTNEW      ; Reload the actual node ptr to the new node.
INSLER  RTS
                
; DELL: Delete a line in the document, pointed to by the handle in X. 
; This routine returns pointer to the "replacement" line, in X. This is usually a different
; address, but in some cases could be the same address.
; This routine does not alter the screen line list.
;  
DELL    STX     TEMPX       ; Find the lines before and after the one to be deleted.
        LDX     0, X        
        STX     LLTNXT
        LDX     TEMPX
        LDX     2, X
        STX     LLTPRV
        CPX     LLTNXT      ; Are the next and prev ptrs the same? The only time that happens is when 
                            ; they're both NULL, meaning this is the first and only line.
        BNE     DELL1       ; If they're different, this is a normal line, skip the next bit.
        LDX     TEMPX       ; This is the only line! Get back the pointer to this node.
        JSR     EMTLIN      ; Keep it in place, just wipe the contents-- we don't allow there to be "no document"
        LDX     DOCHED      ; DOCHED better still point to the front of this same node (can't use TEMPX)
        RTS
DELL1   LDX     TEMPX       ; Get back the pointer to this node
        JSR     FREEL       ; Put it back on the free list.
        STS     TEMPST      ; Save the stack pointer so we can use it.
        LDS     LLTNXT      ; S = the next pointer
        LDX     LLTPRV
        BNE     DELL2       
        STS     DOCHED      ; If the prev ptr is NULL, we deleted the first node. Store the next in DOCHED.
        BRA     DELL3
DELL2   STS     0, X        ; Save the next pointer in the first pointer of the prev node
DELL3   TXS                 ; S = the prev pointer
        INS
        LDX     LLTNXT   
        BNE     DELL4
        STS     DOCTAL      ; If the next pointer was NULL, we deleted the last node. Store the prev in DOCTAL.
        TSX                 ; Since the next ptr was null, we're going to return the new final line.
        DEX
        BRA     DELL5
DELL4   STS     2, X        ; Save the prev pointer in the second pointer on the next node. X still = the next node, which we will retyurn
DELL5   LDS     TEMPST      ; Load back the real stack
        RTS                  

; MRKCHR: Mark the a document location by setting its high bit.
;
MRKCUR  LDA A   0, X        
        ORA A   #$80
        STA A   0, X
        RTS

; RECCUR: Recover cursor. Finds the high-flagged character onscreen and makes that into the cursor.```
; This is heinous and inefficient since it scans all of video space. It also has the unfortunate effect of 
; resulting in the flagged-character looking like a solid block for as many cycles as it takes to get to this
; routine and handle it. !!
;
RECCUR  LDX     R1STDS      ; Start looking with the first line we copied out to.
RCLP    STX     TEMPX       ; Remember the start of this line.
        JSR     LNTEXT      ; Move X to the start of text.
        CLR B               ; Reset character index.
RCRLN1  LDA A   0, X
        BMI     RCFIX       ; Found a character with high bit set
        INX
        INC B
        CMP B   #SCRN_W     ; did we reach the end of the line?
        BNE     RCRLN1
        LDX     TEMPX
        LDX     0, X        ; not found in this line. Load up the next one.
        BEQ     RCUNK       ; If we ran off the end of the document, just fail 
        BRA     RCLP        ; Go all the way back to restart the search with next line.
RCUNK   LDX     SCRDOC      ; In an unexpected failure case, put the cursor home.      
        CLR B
        BRA     RCFIX2      ; Go save the results.
RCFIX   AND A   #$7F        ; Clear the high bit on the actual cursor location.
        STA A   0, X
        LDX     TEMPX       ; Load back the start of this line.
RCFIX2  STX     CSRDLN      ; And update the document cursor to match.
        STA B   CSRDIX
        RTS

;------------------------------------------------------------------------------
;
; Non-edit UI routines
;
;------------------------------------------------------------------------------

; SPLASH: Puts up the program splash dialog
;
SPLASH  BSR     MSGBOX
        LDX     #$E082
        STX     CURSOR
        LDX     #STITLE
        BSR     PUTMSG
        LDX     #$E0C2
        STX     CURSOR
        LDX     #SAUTHR
        BSR     PUTMSG
        JSR     GETCHR
        JMP     MRKALL      ; Tail call to mark screen

; CONFRM: Runs a "confirmation" message box: prints a Y/N question pointed
; to by string in X, then waits for user to respond. This routine sets the Z
; flag when the user has said OK, clears it when cancelled. This routine
; will also mark the screen re-render state so the box clears on the next render
; pass.
;
CONFRM  STX     TEMPX2
        BSR     MSGBOX
        LDX     #$E082
        STX     CURSOR
        LDX     TEMPX2
        BSR     PUTMSG
        LDX     #$E0C2
        STX     CURSOR
        LDX     #SYESNO
        BSR     PUTMSG
        JSR     GETCHR
        JSR     MRKALL      ; Before we test the results, mark the screen for re-render.
        CMP A   #'Y         ; Did the user confirm? Anything but "Y" is no/cancel/esc,
        RTS        
      
; DOMSGB: Does a text message box. Pointer to text in X. This routine draws and 
; then marks the screen for re-render. The caller is not expected to return to 
; edit loop until it's time for the box to go away.
;  
DOMSGB  STX     TEMPX2
        BSR     MSGBOX
        LDX     #$E0A2
        STX     CURSOR
        LDX     TEMPX2
        BSR     PUTMSG
        JMP     MRKALL      ; Tail call to set dirty flag on screen.

; PUTMSG: Writes a nul-terminated string to the screen at current screen cursor.
; String pointer in X.
;
PUTMSG  LDA A   0, X
        BEQ     PMSGDN
        INX
        STX     TEMPX
        LDX     CURSOR
        STA A   0, X
        INX
        STX     CURSOR
        LDX     TEMPX
        BRA     PUTMSG
PMSGDN  RTS        

; MSGBOX: Overdraws an empty dialog box window in the middle of the screen. 
;
MSGBOX  LDX     #$E060
        LDA A   #'+
        LDA B   #'-
        BSR     DRWBLN
        LDA A   #'I
        LDA B   #' 
        BSR     DRWBLN
        BSR     DRWBLN
        BSR     DRWBLN
        LDA A   #'+
        LDA B   #'-
        BSR     DRWBLN
        RTS

; DRWBLN: Helper for drawing box lines, used above. 
;
DRWBLN  PSH A
        PSH B
        STA A   0, X
        STA A   31, X
        LDA A   #30
DBLN1   INX
        STA B   0, X
        DEC A
        BNE     DBLN1
        INX                 ; Incremement to start of next line for convenience
        INX
        PUL B
        PUL A
        RTS

;------------------------------------------------------------------------------
;
; Cassette I/O.
;
;------------------------------------------------------------------------------

; WRTDOC: Write out the current document to cassette. Because the data does not
; lie in a continuous buffer like the normal cassette firmware works with, this is
; a semi-deconstruction of the firmware routine. We still call CASOUT as the core
; write routine, but write the header and trailer "manually" and track the checksum. 
; These bits of code are basically cribbed from SYS2NF source.
;
;
WRTDOC  LDX     #SSAVE
        JSR     CONFRM
        BNE     WRTDDN
        LDX     #SSAVNG
        JSR     DOMSGB      ; Put up the message box saying that we are saving.
        JSR     DOCBYT      ; Calculate document size
        DEX                 ; Subtract one to meet the tape spec.
        STX     DOCSZE
WRTBLK  CLR     NOPRNT      ; Suppress status showing onscreen.
        JSR     TURNON      ; Turn on cassette.
        LDA B   #9          ; LOADS TIME LOOP COUNTER.
CTIME1  LDX     #20833      ; MASTER TIME LOOP (1/4 SEC).
CTIME2  DEX                 ; COUNTS CYCLES OF LOOP.
        BNE     CTIME2      ; TESTS FOR FIRST TIME OUT.
        DEC B               ; COUNTS TIMES IN LOOP.
        BNE     CTIME1      ; SKIPS BACK UNTIL DONE. 
        LDA A   #$16        ; PUTS SYNC CHARS ONTO TAPE. (B is now zero for chksum-- below this point nothing can mess with B-- stupid)
        JSR     CASOUT      ; .
        JSR     CASOUT      ; .
        JSR     CASOUT      ; .
        LDA A   #$1B        ; .
        JSR     CASOUT      ; .
        LDA A   DOCSZE
        JSR     CASOUT
        LDA A   DOCSZE+1
        JSR     CASOUT
        LDA A   #'S     
        JSR     CASOUT      ; .
        LDA A   #'0
        JSR     CASOUT      ; PUTS OUT LAST OF NAME.        
        BSR     WRTDAT      ; Call the routine that emits the actual document data.
        LDA A   #$17        ; OUTPUTS END-OF-BLOCK CHAR.
        JSR     CASOUT      ; ETB IS DISPLAYED AS A "W".
        TBA                 ; A GETS CHECKSUM FROM B.
        JSR     CASOUT      ; OUTPUTS THE CHECKSUM.
        JSR     CASOUT      ; OUTPUTS THE TRAILER FILLER
        JSR     CASOUT      ; BYTES.
        JSR     CASOUT      ; .
        JSR     TRNOFF      ; HALTS CASSETTE DRIVE.
WRTDDN  RTS                 ; -

; WRTDAT: Helper subroutine to write the document's data. This routine
; steps through each doument line until it runs out.
; 
WRTDAT  LDX     DOCHED      ; Load up the first line of the document
WRDAT1  BEQ     WRDTDN      ; is the current line ptr NULL?
        STX     WRTMP1
        BSR     WRTLIN      ; Call the line writer helper.
        LDX     WRTMP1
        LDX     0, X        ; line = line->next
        BRA     WRDAT1
WRDTDN  RTS     

; WRTLIN: Helper routine to emit a single line of text via the cassette
; output. Input is X, pointing to a line node.
;
WRTLIN  JSR     LNTEXT      ; Get the contents start
        LDA A   #SCRN_W     ; Set write count to 32
        STA A   WRCNT
WRLNLP  LDA A   0, X        
        STX     WRTMP2
        JSR     CASOUT
        ABA                 ; Update checksum A = B + A
        TAB                 ;                 B = A
        LDX     WRTMP2
        INX
        DEC     WRCNT
        BNE     WRLNLP
        RTS

; RDDOC: Clear the current document and read in doc from cassette.
; 
;
RDDOC   LDX     #SLOAD
        JSR     CONFRM
        BNE     RDOCDN
        JSR     RSTDOC      ; Reset the document and memory allocator.
        JSR     HOMCLR      ; Cursor to top left, clear display
        LDA A   #'S         ; Set blockname to read ("S0")
        STA A   BLKNAM
        LDA A   #'0
        STA A   BLKNAM+1
        STA A   NOPRNT      ; Save this non-zero value to NOPRNT so user sees load happening
        LDX     #1
        STX     BFRPTR      ; Set "1" as the buffer ptr, means we'll be able to use BFRSZE as byte 
                            ; count after the header is read (we don't actually read to BFRPTR, we just
                            ; take advantange of RDHDR doing the math for us so it's safe)
        JSR     TURNON      ; TURNS ON TAPE DRIVE.
        JSR     RDHDR       ; READS IN THE HEADER.
        CLR B               ; INIT B FOR CHECKSUM.
        LDX     DOCHED
RBFR1   STX     LLTNEW      ; .
        CLR A               ; Clear count for this line
        JSR     LNTEXT      ; Move pointer to the content area
RBFR2   PSH A
        STX     TEMPX2
        JSR     CASIN       ; A GETS CHAR READ IN.
        LDX     TEMPX2      ; X GETS BUFFER PTR.
        STAA    0,X         ; STORS CHAR INTO BUFFER.
        ABA                 ; A GETS A+B.
        TAB                 ; B GETS A.
        INX                 ; INC TO NEXT CHAR POSITIN.
        STX     LLTNEW
        PUL A
        INC A
        CMP A   #SCRN_W
        BNE     RBFR2       ; GOES BACK IF ANY LEFT.
        ; Prep for the next line. 
        PSH B
        LDA A   BFRSZE      ; Decrement bytes remaining by 32.
        LDA B   BFRSZE+1
        SUB B   #SCRN_W
        SBC A   #0
        STA A   BFRSZE
        STA B   BFRSZE+1
        PUL B
        LDX     BFRSZE
        BEQ     RDTRLR      ; if we read all the lines, then move on to the trailer
        LDX     #0          
        JSR     INSL        ; Insert a new line at the tail of the document.
        BRA     RBFR1
RDTRLR  JSR     RDTLR
        JSR     CLEAR       ; Get rid of loading indications onscreen
        TST     CSTATS      ; How did it go?
        BNE     RDERR
READOK  LDX     DOCHED      ; Get ready 
        STX     SCRDOC      ; Cursor to start of document.
        STX     CSRDLN
        CLR     CSRDIX
        JSR     MRKALL      ; Flag the whole screen for re-render
RDOCDN  RTS
RDERR   JMP     ENTRY       ; Wipe out all the state on any error.

; DOCLNC: Compute count of lines in the active document (returned in X)
;
DOCLNC  LDX     DOCHED
        CLR A
        CLR B
DOCLC1  JSR     INCAB
        LDX     0, X        ; line = line->next
        BNE     DOCLC1
        JMP     TABX        ; [TC] X = AB
        
; DOCBYT: Count of bytes in the active document. 
;
DOCBYT  BSR     DOCLNC      ; Count the lines in the document
        JSR     TXAB
        ASL B               ; Multiply line count by line length (32)
        ROL A
        ASL B
        ROL A
        ASL B
        ROL A
        ASL B
        ROL A
        ASL B
        ROL A
        JSR     TABX
        RTS

;------------------------------------------------------------------------------
;
; Semantically specific utility routines
;
;------------------------------------------------------------------------------

; BLNKLN: Inserts <line length> blanks at the X address. Preserves A, B, X.
;
BLNKLN  STX     TEMPX
        PSH A
        PSH B
        LDA A   #BLNKCH
        LDA B   #SCRN_W
BLNLP   STA A   0, X
        INX
        DEC B
        BNE     BLNLP
        PUL B
        PUL A
        LDX     TEMPX
        RTS

; ADDLN: Adds <line length> to the value in X. Preserves A and B.
;
ADDLN   STX     TEMPX
        PSH A
        PSH B
        LDA B   TEMPX+1
        LDA A   TEMPX
        ADD B   #SCRN_W
        ADC A   #0
        STA A   TEMPX
        STA B   TEMPX+1
        PUL B
        PUL A
        LDX     TEMPX
        RTS        


;------------------------------------------------------------------------------
;
; General utility routines
;
;------------------------------------------------------------------------------

; FNDEOM: Find end of memory. Checks at 4K increments (Sphere memory is always in banks of 4k).
; A more aggressive memory walk would halt at the first failing bit/byte, which would result in
; smaller but more reliable memory space. But we are not doing that extra work here, because we
; are assuming either emulation or reliable hardware.
; 
FNDEOM  LDX     #$1000      ; All Spheres have at least 4K. Start checking at $1000.
        STX     TEMPX       ; Use temp for incrememting the address
        LDA A   #$AA        ; We will always try this bit pattern
        LDA B   #$55        ; B will change as we use it but that's OK.
FEOM0   STA A   0, X        ; Store and compare for $AA
        CMP A   0, X
        BNE     EOMFND
        STA B   0, X        ; Store and compare for whatever is in B.
        CMP B   0, X
        BNE     EOMFND
        LDA B   #$10        ; Address += $1000 (4k)
        ADD B   TEMPX
        STA B   TEMPX
        LDX     TEMPX
        CPX     #$E000      ; If we've run all the way into the device addresses, we're done.
        BNE     FEOM0
EOMFND  DEX                 ; End of memory should be one byte prior to the failed address.
        RTS
        
; COPY2: Copies a sequence of bytes from CPYSRC to CPYDST. CPYEND indicates one past the last byte to copy.
;   (If CPYSRC == CPYEND it's a zero-byte no-op).
;   When this routine has completed, B will contain the number of actual bytes copied and X will be == CPYEND
;
COPY2   CLR B
COPY21  LDX     CPYSRC      ; Load the next source addr
        CPX     CPYEND      ; Is this the end address? If so, we are already done!
        BEQ     CPY2DN
        LDA A   0, X
        INX
        STX     CPYSRC
        LDX     CPYDST      
        STA A   0, X
        INX
        INC B
        STX     CPYDST
        BRA     COPY21
CPY2DN  RTS

; CMP16: Unsigned compare of X to CMPX. This substracts CMPX from X. The carry flag
; will be set if the subtraction underflowed, not otherwise. The Z and O flags will not
; be meaningful. To test for 16 bit *equality*, use CPX.
;
CMP16   STX     TEMPX
        PSH A
        PSH B
        LDA A   TEMPX
        LDA B   TEMPX+1
        SUB B   CMPX+1
        SBC A   CMPX   
        TPA                 ; Preserve condition 
        LDX     CMPX        ; Load CMPX into X as a convenience for the caller.
        TAP                 ; Restore condition.
        PUL B
        PUL A
        RTS     
                
; Decrements X by the value in B. Not performant for large subtractions!
;
DEC16B  TST B
        BEQ     D16DN       ; Check B==0, no-op if so.
        PSH B               ; Save the original value in case caller needs it
D16LP   DEX
        DEC B
        BNE     D16LP
        PUL B               ; Restore B
D16DN   RTS        


; TABX: Transfer AB->X
;
TABX    STA A   TEMPX       ; Transfer the value to the X reg
        STA B   TEMPX+1
        LDX     TEMPX
        RTS

; TXAB: Transfer X->AB
;
TXAB    STX     TEMPX
        LDA A   TEMPX
        LDA B   TEMPX+1
        RTS

; INCAB: Increment AB as a 16-bit value
;
INCAB   INC B
        BNE IAB2
        INC A
IAB2    RTS

; HOMCLR: PDS Home then clear
;
HOMCLR  JSR     HOME
        JMP     CLEAR
        
;
; Command jump table. Processed in order. 
;

;               (lt)    Ctrl-A  (rt)    Ctrl-D  (up)    Ctrl-W  Ctrl-S (CR)   Ctrl-Y  (bksp)  (space) Ctrl-V  Ctrl-B Ctrl-P  Ctrl-L Ctrl-K  Ctrl-O              
CMDCHR  FCB     $14,    $01,    $12,    $04,    $11,    $17,    $13,   $0D,   $19,    $08,    $20,    $16,    $02,   $10,    $0C,    $0B,   $0F   
CMDJMP  FDB     CSRLFT, CSRLFT, CSRRT,  CSRRT,  CSRUP,  CSRUP,  CSRDN, INSCR, YANKLN, GOBKSP, INSDEL, WRTDOC, RDDOC, PAGEUP, PAGEDN, CSRHOM,CSREND

;
; Reflow buffer lives here.
; 

RFLBUF  RMB     65          ; two full lines of reflow buffer 
RFLEND  *                   ; this is the end of the reflow buffer 

;
; String table
;

STITLE  DS      "      S C R I P T O R"
        FCB     0
SAUTHR  DS      "     BY BEN ZOTTO 2023 "
        FCB     0
SSAVE   DS      "SAVE DOCUMENT TO TAPE?"
        FCB     0
SSAVNG  DS      "SAVING TO TAPE..."
        FCB     0
SLOAD   DS      "LOAD NEW DOCUMENT FROM TAPE?"
        FCB     0
SYESNO  DS      "CONFIRM (Y) OR CANCEL (ANY)"
        FCB     0    

END
    