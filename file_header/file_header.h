//;--------- MEZW65C_RAM file header --------------------------
//
typedef struct {

	/*
	*	*** NOTE***
	*
	*	When bios_sw=1, the following parameters are not used and are reserved.
	*
	*		picif_p
	*		irq_sw
	*		reg_tblp
	*		reg_tsize
	*		nmi_sw
	*/
	uint8_t		op1;			// if bios_sw=1, PROGRAM BANK:(W65C816), 0:(W65C02)
								// if bios_sw=0, JMP opecode
	uint16_t	cstart_addr;
	uint8_t		op2;			// if bios_sw=1, DATA BANK:(W65C816), 0:(W65C02)
								// if bios_sw=0, JMP opecode
	uint16_t	wstart_addr;
	uint16_t	direct_page;	// DIRECT PAGE : (W65C816), reserve : (W65C02)
	uint8_t		mezID[8];		// Unique ID "MEZW65C",0 (This area is used by monitor for invoking bios_call program)
	uint32_t	load_p;			// Load program address 24bit:(W65C816), 16bit:(W65C02)
	uint32_t	picif_p;		// pic i/o shared memory address
	uint8_t		sw_816;			// 0 : W65C02, 1: W65C816 native mode, 2:works in both modes
	uint8_t		irq_sw;			// 0 : no use IRQ console I/O
								// 1 : use IRQ timer interrupt driven console I/O
	uint16_t	reg_tblp;		// register save pointer( NMI )
	uint16_t	reg_tsize;		// register table size
	uint8_t		nmi_sw;			// 0 : No NMI support, 1: NMI support
	uint8_t		bios_sw;		// 0 : standalone program
								// 1 : program uses bios call
								// 2 : monitor program( .SYS file)
} file_header;
//;--------- end MEZW65C_RAM file header --------------------------
