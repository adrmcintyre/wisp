typedef struct struct_RCHAN {
	void* state;
	int (*readch)(struct struct_RCHAN*);
	void (*unreadch)(struct struct_RCHAN*, int);
} RCHAN;
int internal_read_char(RCHAN* rchan);
void internal_unread_char(RCHAN* rchan, int ch);
int internal_peek_char(RCHAN* rchan);

extern CELL internal_read_with_prompt(char *prompt);
extern CELL internal_read(RCHAN* rchan, CELL token);
extern CELL internal_read_number(RCHAN* rchan, int base, int ch, TYPEID want_type);
extern void read_register_symbols();

