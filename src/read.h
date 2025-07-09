#ifndef READ_H
#define READ_H

typedef struct struct_RCHAN {
    void *state;

    int (*readch)(struct struct_RCHAN *);

    void (*unreadch)(struct struct_RCHAN *, int);
} RCHAN;

int internal_read_char(RCHAN *rchan);

void internal_unread_char(RCHAN *rchan, int ch);

CELL internal_read_line(FILE *fp);

int internal_peek_char(RCHAN *rchan);

extern void internal_set_read_prompt(CELL prompt);

extern CELL internal_read_with_prompt(CELL prompt);

extern CELL internal_read(RCHAN *rchan, CELL token);

extern CELL internal_read_ident(RCHAN *rchan, const char *prefix, size_t prefix_len);

extern CELL internal_read_token(RCHAN *rchan);

extern CELL internal_read_string(RCHAN *rchan);

extern CELL internal_read_number(RCHAN *rchan, const char *prefix, size_t prefix_len);

extern CELL internal_read_vector(RCHAN *rchan);

extern void read_register_symbols();

#endif // READ_H
