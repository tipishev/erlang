#include <unistd.h>
typedef unsigned char byte;

// exported
int read_cmd(byte *buf);
int write_cmd(byte *buf, int len);

// internal
int read_exact(byte *buf, int len);
int write_exact(byte *buf, int len);

int read_cmd(byte *buf)
{
    int len;
    if (read_exact(buf, 2) != 2)
        return(-1);  // oh-oh, fragmentation
    len = (buf[0] << 8) | buf[1];  // 2-byte length
    return read_exact(buf, len);
}

int write_cmd(byte *buf, int len)
{
    byte li;

    // first header byte
    li = (len >> 8) & 0xff;
    write_exact(&li, 1);

    // second header byte
    li = len & 0xff;
    write_exact(&li, 1);

    // write payload
    return write_exact(buf, len);
}

int read_exact(byte *buf, int len)
{
    int i, got=0;
    do {
        if ((i = read(0, buf+got, len-got)) <= 0)
            return(i);
        got += i;
    } while (got<len);
    return(len);
}

int write_exact(byte *buf, int len)
{
    int i, wrote = 0;
    do {
        if ((i = write(1, buf+wrote, len-wrote)) <= 0)
            return (i);
        wrote += 1;
    } while (wrote<len);
    return (len);
}
