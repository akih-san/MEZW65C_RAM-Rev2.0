/*
  Very Tiny Language

  T. Nakagawa

  2004/05/23
  2004/06/26
*/

#define MSIZE 32768		/*ターゲットに配置し確定後に、再設定する*/

#define MEMSIZE (MSIZE)
#define READB(adrs) (*(Lct + (adrs)))
#define WRITEB(adrs, data) (*(Lct + (adrs)) = (data))

/* original
#define READW(adrs) (*(Lct + (adrs)) * 256 + *(Lct + ((adrs) + 1)))
#define WRITEW(adrs, data) (*(Lct + (adrs)) = (data) / 256, *(Lct + ((adrs) + 1)) = (data) % 256)
*/
/* オリジナルは、16ビットがバイト単位で80系のエンディアンと逆になっているので           */
/* コード効率と、処理の効率が悪い。そのため、80系のエンディアンに合わせるように変更した */
#define READW(adrs) (*(unsigned short *)(Lct + (adrs)))
#define WRITEW(adrs, data) (*(unsigned short *)(Lct + (adrs)) = (data))

#define Nbf 0x82				/* 130 */
#define Lbf 0x88				/* 136 */
#define Svp ((2 + '!') * 2)		/* 70 */
#define Pcc ((2 + '#') * 2)		/* 74 */
#define Rmd ((2 + '%') * 2)		/* 78 */
#define Bnd ((2 + '&') * 2)		/* 80 */
#define Rnd ((2 + '\'') * 2)	/* 82 */
#define Lmt ((2 + '*') * 2)		/* 88 */
#define Obj 0x108				/* 264 */

unsigned char Lct[MEMSIZE];

extern void initl(void);
extern unsigned char c_getch(void);
extern unsigned char getchr(void);
extern void putchr(unsigned char);
extern unsigned char c_kbhit();
extern void warm_boot();
extern void mach_fin();
extern void srand(int);
extern int rand();

static int fndln(unsigned short *ptr);
static unsigned short nxtln(unsigned short ptr);
static void getln(unsigned short lbf);
static int getnm(unsigned short *ptr, unsigned short *n);
static int num(unsigned short ptr);
static void ordr(unsigned short ptr);
static void expr(unsigned short *ptr, unsigned short *val);
static void factr(unsigned short *ptr, unsigned short *val);
static void term(unsigned short *ptr, unsigned short *val);
static void getvr(unsigned short *ptr, unsigned char *c, unsigned short *adr);
static void putl(unsigned short *ptr, unsigned char d);
static void crlf(void);
static void putnm(unsigned short x);
static void putstr(char *str);

void breakCheck()
{
	char c;

	if (c_kbhit()) {/* check keyin */
		c = c_getch();
		if(c == 0x03) warm_boot();			/* force warm boot */
	}
}

int main(int key) {
	 unsigned short ptr;
	 unsigned short n;

	if (!key) {
		/* initialize '*' and '&' */
		WRITEW(Lmt, MSIZE);	/* RAM End */
		WRITEW(Bnd, Obj);		/* Program End */

		srand(65816); /* for RND function */
		putstr("VTL-C MEZW65C-RAM edition.");
	}
	for (; ; ) {
		putstr("\r\nOK");
nmsg:
		ptr = Lbf + 2;				/* ptr = 138 */
		getln(ptr);					/* buffer address = Lct+138 */
		if (!getnm(&ptr, &n)) {
			unsigned short line;

			line = Lbf;
			WRITEW(line, 0);	/* line = 0 */
			WRITEW(Pcc, 0);		/* Pcc = 0 */
			for (; ; ) {
				breakCheck();
				ordr(ptr);
				if (READW(Pcc) == 0 || READW(Pcc) == READW(line)) {
					/* no branch */
					if (line == Lbf) break;	/* ダイレクトモードの場合 */
					line = nxtln(line);
					if (line == READW(Bnd)) break;	/* 最終行まで実行した場合 */
				} else {
					/* branch */
					WRITEW(Svp, READW(line) + 1);
					if (fndln(&line)) break;
				}
				WRITEW(Pcc, READW(line));
				ptr = line + 2 + 1;
			}
		} else {
			if (n == 0) {
				/* LIST */
				for (ptr = Obj; ptr != READW(Bnd); ) {
					breakCheck();
					putnm(READW(ptr));
					ptr += 2;
					putl(&ptr, '\0');
					crlf();
				}
			} else {
				unsigned short cur, src, dst, tmp;
				unsigned short m;

				/* DELETE */
				WRITEW(Pcc, n);
				if (!fndln(&cur) && READW(cur) == n) {
					src = nxtln(cur);
					for (dst = cur; src != READW(Bnd); WRITEB(dst++, READB(src++))) ;
					WRITEW(Bnd, dst);
				}
				/* INSRT */
				if (READB(ptr) == '\0') goto nmsg;		/*continue;*/
				for (m = 3, tmp = ptr; READB(tmp) != '\0'; m++, tmp++) ;
				if (READW(Bnd) + m < READW(Lmt)) {
					src = READW(Bnd);
					WRITEW(Bnd, READW(Bnd) + m);
					for (dst = READW(Bnd); src != cur; WRITEB(--dst, READB(--src))) ;
					WRITEW(src, n);
					src += 2;
					while (WRITEB(src++, READB(ptr++)) != '\0') ;
					goto nmsg;	/* continue; */
				}
			}
		}
	}

/*	return 0; */
}


/*
  行番号が*Pcc以上の最小の行を探して，
  見つかればその先頭番地を*ptrに入れて0を返し，見つからなければ1を返す．
*/
static int fndln(unsigned short *ptr) {
  for (*ptr = Obj; *ptr != READW(Bnd); *ptr = nxtln(*ptr)) {
    if (READW(*ptr) >= READW(Pcc)) return 0;
  }
  return 1;
}


/*
  *ptrの次の行の先頭を見つけてその位置を返す
*/
static unsigned short nxtln(unsigned short ptr) {
  for (ptr += 2; READB(ptr++) != '\0'; ) ;
  return ptr;
}


/*
  一行読み込んでlbfから始まるアドレスに格納する
*/
static void getln(unsigned short lbf) {
  int p;
  unsigned char c;

	for (p = 0; ; ) {
		c = getchr();
		if (c == '\b') {
		      if (p > 0) p--;
		} else if (c == '\r') {
			WRITEB(lbf + p, '\0');
			putchr('\n');
			return;
		} else if (c == 0x15 || p + 1 == 74) {
			crlf();
			p = 0;
		} else if (c <= 0x1f) {
		} else {
	      WRITEB(lbf + p++, c);
    	}
  	}
}


/*
  **ptrが数字でなければ0を返す
  数字の場合は，その数字をバイナリに変換して*nに入れ，
  *ptrを数字の次のアドレスに進ませて，1を返す
*/
static int getnm(unsigned short *ptr, unsigned short *n) {
  if (!num(*ptr)) return 0;
  *n = 0;
  do {
    *n *= 10;
    *n += READB((*ptr)++) - '0';
  } while (num(*ptr));
  return 1;
}


/*
  *ptrが数字なら1，それ以外なら0を返す
*/
static int num(unsigned short ptr) {
  return ('0' <= READB(ptr) && READB(ptr) <= '9');
}


/*
  ptrから始まる文を実行する
*/
static void ordr(unsigned short ptr) {
  unsigned char c;
  unsigned short adr;

  getvr(&ptr, &c, &adr);	/* 左辺値のアドレスを求める */
  ptr++;	/* 代入の'='を読み飛ばす */

  if (READB(ptr) == '"') {
    ptr++;
    putl(&ptr, '"');
    if (READB(ptr) != ';') crlf();
  } else {
    unsigned short val;

    expr(&ptr, &val);	/* 右辺値を計算する */

    if (c == '$') {
      putchr(val & 0xff);
    } else if ((c -= '?') == 0) {
      putnm(val);
    } else {
      unsigned short r;

      WRITEW(adr, val);

/*
      r = READW(Rnd);
      r += val;
      r = (r << 8) + (r >> 8);
*/
	  /* 乱数を取出す */
	  r = rand();

      WRITEW(Rnd, r);
    }
  }
  return;
}


/*
  *ptrで始まる値を計算し，*valに入れる
*/
static void expr(unsigned short *ptr, unsigned short *val) {
  unsigned char c;

  factr(ptr, val);
  while ((c = READB(*ptr)) != '\0' && c != ')') {
    term(ptr, val);
  }
  (*ptr)++;
  return;
}


/*
  *ptrから始まる値の要素を読み*valに入れる
*/
static void factr(unsigned short *ptr, unsigned short *val) {
  unsigned char c;

  if (READB(*ptr) == '\0') {
    *val = 0;
    return;
  }

  if (getnm(ptr, val)) return;

  c = READB((*ptr)++);
  if (c == '?') {
    unsigned short tmp;

    tmp = Lbf;
    getln(tmp);
    expr(&tmp, val);
  } else if (c == '$') {
    *val = getchr();
  } else if (c == '(') {
    expr(ptr, val);
  } else {
    unsigned short adr;

    (*ptr)--;
    getvr(ptr, &c, &adr);
    *val = READW(adr);	/* 変数か配列の値を得る */
  }
  return;
}


/*
  *ptrから始まる演算子と値の要素を読み演算結果を*valに入れる
*/
static void term(unsigned short *ptr, unsigned short *val) {
  unsigned char c;
  unsigned short val2;

  c = READB((*ptr)++);
  factr(ptr, &val2);
  if (c == '*') {
    *val *= val2;
  } else if (c == '+') {
    *val += val2;
  } else if (c == '-') {
    *val -= val2;
  } else if (c == '/') {
    WRITEW(Rmd, *val % val2);
    *val /= val2;
  } else if (c == '=') {
    *val = (*val == val2);
  } else if (c == '>') {
    *val = (*val >= val2);
  } else {
    *val = (*val < val2);
  }
  return;
}


/*
  *ptrから始まる変数・配列の文字を*cに入れてアドレスを*adr返す
*/
static void getvr(unsigned short *ptr, unsigned char *c, unsigned short *adr)
{
	unsigned short val;

	*c = READB((*ptr)++);
	if (*c == ':') {
		expr(ptr, &val);
		*adr = READW(Bnd) + val * 2;
	}
	else if ( *c == 0x7f ) mach_fin();	/* DEL key */
	else *adr = ((*c & 0x3f) + 2) * 2;
	return;
}


/*
  文字dで終る文字列ptrを表示する
*/
static void putl(unsigned short *ptr, unsigned char d) {
  while (READB(*ptr) != d) putchr(READB((*ptr)++));
  (*ptr)++;
  return;
}


/*
  改行する
*/
static void crlf(void) {
  putchr('\r');
  putchr('\n');
  return;
}


/*
  数値xを表示する
*/
static void putnm(unsigned short x) {
  unsigned short ptr;
  unsigned char y;

  ptr = Nbf + 5;
  WRITEB(ptr, '\0');
  do {
    y = x % 10;
    x /= 10;
    WRITEB(--ptr, y + '0');
  } while (x != 0);
  putl(&ptr, '\0');
  return;
}


/*
  文字列strを表示して改行する
*/
static void putstr(char *str) {
  while (*str != '\0') putchr(*(str++));
  crlf();
  return;
}
