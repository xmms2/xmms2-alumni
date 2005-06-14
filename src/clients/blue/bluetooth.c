/*
 *  Bluetooth Stubs.
 *  Copyright (C) 2005 Jens Taprogge
 *
 *  Based on Sony Ericcson's "AT Commands Online Reference - T68, T68i, T300,
 *  T310, T610, Z600, T230/T238/T226, T630, T290" 
 *  dated October 2004.
 *
 *  The documentation seems to apply to later models as well.
 */

#include "xmms2_blue.h"

#include <sys/select.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <termios.h>
#include <unistd.h>

int blue_open(char *devname) 
{
	char errstring[256];
	struct termios attr;
	int dev;

	dev = open(devname, O_RDWR | O_NOCTTY);  
	if (dev < 0) {
		print_error("Open of Bletooth device failed.\n");
		return -1;
	}

	if (!isatty(dev)) {
		close(dev);
		dev = -1;
		sprintf(errstring, "\"%s\" is not a tty.", devname);  
		print_error(errstring);
		return -1;
	}

	memset(&attr, 0, sizeof(attr));
	attr.c_cflag = B4800 | CS8 | CLOCAL | CREAD;
	attr.c_cc[VTIME] = 1;
	attr.c_cc[VMIN] = 0;
	if (tcsetattr(dev, TCSANOW, &attr) < 0) {
		perror("tcsetattr");
		close(dev);
		return -1;
	}

	return dev;
}


void blue_put(int dev, char *c)
{
	debug("blue_put: %s\n", c);
	write(dev, c, strlen(c));
	write(dev, "\r", 1);
	tcdrain(dev);
}


void blue_get(int dev, char *a)
{
	char tmp[1];
	unsigned int c, cs = 0;
	unsigned int timeout = 0;
	while(1) {
		c = read(dev, &tmp, 1);
		if (!c) {
			timeout++;
			if (timeout == 5) {
				a[0] = '\0';
				return;
			}
			continue;
		}
		if (tmp[0] == '\r') continue;
		if (timeout) timeout = 0;
		if (tmp[0] == '\n') {
			if (!cs) continue;
			a[cs] = '\0';
			debug("blue_get: %s\n", a);
			return;
		}
		a[cs++] = tmp[0];
	}
}


void blue_get_block(int dev, char *buf)
{
	fd_set rfds;
	
	for (;;) {
		FD_ZERO(&rfds);
		FD_SET(dev, &rfds);

		select(dev + 1, &rfds, NULL, NULL, NULL);
		
		if (FD_ISSET(dev, &rfds)) {
			blue_get(dev, buf);
			return;
		} else {
			debug("timeout\n");
		}
	}
}


int blue_put_expect(int dev, char* c, char *expect)
{
	static char buf[256];
	blue_put(dev, c);
	blue_get(dev, buf);
	return strcasecmp(buf, expect);
}


int blue_put_echo_expect(int dev, char* c, char *expect)
{
	static char buf[256];
	blue_put(dev, c);
	blue_get(dev, buf);
	if (strcasecmp(buf, c)) return -1;
	blue_get(dev, buf);
	return strcasecmp(buf, expect);
}


int blue_init(int dev)
{
	if (blue_put_echo_expect(dev, "ATZ", "OK") < 0)
		return -1;
	if (blue_put_echo_expect(dev, "ATE=0", "OK") < 0)
		return -1;
	if (blue_put_expect(dev, "AT*EAM=\"xmms2\"", "OK") < 0)
		return -1;
	return 0;
}    


int blue_menu(int dev, char *title, struct cmds *items, int init, void *user)
{
	int i;
	char buf[256], buf2[256], rbuf[256];
	int item = init;
	int ret;
	
	buf[0]='\0';
	for (i=0; items[i].name; i++) {
		sprintf(buf+strlen(buf), ",\"%s\"", items[i].name);
	}

	sprintf(buf2, "AT*EASM=\"%s\",1,%d,%d%s", title, item, i, buf);
	if (blue_put_expect(dev, buf2, "OK")) return -1;
	
	while (item) {
		blue_get_block(dev, rbuf);
		if (!strncasecmp("*EAMI: ", rbuf, 7)) {
			item = atoi(rbuf+7);
			if (!item) break;
			if ((item < 0) || (item > i)) {
				debug("Item %i out of range.\n", item);
				return -1;
			} else if ((ret = items[item-1].func(user, items[item-1].arg, dev))) {
				debug("\"%s\" failed", items[item].name);
				return ret;
			}
		} else {
			debug("unmatched sequence \"%s\"", rbuf);
		}
		/* display the menu again */
		sprintf(buf2, "AT*EASM=\"%s\",1,%i,%d%s", title, item, i, buf);
		if (blue_put_expect(dev, buf2, "OK")) return -1;
	}
	return 0;
}


int blue_select(int dev, char *title, char **items, int init)
{
	int i;
	char buf[256], buf2[256], rbuf[256];
	int item = init;
	
	buf[0]='\0';
	for (i=0; items[i]; i++) {
		sprintf(buf+strlen(buf), ",\"%s\"", items[i]);
	}

	sprintf(buf2, "AT*EAID=5,0,\"%s\",%d,%d%s", title, item, i, buf);
	if (blue_put_expect(dev, buf2, "OK")) return -1;
	
	blue_get_block(dev, rbuf);
	if (!strcasecmp("*EAII", rbuf)) {
		/* aborted */
		return -1;
	} else if (!strcasecmp("*EAII: 0", rbuf)) {
		/* rejected */
		return 0;
	} else if (!strncasecmp("*EAII: 5,", rbuf, 9)) {
		item = atoi(rbuf+9);
		if ((item < 0) || (item > i)) {
			debug("Item %i out of range.\n", item);
			return -1;
		}
	} else {
		debug("unmatched sequence \"%s\"", rbuf);
		return -1;
	}
	
	return item;
}


int blue_percent(int dev, char *title, int steps, int init, int (*updfunc)(int perc, void *user), void *user)
{
	char buf[256], rbuf[256];
	int perc;
	
	sprintf(buf, "AT*EAID=4,0,\"%s\",%d,%d", title, steps, init);
	if (blue_put_expect(dev, buf, "OK")) return -1;
	
		
	for (;;) {
		blue_get_block(dev, rbuf);
		if (!strcasecmp("*EAII", rbuf)) {
			/* aborted */
			return -1;
		} else if (!strcasecmp("*EAII: 0", rbuf)) {
			/* rejected */
			return 0;
		} else if (!strncasecmp("*EAII: 15,", rbuf, 10)) {
			if (!updfunc) continue;
			perc = atoi(rbuf+10);
			if ((perc < 0) || (perc > 100)) {
				debug("Percentage %i out of range.\n", perc);
				return -1;
			}
			updfunc(perc, user);
		} else if (!strncasecmp("*EAII: 4,", rbuf, 9)) {
			perc = atoi(rbuf+9);
			if ((perc < 0) || (perc > 100)) {
				debug("Percentage %i out of range.\n", perc);
				return -1;
			}
			return perc;
		} else {
			debug("unmatched sequence \"%s\"", rbuf);
			return -1;
		}
	}
	
	return 0;
}



