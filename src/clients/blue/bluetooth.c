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
		print_info("Open of Bletooth device failed.\n");
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


ssize_t blue_put(int dev, char *c)
{
	ssize_t ret, sum = 0;
	debug("blue_put: %s\n", c);
	if ((ret = write(dev, c, strlen(c))) < 0) return ret;
	sum += ret;
	if ((ret = write(dev, "\r", 1)) < 0) return ret;
	sum += ret;
	tcdrain(dev);
	return sum;
}


ssize_t blue_get(int dev, char *a)
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
				debug("reading timed out five times. device gone?\n");
				return -1;
			}
			continue;
		}
		if (tmp[0] == '\r') continue;
		if (timeout) timeout = 0;
		if (tmp[0] == '\n') {
			if (!cs) continue;
			a[cs] = '\0';
			debug("blue_get: %s\n", a);
			return cs;
		}
		a[cs++] = tmp[0];
	}
}


int blue_get_block(int dev, char *buf)
{
	fd_set rfds;
	
	for (;;) {
		FD_ZERO(&rfds);
		FD_SET(dev, &rfds);

		select(dev + 1, &rfds, NULL, NULL, NULL);
		
		if (FD_ISSET(dev, &rfds)) {
			return blue_get(dev, buf);
		} else {
			debug("timeout\n");
		}
	}
}


int blue_put_expect(int dev, char* c, char *expect)
{
	static char buf[256];
	
	if (blue_put(dev, c) < 0) return -2;
	if (blue_get(dev, buf) < 0) return -2;
	return strcasecmp(buf, expect) ? -1 : 0;
}


int blue_put_echo_expect(int dev, char* c, char *expect)
{
	static char buf[256];
	
	if (blue_put(dev, c) < 0) return -2;
	if (blue_get(dev, buf) < 0) return -2;
	if (strcasecmp(buf, c)) return -1;
	if (blue_get(dev, buf) < 0) return -2;
	return strcasecmp(buf, expect) ? -1 : 0;
}


int blue_init(int dev)
{
	int ret; 
	
	/* init */
	if ((ret = blue_put_echo_expect(dev, "ATZ", "OK")) < 0)
		return ret;
	/* echo off */
	if ((ret = blue_put_echo_expect(dev, "ATE=0", "OK")) < 0)
		return ret;
	/* Accessory name */
	if ((ret = blue_put_expect(dev, "AT*EAM=\"xmms2\"", "OK")) < 0)
		return ret;
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
		if (blue_get_block(dev, rbuf) < 0) return -2;
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
			debug("unmatched sequence \"%s\"\n", rbuf);
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
	
	if (blue_get_block(dev, rbuf) < 0) return -2;
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
		debug("unmatched sequence \"%s\"\n", rbuf);
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
		if (blue_get_block(dev, rbuf) < 0) return -2;
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
			debug("unmatched sequence \"%s\"\n", rbuf);
			return -1;
		}
	}
	
	return 0;
}



